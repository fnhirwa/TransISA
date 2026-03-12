#!/usr/bin/env python3
"""
TransISA Benchmark Suite
Transpiles x86 assembly programs to ARM at multiple optimization levels,
compiles both, collects static metrics, and verifies correctness.

Usage:
    python3 analyzefiles.py [--transisa PATH] [--benchmarks DIR] [--output DIR]

Requirements (macOS):
    - TransISA binary (built from the project)
    - Xcode command-line tools (as, ld, otool, llvm-objdump)
"""

import argparse
import csv
import json
import os
import re
import subprocess
import sys
from pathlib import Path
from dataclasses import dataclass, field, asdict

SCRIPT_DIR = Path(__file__).parent.resolve()
OPT_LEVELS = [0, 1, 2]

# Expected exit codes for correctness verification.
# Programs not listed here skip the exit-code check.
EXPECTED_EXIT_CODES = {
    "hello": 0,
    "add": 0,
    "sum_loop": 36,
    "fibonacci": 55,
    "max_of_three": 42,
    "nested_loop": 20,
    "gcd": 6,
}


@dataclass
class Metrics:
    name: str
    arch: str
    opt_level: str  # "src" for x86 original, "O0"/"O1"/"O2" for ARM
    instruction_count: int = 0
    text_size_bytes: int = 0
    binary_size_bytes: int = 0
    syscall_count: int = 0
    stack_size_bytes: int = 0
    correct: str = "—"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def run(cmd, capture=True, timeout=10):
    """Run a shell command and return stdout, or empty string on failure."""
    try:
        r = subprocess.run(
            cmd, shell=True, capture_output=capture,
            text=True, timeout=timeout
        )
        return r.stdout if capture else ""
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
        return ""


def run_retcode(cmd, timeout=10):
    """Run a command and return its exit code, or -1 on failure."""
    try:
        r = subprocess.run(cmd, shell=True, timeout=timeout)
        return r.returncode
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
        return -1


# ---------------------------------------------------------------------------
# Metric collection
# ---------------------------------------------------------------------------

def count_instructions_objdump(binary_path):
    """Count instructions via llvm-objdump disassembly."""
    output = run(f"llvm-objdump --disassemble --no-show-raw-insn {binary_path}")
    count = 0
    for line in output.splitlines():
        if re.match(r"^\s*[0-9a-fA-F]+:\s+\S", line):
            count += 1
    return count


def count_instructions_asm(asm_path):
    """Fallback: count non-directive, non-label lines in an assembly file."""
    count = 0
    for line in Path(asm_path).read_text().splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#") or stripped.startswith(";"):
            continue
        if stripped.startswith("."):
            continue
        if stripped.endswith(":"):
            continue
        count += 1
    return count


def get_text_size(binary_path):
    """Get __text section size via size -m."""
    output = run(f"size -m {binary_path}")
    for line in output.splitlines():
        m = re.search(r"Section __text:\s+(\d+)", line)
        if m:
            return int(m.group(1))
    return 0


def get_binary_size(binary_path):
    """Get total file size in bytes."""
    p = Path(binary_path)
    return p.stat().st_size if p.exists() else 0


def count_syscalls(binary_path):
    """Count syscall/svc/int instructions in disassembly."""
    output = run(f"otool -tv {binary_path}")
    return sum(
        1 for line in output.splitlines()
        if any(kw in line for kw in ("svc", "int", "syscall"))
    )


def estimate_stack_size(asm_path):
    """Estimate stack frame size from sub sp/rsp instructions."""
    text = Path(asm_path).read_text()
    total = 0
    # ARM: sub sp, sp, #N
    for m in re.finditer(r"sub\s+sp,\s*sp,\s*#(0x[0-9a-fA-F]+|\d+)", text):
        total += int(m.group(1), 0)
    # ARM: sub sp, sp, #N, lsl #S
    for m in re.finditer(
        r"sub\s+sp,\s*sp,\s*#(0x[0-9a-fA-F]+|\d+),\s*lsl\s*#(\d+)", text
    ):
        total += int(m.group(1), 0) << int(m.group(2))
    # x86 (Intel form after lexer normalization): sub rsp, N
    for m in re.finditer(r"sub\s+rsp,\s*(\d+)", text):
        total += int(m.group(1))
    return total


def collect_metrics(name, arch, opt_level, binary_path, asm_path):
    """Collect all static metrics for a single binary."""
    m = Metrics(name=name, arch=arch, opt_level=opt_level)
    if Path(binary_path).exists():
        m.instruction_count = count_instructions_objdump(binary_path)
        m.text_size_bytes = get_text_size(binary_path)
        m.binary_size_bytes = get_binary_size(binary_path)
        m.syscall_count = count_syscalls(binary_path)
    if Path(asm_path).exists():
        m.stack_size_bytes = estimate_stack_size(asm_path)
        if m.instruction_count == 0:
            m.instruction_count = count_instructions_asm(asm_path)
    return m


# ---------------------------------------------------------------------------
# Build and run
# ---------------------------------------------------------------------------

def get_sdk_path():
    """Cache the macOS SDK path."""
    if not hasattr(get_sdk_path, "_cached"):
        get_sdk_path._cached = run("xcrun --show-sdk-path").strip()
    return get_sdk_path._cached


def build_x86(asm_path, out_dir):
    """Assemble and link an x86 program on macOS."""
    stem = Path(asm_path).stem
    obj = out_dir / f"{stem}_x86.o"
    binary = out_dir / f"{stem}_x86"
    sdk = get_sdk_path()

    run(f"as -arch x86_64 {asm_path} -o {obj}")
    run(
        f"ld -arch x86_64 -macosx_version_min 10.7 -lSystem "
        f"-o {binary} {obj} -syslibroot {sdk} -e _start"
    )
    return binary


def build_arm(asm_path, out_dir):
    """Assemble and link an ARM program on macOS."""
    stem = Path(asm_path).stem
    obj = out_dir / f"{stem}.o"
    binary = out_dir / stem
    sdk = get_sdk_path()

    run(f"as -arch arm64 {asm_path} -o {obj}")
    run(
        f"ld -macosx_version_min 11.0 -o {binary} {obj} "
        f"-lSystem -syslibroot {sdk} -e __start"
    )
    return binary


def transpile(transisa_bin, x86_asm, output_asm, opt_level):
    """Run TransISA to transpile x86 -> ARM at a given optimization level."""
    cmd = f"{transisa_bin} {x86_asm} -o {output_asm} --opt-level={opt_level}"
    r = subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=30)
    if r.returncode != 0:
        print(f"    TransISA error: {r.stderr.strip()}")
    return Path(output_asm).exists()


def check_correctness(binary_path, name):
    """Run binary and check exit code against expected value."""
    if not Path(binary_path).exists():
        return "NO_BIN"
    expected = EXPECTED_EXIT_CODES.get(name)
    if expected is None:
        return "—"
    code = run_retcode(str(binary_path))
    return "PASS" if code == expected else f"FAIL({code})"


# ---------------------------------------------------------------------------
# Output formatting
# ---------------------------------------------------------------------------

def print_table(all_results):
    """Print a formatted comparison table grouped by benchmark."""
    benchmarks = {}
    for m in all_results:
        benchmarks.setdefault(m.name, []).append(m)

    header = (
        f"{'Benchmark':<14} {'Config':<10} {'Instr':>6} "
        f"{'Text(B)':>8} {'Bin(B)':>8} {'Syscalls':>8} "
        f"{'Stack(B)':>9} {'Correct':>10}"
    )
    sep = "-" * len(header)

    print("\n" + sep)
    print(header)
    print(sep)

    for name, metrics in benchmarks.items():
        for m in metrics:
            config = f"{m.arch}/{m.opt_level}"
            print(
                f"{m.name:<14} {config:<10} "
                f"{m.instruction_count:>6} {m.text_size_bytes:>8} "
                f"{m.binary_size_bytes:>8} {m.syscall_count:>8} "
                f"{m.stack_size_bytes:>9} {m.correct:>10}"
            )
        print(sep)


def print_reduction_summary(all_results):
    """Print instruction count reduction ratios relative to O0."""
    benchmarks = {}
    for m in all_results:
        benchmarks.setdefault(m.name, {})[m.opt_level] = m

    print("\n  Instruction Count Reduction (relative to O0)")
    header = f"  {'Benchmark':<14} {'x86':>6} {'O0':>6} {'O1':>6} {'O1 %red':>8} {'O2':>6} {'O2 %red':>8}"
    print(header)
    print("  " + "-" * (len(header) - 2))

    for name, levels in benchmarks.items():
        x86_n = levels.get("src", Metrics("", "", "")).instruction_count
        o0_n = levels.get("O0", Metrics("", "", "")).instruction_count
        o1_n = levels.get("O1", Metrics("", "", "")).instruction_count
        o2_n = levels.get("O2", Metrics("", "", "")).instruction_count

        def pct(opt, base):
            if base > 0 and opt > 0:
                return f"{(1 - opt / base) * 100:.1f}%"
            return "—"

        print(
            f"  {name:<14} {x86_n:>6} {o0_n:>6} "
            f"{o1_n:>6} {pct(o1_n, o0_n):>8} "
            f"{o2_n:>6} {pct(o2_n, o0_n):>8}"
        )


def write_csv(all_results, csv_path):
    """Write results to a CSV file."""
    with open(csv_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=[
            "name", "arch", "opt_level", "instruction_count",
            "text_size_bytes", "binary_size_bytes", "syscall_count",
            "stack_size_bytes", "correct"
        ])
        writer.writeheader()
        for m in all_results:
            writer.writerow(asdict(m))
    print(f"\n  CSV  -> {csv_path}")


def write_json(all_results, json_path):
    """Write results to a JSON file."""
    with open(json_path, "w") as f:
        json.dump([asdict(m) for m in all_results], f, indent=2)
    print(f"  JSON -> {json_path}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="TransISA Benchmark Suite",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=(
            "Examples:\n"
            "  python3 analyzefiles.py\n"
            "  python3 analyzefiles.py --transisa ../build/src/TransISA\n"
            "  python3 analyzefiles.py --benchmarks ./x86 --output ./results\n"
        ),
    )
    parser.add_argument(
        "--transisa", type=str,
        default=str(SCRIPT_DIR.parent / "build" / "src" / "TransISA"),
        help="Path to TransISA binary (default: ../build/src/TransISA)"
    )
    parser.add_argument(
        "--benchmarks", type=str,
        default=str(SCRIPT_DIR / "x86"),
        help="Directory containing x86 benchmark .s files"
    )
    parser.add_argument(
        "--output", type=str,
        default=str(SCRIPT_DIR / "results"),
        help="Output directory for transpiled files and metrics"
    )
    args = parser.parse_args()

    transisa_bin = Path(args.transisa).resolve()
    bench_dir = Path(args.benchmarks).resolve()
    out_dir = Path(args.output).resolve()
    out_dir.mkdir(parents=True, exist_ok=True)

    if not transisa_bin.exists():
        print(f"Error: TransISA binary not found at {transisa_bin}")
        print("Build first: cmake -B build -S . -G Ninja && cmake --build build")
        sys.exit(1)

    asm_files = sorted(bench_dir.glob("*.s"))
    if not asm_files:
        print(f"No .s files found in {bench_dir}")
        sys.exit(1)

    print(f"  TransISA:   {transisa_bin}")
    print(f"  Benchmarks: {bench_dir} ({len(asm_files)} programs)")
    print(f"  Output:     {out_dir}\n")

    all_results = []
    for asm_file in asm_files:
        name = asm_file.stem
        print(f"  [{name}]")

        # x86 baseline
        print(f"    x86 build...")
        x86_binary = build_x86(asm_file, out_dir)
        m = collect_metrics(name, "x86", "src", x86_binary, asm_file)
        m.correct = check_correctness(x86_binary, name)
        all_results.append(m)

        # ARM at each optimization level
        for lvl in OPT_LEVELS:
            tag = f"O{lvl}"
            arm_asm = out_dir / f"{name}_{tag}.s"
            print(f"    ARM {tag}: transpile...", end=" ")
            ok = transpile(transisa_bin, asm_file, arm_asm, lvl)
            if not ok:
                print("FAILED")
                all_results.append(Metrics(name=name, arch="arm64", opt_level=tag))
                continue
            print("build...", end=" ")
            arm_binary = build_arm(arm_asm, out_dir)
            m = collect_metrics(name, "arm64", tag, arm_binary, arm_asm)
            m.correct = check_correctness(arm_binary, name)
            all_results.append(m)
            print("done")

    # Output
    print_table(all_results)
    print_reduction_summary(all_results)
    write_csv(all_results, out_dir / "metrics.csv")
    write_json(all_results, out_dir / "metrics.json")


if __name__ == "__main__":
    main()