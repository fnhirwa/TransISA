#!/usr/bin/env python3

import os
import subprocess
import re
import sys
from pathlib import Path

CURRENT_DIR = Path(__file__).parent.resolve()
METRICS = {"ARM": {}, "X86": {}}


def run_cmd(cmd):
    try:
        result = subprocess.check_output(cmd, shell=True, text=True)
        return result
    except subprocess.CalledProcessError:
        return ""


def count_instructions(binary_path):
    cmd = f"llvm-objdump --disassemble --no-show-raw-insn {binary_path}"
    output = run_cmd(cmd)

    count = 0
    for line in output.splitlines():
        # Match lines that contain an instruction (heuristic: address + tab + mnemonic)
        # Example: "100003f74:  mov     x29, sp"
        if re.match(r"^\s*[0-9a-fA-F]+:\s+\S", line):
            count += 1
    return count


def get_text_segment_size(binary_path):
    output = run_cmd(f"size -m {binary_path}")
    for line in output.splitlines():
        if "Section __text:" in line:
            match = re.search(r"Section __text:\s+(\d+)", line)
            if match:
                return int(match.group(1))
    return 0


def count_syscalls(binary_path):
    output = run_cmd(f"otool -tv {binary_path}")
    lines = output.splitlines()
    return sum(
        1
        for line in lines
        if any(keyword in line for keyword in ("svc", "int", "syscall"))
    )


def estimate_stack_size(asm_path):
    asm_code = Path(asm_path).read_text()
    matches = re.findall(r"sub\s+sp,\s*sp,\s*#(0x[0-9a-fA-F]+|\d+)", asm_code)
    matches_shift = re.findall(
        r"sub\s+sp,\s*sp,\s*#(0x[0-9a-fA-F]+|\d+),\s*lsl\s*#(\d+)", asm_code
    )

    total = sum(int(v, 0) for v in matches)
    for base, shift in matches_shift:
        total += int(base, 0) << int(shift)
    return total


def analyze(binary_path, asm_path):
    return {
        "Instruction Count": count_instructions(binary_path),
        "Text Size (bytes)": get_text_segment_size(binary_path),
        "Syscall Count": count_syscalls(binary_path),
        "Stack Size (bytes)": estimate_stack_size(asm_path),
    }


def process_arm64_assembly_files(directory=CURRENT_DIR):
    """
    Compile all assembly files in the given directory.
    Generate binary files from the assembly files in the directory.
    """
    global METRICS
    armdir = os.path.join(directory, "arm")
    if not os.path.exists(armdir):
        print(f"Directory {armdir} does not exist.")
        return
    asm_files = list(Path(armdir).glob("*.s"))
    if not asm_files:
        print("No assembly files found in the directory.")
        return
    for asm_file in asm_files:
        binary_file = asm_file.with_suffix(".o")
        if binary_file.exists():
            print(f"Removing existing {binary_file}...")
            os.remove(binary_file)
        if not binary_file.exists():
            print(f"Compiling {asm_file} to {binary_file}...")
            os.system(f"as -arch arm64 {asm_file} -o  {binary_file}")
        print(f"Linking {binary_file} to executable...")
        os.system(
            f"ld -macosx_version_min 11.0 -o {binary_file.with_suffix('.out')} {binary_file} -lSystem -syslibroot $(xcrun --show-sdk-path) -e __start"
        )
        print(f"Processing {binary_file.with_suffix('.out')}...")
        metrics = analyze(binary_file.with_suffix(".out"), asm_file)
        METRICS["ARM"][asm_file.stem] = metrics
    print("All assembly files processed.")


def process_x86_assembly_files(directory=CURRENT_DIR):
    """
    Compile all assembly files in the given directory.
    Generate binary files from the assembly files in the directory.
    """
    global METRICS
    x86dir = os.path.join(directory, "x86")
    if not os.path.exists(x86dir):
        print(f"Directory {x86dir} does not exist.")
        return
    asm_files = list(Path(x86dir).glob("*.s"))
    if not asm_files:
        print("No assembly files found in the directory.")
        return
    for asm_file in asm_files:
        binary_file = asm_file.with_suffix(".o")
        if binary_file.exists():
            print(f"Removing existing {binary_file}...")
            os.remove(binary_file)
        if not binary_file.exists():
            print(f"Compiling {asm_file} to {binary_file}...")
            os.system(f"as -arch x86_64 {asm_file} -o  {binary_file}")
        print(f"Linking {binary_file} to executable...")
        os.system(
            f"ld -arch x86_64 -macosx_version_min 10.7 -lSystem -o {binary_file.with_suffix('.out')} {binary_file} -syslibroot $(xcrun --show-sdk-path) -e _start"
        )
        print(f"Processing {binary_file.with_suffix('.out')}...")
        metrics = analyze(binary_file.with_suffix(".out"), asm_file)
        METRICS["X86"][asm_file.stem] = metrics
    print("All assembly files processed.")


def print_comparison():
    """
    Print the comparison of metrics between ARM and X86 assembly files.
    """
    print("\nComparison of ARM and X86 Assembly Files:")
    for asm_file in METRICS["ARM"]:
        arm_metrics = METRICS["ARM"][asm_file]
        x86_metrics = METRICS["X86"].get(asm_file, {})
        
        metrics = list(arm_metrics.keys())
        print(f"\nMetrics for {asm_file}:\n")
        print(f"{'Metric':<25} {'x86':>10} {'AArch64':>10}")
        print("-" * 50)
        for metric in metrics:
            arm_value = arm_metrics[metric]
            x86_value = x86_metrics.get(metric, "N/A")
            print(f"{metric:<25} {x86_value:>10} {arm_value:>10}")


if __name__ == "__main__":
    if len(sys.argv) > 1:
        directory = sys.argv[1]
    else:
        directory = CURRENT_DIR
    process_arm64_assembly_files(directory)
    process_x86_assembly_files(directory)
    print_comparison()
