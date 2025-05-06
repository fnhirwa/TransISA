#!/usr/bin/env python3

import subprocess
import re
import sys
from pathlib import Path

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
    return sum(1 for line in lines if any(keyword in line for keyword in ("svc", "int", "syscall")))

def estimate_stack_size(asm_path):
    asm_code = Path(asm_path).read_text()
    matches = re.findall(r"sub\s+sp,\s*sp,\s*#(0x[0-9a-fA-F]+|\d+)", asm_code)
    matches_shift = re.findall(r"sub\s+sp,\s*sp,\s*#(0x[0-9a-fA-F]+|\d+),\s*lsl\s*#(\d+)", asm_code)

    total = sum(int(v, 0) for v in matches)
    for base, shift in matches_shift:
        total += int(base, 0) << int(shift)
    return total

def analyze(binary_path, asm_path):
    return {
        "Instruction Count": count_instructions(binary_path),
        "Text Size (bytes)": get_text_segment_size(binary_path),
        "Syscall Count": count_syscalls(binary_path),
        "Stack Size (bytes)": estimate_stack_size(asm_path)
    }

def print_comparison(x86_metrics, arm_metrics):
    metrics = list(x86_metrics.keys())
    print(f"{'Metric':<25} {'x86':>10} {'AArch64':>10}")
    print("-" * 50)
    for m in metrics:
        print(f"{m:<25} {x86_metrics[m]:>10} {arm_metrics[m]:>10}")

if __name__ == "__main__":
    if len(sys.argv) != 5:
        print("Usage: python analyze_binary.py <x86_bin> <x86_asm> <arm_bin> <arm_asm>")
        sys.exit(1)

    x86_bin, x86_asm, arm_bin, arm_asm = sys.argv[1:5]

    x86_metrics = analyze(x86_bin, x86_asm)
    arm_metrics = analyze(arm_bin, arm_asm)

    print_comparison(x86_metrics, arm_metrics)
