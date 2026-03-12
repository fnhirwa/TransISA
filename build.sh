#!/bin/bash
set -e

PROJECT_ROOT="$(cd "$(dirname "$0")" && pwd)"
BUILD_DIR="$PROJECT_ROOT/build"
BUILD_TYPE="${BUILD_TYPE:-Release}"
GENERATOR="Ninja"
DEFAULT_PRESET="default"

has_configure_preset() {
    [ -f "$PROJECT_ROOT/CMakePresets.json" ] || return 1
    cmake --list-presets >/dev/null 2>&1 || return 1
    cmake --list-presets 2>/dev/null | grep -q "^[[:space:]]*\"$DEFAULT_PRESET\"[[:space:]]*-"
}

# Fall back to Makefiles if Ninja isn't installed
if ! command -v ninja &>/dev/null; then
    GENERATOR="Unix Makefiles"
fi

usage() {
    cat <<EOF
Usage: ./build.sh <command>

Commands:
    configure   Run CMake configuration
    build       Build the project (configures first if needed)
    test        Run all tests
    bench       Run the benchmark suite
    clean       Remove the build directory
    rebuild     Clean + build
    help        Show this message

Environment:
    BUILD_TYPE  CMake build type (default: Release)
    LLVM_DIR    Override LLVM cmake path if auto-detection fails

Examples:
    ./build.sh build
    ./build.sh test
    ./build.sh bench
    BUILD_TYPE=Debug ./build.sh rebuild
    LLVM_DIR=/opt/homebrew/opt/llvm@19/lib/cmake/llvm ./build.sh build
EOF
}

do_configure() {
    echo "-- Configuring ($BUILD_TYPE, $GENERATOR)"
    cmake_args=()

    if has_configure_preset; then
        cmake_args+=(--preset "$DEFAULT_PRESET")
    else
        cmake_args=(
            -B "$BUILD_DIR"
            -S "$PROJECT_ROOT"
            -G "$GENERATOR"
            -DCMAKE_BUILD_TYPE="$BUILD_TYPE"
        )
    fi

    if [ -n "$BUILD_TYPE" ]; then
        cmake_args+=(-DCMAKE_BUILD_TYPE="$BUILD_TYPE")
    fi

    if [ -n "$LLVM_DIR" ]; then
        cmake_args+=(-DLLVM_DIR="$LLVM_DIR")
    fi

    cmake "${cmake_args[@]}"
}

do_build() {
    if [ ! -f "$BUILD_DIR/build.ninja" ] && [ ! -f "$BUILD_DIR/Makefile" ]; then
        do_configure
    fi
    echo "-- Building"
    cmake --build "$BUILD_DIR" -j "$(nproc 2>/dev/null || sysctl -n hw.ncpu)"
    echo "-- Binary: $BUILD_DIR/src/TransISA"
}

do_test() {
    do_build
    echo "-- Running tests"
    cd "$BUILD_DIR"
    ctest --output-on-failure
}

do_bench() {
    do_build
    echo "-- Running benchmarks"
    cd "$PROJECT_ROOT/benchmarking"
    python3 analyzefiles.py --transisa "$BUILD_DIR/src/TransISA"
}

do_clean() {
    echo "-- Cleaning $BUILD_DIR"
    rm -rf "$BUILD_DIR"
}

case "${1:-help}" in
    configure)  do_configure ;;
    build)      do_build ;;
    test)       do_test ;;
    bench)      do_bench ;;
    clean)      do_clean ;;
    rebuild)    do_clean; do_build ;;
    help|*)     usage ;;
esac