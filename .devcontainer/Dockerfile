FROM ubuntu:22.04

# Environment prep
ENV DEBIAN_FRONTEND=noninteractive

# Install base tools
RUN apt-get update && \
    apt-get install -y \
    build-essential \
    curl \
    wget \
    gnupg \
    ca-certificates \
    software-properties-common \
    nasm \
    gdb \
    vim \
    git \
    htop \
    lsof \
    linux-tools-common \
    linux-tools-generic \
    && apt-get clean

# Add LLVM apt repo for version 19
RUN wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh 19 && \
    rm llvm.sh

# Install core LLVM 19 tools
RUN apt-get install -y \
    clang-19 \
    llvm-19 \
    lld-19 \
    lldb-19 \
    clang-format-19 \
    clang-tidy-19

# Set default alternatives
RUN update-alternatives --install /usr/bin/clang clang /usr/bin/clang-19 100 && \
    update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-19 100 && \
    update-alternatives --install /usr/bin/llc llc /usr/bin/llc-19 100 && \
    update-alternatives --install /usr/bin/opt opt /usr/bin/opt-19 100 && \
    update-alternatives --install /usr/bin/lld lld /usr/bin/lld-19 100 && \
    update-alternatives --install /usr/bin/lldb lldb /usr/bin/lldb-19 100 && \
    update-alternatives --install /usr/bin/clang-format clang-format /usr/bin/clang-format-19 100 && \
    update-alternatives --install /usr/bin/clang-tidy clang-tidy /usr/bin/clang-tidy-19 100
# Install CMake and Ninja
RUN apt-get install -y cmake ninja-build

# Set up workspace
WORKDIR /workspace

CMD ["/bin/bash"]
