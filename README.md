# Experimental LLVM Compiler for eZ80

This repo is a fork of https://github.com/jacobly0/llvm-project.
It contains a version of LLVM C/C++ compiler for eZ80 processor.

Patches and build scripts borrowed from:

  * https://bitbucket.org/cocoacrumbselectronics/ez80-llvm-toolchain/
  * https://github.com/codebje/ez80-toolchain

Build and install the C/C++ compiler:

    export INSTALLDIR=$PWD/toolchain/ez80-none-elf
    git clone --depth 1 https://github.com/sergev/llvm-compiler-ez80.git
    pushd llvm-compiler-ez80
    cmake -B build llvm -GNinja -DLLVM_ENABLE_PROJECTS="clang" \
        -DCMAKE_INSTALL_PREFIX=$INSTALLDIR \
        -DCMAKE_BUILD_TYPE=Release \
        -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=Z80 \
        -DLLVM_TARGETS_TO_BUILD= \
        -DLLVM_DEFAULT_TARGET_TRIPLE=ez80-none-elf
    ninja -C build install
    popd

Build and install the assembler:

    curl -LO https://ftp.gnu.org/gnu/binutils/binutils-2.37.tar.gz
    tar xf binutils-2.37.tar.gz
    pushd binutils-2.37
        ./configure --target=z80-none-elf --program-prefix=ez80-none-elf- --prefix=$INSTALLDIR
        make -j
        make install
    popd

Create symbolic links so that we can differentiate this Clang toolchain
from the system Clang toolchain:

    pushd $INSTALLDIR/bin
    ln -s clang   ez80-none-elf-clang
    ln -s clang++ ez80-none-elf-clang++
    ln -s llc     ez80-none-elf-llc
    ln -s opt     ez80-none-elf-opt
    popd

Minimal C runtime for eZ80 is available here: https://bitbucket.org/cocoacrumbselectronics/minimal-c-runtime/.
