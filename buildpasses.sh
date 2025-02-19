
currPath=${PWD}

# Clone and build LLVM
if ! [[ -e "llvm-project" ]]; then
    git clone https://github.com/llvm/llvm-project/
    cd llvm-project
    git checkout c166a43c6e6157b1309ea757324cc0a71c078e66
    mkdir build && cd build
    cmake -GNinja -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD=host ../llvm
    ninja
fi

# Build LLVM passes
buildPath="${currPath}/passes/build"
if ! [[ -e ${buildPath} ]]; then
    mkdir ${buildPath}
fi
cd ${buildPath}
cmake -DLLVM_DIR=${currPath}/llvm-project/build/lib/cmake/llvm ..
make