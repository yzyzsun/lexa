{ wrapCC, stdenv, python38, cmake, ninja, fetchgit }:

wrapCC ( stdenv.mkDerivation rec {
    pname = "llvm";
    version = "c166a43";

    src = fetchgit {
    url = "https://github.com/llvm/llvm-project/";
    rev = "c166a43c6e6157b1309ea757324cc0a71c078e66";
    sha256 = "sha256-iveg9P2V7WQIQ/eL63vnYBFsR7Ob8a2Vahv8MXm4nyQ="; 
    };

    patchFile = ./preserve_none_no_save_rbp.patch;
    patchFile2 = ./omit_fp_even_with_stackmap.patch;

    buildInputs = [ python38 ];
    nativeBuildInputs = [ cmake ninja ];
    dontUseCmakeConfigure=true;
    dontStrip=true;

    patchPhase = ''
    patch -p1 -i ${patchFile}
    patch -p1 -i ${patchFile2}
    '';

    buildPhase = ''
    cmake -S llvm -B build -G Ninja \
        -DLLVM_ENABLE_PROJECTS="clang;compiler-rt" \
        -DCMAKE_BUILD_TYPE=Release \
        -DLLVM_INCLUDE_TESTS=OFF \
        -DLLVM_TARGETS_TO_BUILD=X86
    ninja -C build
    '';

    installPhase = ''
    mkdir -p $out/bin
    cp build/bin/clang $out/bin
    cp build/bin/opt $out/bin
    cp build/bin/clang-format $out/bin
    cp -r build/lib $out/lib
    mkdir -p $out/lib/clang/19/lib/linux
    cp $out/lib/clang/19/lib/x86_64-pc-linux-gnu/libclang_rt.profile.a $out/lib/clang/19/lib/linux/libclang_rt.profile-x86_64.a
    '';

    passthru.isClang = true;  
})