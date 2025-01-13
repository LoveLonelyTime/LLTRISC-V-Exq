{
  stdenv,
  fetchzip,
  autoPatchelfHook,
  zlib,
  libgcc
}:
stdenv.mkDerivation {
  pname = "firrtl";
  version = "1.43.0";
  src = fetchzip {
    url = "https://github.com/llvm/circt/releases/download/firtool-1.43.0/firrtl-bin-ubuntu-20.04.tar.gz";
    hash = "sha256-psm4n7Qhj8UP3ONO79yrcXm+Qph0JeKVVQb26Lgt794=";
  };

  nativeBuildInputs = [
    autoPatchelfHook
  ];

  buildInputs = [
    zlib libgcc
  ];

  installPhase = ''
    runHook preInstall
    ls
    install -m755 -D bin/firtool $out/bin/firtool
    runHook postInstall
  '';
}
