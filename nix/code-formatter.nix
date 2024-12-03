{
  stdenv,
  fetchFromGitHub,
  chickenPackages_5,
  chicken,
}:
stdenv.mkDerivation {
  pname = "code-formatter";
  version = "unstable-2021-07-01";

  executables = ["scheme-format"];

  src = fetchFromGitHub {
    owner = "lispunion";
    repo = "code-formatter";
    rev = "112206e3d3cc51df0ba8c04e28917484069d5aff";
    hash = "sha256-qKNJ8+gjZXtMPuE+Fxoo3s30lLf/WSR45DzWMyHJWGI=";
  };

  buildPhase = ''
    runHook preBuild
    csc -s etc.scm -j etc
    csc -s format.scm -j format
    csc -static main.scm -o scheme-format
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp scheme-format $out/bin/
    runHook postInstall
  '';

  nativeBuildInputs = with chickenPackages_5.chickenEggs; [
    chicken
    simple-loops
    callable-data-structures
    srfi-1
    srfi-13
    matchable
  ];

  doCheck = true;
}
