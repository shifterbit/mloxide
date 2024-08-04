{ lib, rustPlatform }:

rustPlatform.buildRustPackage  {
  pname = "mloxide";
  version = "0.0.0";

  src = ./.;

  cargoHash = "sha256-ucOxyVEr7UPNcrgxjJmiBhPp+DiuXfRG/UMUrJdXInQ=";

  meta = {
    description = "A statically typed functional programming language inspired by ML";
    homepage = "https://github.com/shifterbit.mloxide";
    license = lib.licenses.mit;
  };
}


