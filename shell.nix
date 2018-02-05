let 
  rien = import ../rien/rien.nix {
    packageName = "prosthetic-conscience";
    packagePath = ./.;

    # Instead of using <nixpkgs>, use a lock-file to stick to
    # a particular `nixpkgs` commit.
    nixpkgsLock = ./nixpkgs.json;

    ghcVersion = "ghc822";

    overrides = rec {
      jailbreak = [ "cabal-helper" "ghc-mod" "liquidhaskell" "streaming-utils" ];
      skipHaddock = justStaticExecutables;
      skipTests = [ "cabal-helper" "ghc-mod" "tweet-hs" ];
      justStaticExecutables = [ 
        "brittany" 
        "hpack"
      ];
    };
  };

in
  rien.shell {
    # Generate Hoogle documentation?
    wantHoogle = true;

    # Haskell dependencies
    deps = hsPkgs: with hsPkgs; [
      brittany
      stylish-haskell
      hindent
      hpack
      ghc-mod

      streaming

      aeson
      attoparsec
      authenticate-oauth
      base
      bytestring
      case-insensitive
      conduit
      conduit-combinators
      conduit-extra
      containers
      data-default
      directory
      exceptions
      filepath
      http-client
      http-conduit
      http-types
      lens
      network-uri
      process
      resourcet
      scalpel
      stm-conduit
      taggy
      taggy-lens
      tagsoup
      template-haskell
      text
      time
      transformers
      transformers-base
      twitter-types
      twitter-types-lens
      wreq

      scientific
      hashable
      rel8
      postgresql-simple
      sqlite-simple
      opaleye

      beam-core
      beam-sqlite
      beam-postgres
      beam-migrate
      beam-migrate-cli
      streaming-utils
      streaming-bytestring
    ];

    # Optionally, also add sets of related packages that are
    # commonly used together.
    depSets = hsPkgs: with (rien.package-sets hsPkgs); [ ];

    # Native dependencies
    nativeDeps = pkgs: with pkgs; [
      # llvm
      postgresql
      sqlite
    ];
  }
