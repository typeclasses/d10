let

sources = import ./nix/sources.nix;
nixos-22-05 = import sources."nixos-22.05" {};
nixos-22-11 = import sources."nixos-22.11" {};
inherit (nixos-22-11) haskell lib symlinkJoin;
inherit (lib) fold composeExtensions concatMap attrValues;

combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

sourceOverrides = haskell.lib.packageSourceOverrides {
    d10 = ./d10;
};

depOverrides = new: old: {
    # ascii-case = new.callPackage ./nix/ascii-case-1.0.1.0.nix {};
};

ghc."8.10" = nixos-22-05.haskell.packages.ghc8107.override (old: {
    overrides = combineOverrides old [ sourceOverrides depOverrides ];
});

ghc."9.0" = nixos-22-11.haskell.packages.ghc90.override (old: {
    overrides = combineOverrides old [ sourceOverrides depOverrides ];
});

ghc."9.2" = nixos-22-11.haskell.packages.ghc92.override (old: {
    overrides = combineOverrides old [ sourceOverrides depOverrides ];
});

ghc."9.4" = nixos-22-11.haskell.packages.ghc94.override (old: {
    overrides = combineOverrides old [ sourceOverrides depOverrides ];
});

in

symlinkJoin {
    name = "d10";
    paths = concatMap (x: [x.d10]) (attrValues ghc);
} // {
    inherit ghc;
    pkgs = nixos-22-11;
}
