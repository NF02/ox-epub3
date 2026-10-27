{ pkgs ? import <nixpkgs> {} }:

let
  emacs = (pkgs.emacsPackagesFor pkgs.emacs).withPackages
    (epkgs: with epkgs; [ htmlize nov ]);

  tex = pkgs.texliveSmall.withPackages
    (ps: with ps; [ dvisvgm preview ]);
in
pkgs.mkShell {
  packages = [
    emacs
    tex
    pkgs.zip
    pkgs.unzip
    pkgs.pandoc
    pkgs.epubcheck
  ];

  shellHook = ''
    echo "ox-epub3 development shell"
    echo "  Emacs:     $(emacs --version | head -n 1)"
    echo "  epubcheck: $(epubcheck --version 2>/dev/null | head -n 1)"
  '';
}
