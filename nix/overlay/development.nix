_final: prev: {
  austral = prev.austral.overrideScope (afinal: _aprev: {
    dev-shell = afinal.callPackage ../package/dev-shell.nix { };
  });
}
