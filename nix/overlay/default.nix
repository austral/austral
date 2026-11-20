final: _prev: {
  austral = final.lib.makeScope final.newScope (self: {
    austral = self.callPackage ../package/austral.nix { };

    vim-plugin = self.callPackage ../package/vim-plugin.nix { };

    default = self.austral;
  });

  vimPlugins = final.vimPlugins.extend (vfinal: _vprev: {
    austral-vim = final.austral.vim-plugin;
  });
}
