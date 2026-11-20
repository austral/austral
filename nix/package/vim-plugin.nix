{
  vimUtils,
  austral,
}:

vimUtils.buildVimPlugin {
  inherit (austral) pname version;
  src = ../../editor/austral.vim;
}
