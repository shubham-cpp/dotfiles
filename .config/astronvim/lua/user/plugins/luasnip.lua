return {
  'L3MON4D3/LuaSnip',
  config = function(plugin, opts)
    require 'plugins.configs.luasnip'(plugin, opts) -- include the default astronvim config that calls the setup call
    require('luasnip.loaders.from_vscode').lazy_load({
      paths = { vim.fn.expand '~/Documents/dotfiles/.config/astronvim/lua/user/snippets' },
    }) -- load snippets paths
  end,
}
