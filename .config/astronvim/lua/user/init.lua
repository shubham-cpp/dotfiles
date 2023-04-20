local util = require 'user.lsp.util'

return {
  lsp = {
    config = {
      hls = { cmd = { util.xdg_data_bin() .. '/haskell-language-server-9.2.5', '--lsp' } },
      html = { cmd = { util.bun_path() .. '/vscode-html-language-server', '--stdio' } },
      cssls = { cmd = { util.bun_path() .. '/vscode-css-language-server', '--stdio' } },
      jsonls = { cmd = { util.bun_path() .. '/vscode-json-language-server', '--stdio' } },
      eslint = { cmd = { util.bun_path() .. '/vscode-eslint-language-server', '--stdio' } },
      volar = { cmd = { util.bun_path() .. '/vue-language-server', '--stdio' } },
      bashls = { cmd = { util.bun_path() .. '/bash-language-server', 'start' } },
      awk_ls = { cmd = { util.bun_path() .. '/awk-language-server' } },
      dockerls = { cmd = { util.bun_path() .. '/docker-langserver', '--stdio' } },
      svelte = { cmd = { util.bun_path() .. '/svelteserver', '--stdio' } },
      emmet_ls = { cmd = { util.bun_path() .. '/emmet-ls', '--stdio' } },
      vimls = { cmd = { util.bun_path() .. '/vim-language-server', '--stdio' } },
      astro = { cmd = { util.bun_path() .. '/astro-ls', '--stdio' } },
      prismals = { cmd = { util.bun_path() .. '/prisma-language-server', '--stdio' } },
      tailwindcss = { cmd = { util.bun_path() .. '/tailwindcss-language-server', '--stdio' } },
      docker_compose_language_service = { cmd = { util.bun_path() .. '/docker-compose-langserver', '--stdio' } },
    },
  },
  plugins = {
    {
      'L3MON4D3/LuaSnip',
      config = function(plugin, opts)
        require 'plugins.configs.luasnip' (plugin, opts) -- include the default astronvim config that calls the setup call
        require('luasnip.loaders.from_vscode').lazy_load({
          paths = { '~/Documents/dotfiles/.config/astronvim/lua/user/snippets' },
        }) -- load snippets paths
      end,
    },
    {
      'nvim-neo-tree/neo-tree.nvim',
      version = '*',
      opts = function(_, opts)
        -- opts.default_component_configs.name.trailing_slash = true
        opts.window.mappings['l'] = 'open'
        opts.window.mappings['t'] = 'open_tabnew'
        opts.window.mappings['?'] = 'show_help'
        opts.window.mappings['<'] = 'prev_source'
        opts.window.mappings['>'] = 'next_source'
        return opts
      end,
    },
  },
}
