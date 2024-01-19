local util = require 'user.lsp.util'

return {
  lsp = {
    config = {
      -- hls = { cmd = { util.xdg_data_bin() .. '/haskell-language-server-9.2.5', '--lsp' } },
      html = { cmd = { util.bun_path() .. '/vscode-html-language-server', '--stdio' } },
      cssls = { cmd = { util.bun_path() .. '/vscode-css-language-server', '--stdio' } },
      jsonls = { cmd = { util.bun_path() .. '/vscode-json-language-server', '--stdio' } },
      eslint = { cmd = { util.bun_path() .. '/vscode-eslint-language-server', '--stdio' } },
      volar = { cmd = { util.bun_path() .. '/vue-language-server', '--stdio' } },
      bashls = { cmd = { util.bun_path() .. '/bash-language-server', 'start' } },
      -- awk_ls = { cmd = { util.bun_path() .. '/awk-language-server' } },
      -- dockerls = { cmd = { util.bun_path() .. '/docker-langserver', '--stdio' } },
      svelte = { cmd = { util.bun_path() .. '/svelteserver', '--stdio' } },
      -- emmet_ls = { cmd = { util.bun_path() .. '/emmet-ls', '--stdio' } },
      emmet_language_server = { cmd = { util.bun_path() .. '/emmet-language-server', '--stdio' } },
      vimls = { cmd = { util.bun_path() .. '/vim-language-server', '--stdio' } },
      astro = { cmd = { util.bun_path() .. '/astro-ls', '--stdio' } },
      prismals = { cmd = { util.bun_path() .. '/prisma-language-server', '--stdio' } },
      tailwindcss = { cmd = { util.bun_path() .. '/tailwindcss-language-server', '--stdio' } },
      -- docker_compose_language_service = { cmd = { util.bun_path() .. '/docker-compose-langserver', '--stdio' } },
    },
  },
}
