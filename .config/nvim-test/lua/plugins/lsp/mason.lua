---@type LazySpec
return {
  {
    'williamboman/mason.nvim',
    cmd = { 'Mason', 'MasonInstall', 'MasonUninstall', 'MasonUpdate' },
    event = 'BufReadPre',
    opts = {
      ---@type '"prepend"' | '"append"' | '"skip"'
      PATH = 'append',
      ui = {
        icons = {
          package_installed = '✓',
          package_uninstalled = '✗',
          package_pending = '⟳',
        },
      },
    },
    opts_extend = { 'ensure_installed' },
    ---@param opts MasonSettings | {ensure_installed: string[]}
    config = function(_, opts)
      require('mason').setup(opts)
      local mr = require 'mason-registry'
      mr.refresh(function()
        for _, tool in ipairs(opts.ensure_installed or {}) do
          local p = mr.get_package(tool)
          if not p:is_installed() then
            p:install()
          end
        end
      end)
    end,
  },
  {
    'williamboman/mason-lspconfig.nvim',
    event = 'BufReadPre',
    dependencies = {
      'mason.nvim',
    },
    opts = {
      -- use AstroLSP setup for mason-lspconfig
      handlers = {
        function(server)
          require('astrolsp').lsp_setup(server)
        end,
      },
    },
  },
}
