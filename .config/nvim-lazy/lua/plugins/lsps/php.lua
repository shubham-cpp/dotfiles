---@type LazySpec
return {
  {
    "williamboman/mason.nvim",
    opts = {
      ensure_installed = {
        "phpcbf",
        "phpcs",
        "php-cs-fixer",
        "blade-formatter",
        "phpactor",
        "php-debug-adapter",
      },
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      local parser_config = require("nvim-treesitter.parsers").get_parser_configs()

      parser_config.blade = {
        install_info = {
          url = "https://github.com/EmranMR/tree-sitter-blade",
          files = { "src/parser.c" },
          branch = "main",
        },
        filetype = "blade",
      }

      vim.filetype.add({
        pattern = {
          [".*%.blade%.php"] = "blade",
        },
      })
      opts.ensure_installed = vim.tbl_extend("force", opts.ensure_installed, {
        "php",
        "phpdoc",
        "blade",
      })
      return opts
    end,
  },
  {
    "mfussenegger/nvim-dap",
    optional = true,
    opts = function()
      local dap = require("dap")

      dap.adapters.php = {
        type = "executable",
        command = require("mason-registry").get_package("php-debug-adapter"):get_install_path() .. "php-debug-adapter",
        -- command = 'node',
        -- args = { require('mason-registry').get_package('php-debug-adapter'):get_install_path() .. '/out/phpDebug.js' }
      }

      dap.configurations.php = {
        {
          type = "php",
          request = "launch",
          name = "Listen for Xdebug",
          port = 9000,
        },
      }
    end,
  },
}
