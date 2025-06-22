---@type LazySpec
return {
  {
    "nvim-treesitter/nvim-treesitter",
    event = "VeryLazy",
    version = false, -- last release is way too old and doesn't work on Windows
    build = ":TSUpdate",
    cmd = { "TSUpdateSync", "TSUpdate", "TSInstall" },
    dependencies = {
      {
        "nvim-treesitter/nvim-treesitter-context",
        opts = { mode = "cursor", max_lines = 3 },
      },
    },
    opts = {
      highlight = { enable = true },
      indent = { enable = true },
      ensure_installed = {
        "vim",
        "lua",
        "luadoc",
        "scss",
        "vimdoc",
        "html",
        "css",
      },
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "<C-space>",
          node_incremental = "<C-space>",
          scope_incremental = false,
          node_decremental = "<bs>",
        },
      },
    },
    opts_extend = { "ensure_installed" },
    ---@param opts TSConfig
    config = function(_, opts)
      -- if type(opts.ensure_installed) == "table" then
      --   opts.ensure_installed = LazyVim.dedup(opts.ensure_installed)
      -- end
      require("nvim-treesitter.configs").setup(opts)
    end,
    keys = {
      {
        "<localleader>c",
        function()
          require("treesitter-context").go_to_context(vim.v.count1)
        end,
        desc = "Context Go-Up",
      },
      {
        "<leader><up>",
        function()
          require("treesitter-context").go_to_context(vim.v.count1)
        end,
        desc = "Context Go-Up",
      },
      {
        "<leader>ut",
        function()
          local tsc = require "treesitter-context"
          if tsc.enabled then
            tsc.disable()
          else
            tsc.enable()
          end
        end,
        desc = "Toggle TS Context",
      },
    },
  },
  {
    "windwp/nvim-ts-autotag",
    event = "InsertEnter",
    opts = {},
  },
  {
    "danymat/neogen",
    cmd = "Neogen",
    opts = {
      snippet_engine = "luasnip",
      languages = {
        lua = { template = { annotation_convention = "ldoc" } },
        typescript = { template = { annotation_convention = "tsdoc" } },
        typescriptreact = { template = { annotation_convention = "tsdoc" } },
      },
    },
    keys = {
      {
        "<leader>nn",
        function()
          require("neogen").generate { type = "any" }
        end,
        desc = "Current",
      },
      {
        "<leader>nc",
        function()
          require("neogen").generate { type = "class" }
        end,
        desc = "Class",
      },
      {
        "<leader>nf",
        function()
          require("neogen").generate { type = "func" }
        end,
        desc = "Function",
      },
      {
        "<leader>nF",
        function()
          require("neogen").generate { type = "file" }
        end,
        desc = "File",
      },
      {
        "<leader>nt",
        function()
          require("neogen").generate { type = "type" }
        end,
        desc = "Type",
      },
    },
  },
}
