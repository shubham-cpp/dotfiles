---@type LazySpec
return {
  {
    "nvim-treesitter/nvim-treesitter",
    event = "VeryLazy",
    version = false, -- last release is way too old and doesn't work on Windows
    build = ":TSUpdate",
    cmd = { "TSUpdateSync", "TSUpdate", "TSInstall" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
      {
        "nvim-treesitter/nvim-treesitter-context",
        opts = { mode = "cursor", max_lines = 3 },
      },
      {
        "andymass/vim-matchup",
        init = function()
          vim.g.matchup_matchparen_offscreen = { method = "popup" }
        end,
      },
    },
    opts = {
      highlight = { enable = true },
      indent = { enable = true },
      ensure_installed = {
        "vim",
        "lua",
        "luadoc",
        "vimdoc",
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
      matchup = { enable = true },
      textobjects = {
        select = {
          enable = true,
          lookahead = true,
          keymaps = {
            ["ak"] = { query = "@block.outer", desc = "around block" },
            ["ik"] = { query = "@block.inner", desc = "inside block" },
            ["ac"] = { query = "@class.outer", desc = "around class" },
            ["ic"] = { query = "@class.inner", desc = "inside class" },
            ["a?"] = { query = "@conditional.outer", desc = "around conditional" },
            ["i?"] = { query = "@conditional.inner", desc = "inside conditional" },
            ["af"] = { query = "@function.outer", desc = "around function " },
            ["if"] = { query = "@function.inner", desc = "inside function " },
            ["ao"] = { query = "@loop.outer", desc = "around loop" },
            ["io"] = { query = "@loop.inner", desc = "inside loop" },
            ["aa"] = { query = "@parameter.outer", desc = "around argument" },
            ["ia"] = { query = "@parameter.inner", desc = "inside argument" },
          },
        },
        move = {
          enable = true,
          goto_next_start = {
            ["]k"] = { query = "@block.outer", desc = "Next start: block" },
            ["]f"] = { query = "@function.outer", desc = "Next start: function" },
            ["]x"] = { query = "@class.outer", desc = "Next start: class" },
            ["]a"] = { query = "@parameter.inner", desc = "Next start: parameter" },
          },
          goto_next_end = {
            ["]K"] = { query = "@block.outer", desc = "Next end: block" },
            ["]F"] = { query = "@function.outer", desc = "Next end: function" },
            ["]X"] = { query = "@class.outer", desc = "Next end: class" },
            ["]A"] = { query = "@parameter.inner", desc = "Next end: parameter" },
          },
          goto_previous_start = {
            ["[k"] = { query = "@block.outer", desc = "Previous start: block" },
            ["[f"] = { query = "@function.outer", desc = "Previous start: function" },
            ["[x"] = { query = "@class.outer", desc = "Previous start: class" },
            ["[a"] = { query = "@parameter.inner", desc = "Previous start: parameter" },
          },
          goto_previous_end = {
            ["[K"] = { query = "@block.outer", desc = "Previous end: block" },
            ["[F"] = { query = "@function.outer", desc = "Previous end: function" },
            ["[X"] = { query = "@class.outer", desc = "Previous end: class" },
            ["[A"] = { query = "@parameter.inner", desc = "Previous end: parameter" },
          },
        },
        swap = {
          enable = true,
          swap_next = {
            ["<localleader>k"] = { query = "@block.outer", desc = "Swap next block" },
            ["<localleader>f"] = { query = "@function.outer", desc = "Swap next function" },
            ["<localleader>a"] = { query = "@parameter.inner", desc = "Swap next argument" },
          },
          swap_previous = {
            ["<localleader>K"] = { query = "@block.outer", desc = "Swap previous block" },
            ["<localleader>F"] = { query = "@function.outer", desc = "Swap previous function" },
            ["<localleader>A"] = { query = "@parameter.inner", desc = "Swap previous argument" },
          },
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
