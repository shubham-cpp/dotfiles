---@type LazySpec
return {
  {
    "nvim-treesitter/nvim-treesitter",
    version = false, -- last release is way too old and doesn't work on Windows
    build = ":TSUpdate",
    lazy = false,
    cmd = { "TSUpdateSync", "TSUpdate", "TSInstall" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
      "andymass/vim-matchup",
    },
    opts_extend = { "ensure_installed" },
    ---@type TSConfig
    ---@diagnostic disable-next-line: missing-fields
    opts = {
      highlight = {
        enable = true,
        disable = function(_, bufnr)
          local line_count = vim.api.nvim_buf_line_count(bufnr)
          if line_count > 2500 then return true end
        end,
      },
      indent = { enable = true },
      ensure_installed = {
        "bash",
        "fish",
        "diff",
        "git_config",
        "git_rebase",
        "gitattributes",
        "gitcommit",
        "gitignore",
        "hyprlang",
        "ini",
        "query",
        "rasi",
        "regex",
        "sql",
        "ssh_config",
        "sxhkdrc",
        "tmux",
        "vim",
        "vimdoc",
        "xml",
        "zathurarc",
      },
      textobjects = {
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
      matchup = { enable = true },
    },
    ---@param opts TSConfig
    config = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        opts.ensure_installed = require("user.config.util").dedup(opts.ensure_installed)
      end
      require("nvim-treesitter.configs").setup(opts)
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter-context",
    event = "BufRead",
    opts = {},
    cmd = { "TSContextEnable", "TSContextDisable", "TSContextToggle" },
    keys = {
      {
        "<localleader>c",
        function() require("treesitter-context").go_to_context(vim.v.count1) end,
        silent = true,
        desc = "Jumping to context (upwards)",
      },
      { "<leader>uc", "<cmd>TSContextToggle<cr>", desc = "TSContextToggle" },
    },
  },
  {
    "windwp/nvim-ts-autotag",
    event = "VeryLazy",
    opts = {},
  },
}
