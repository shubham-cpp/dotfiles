local ensure_installed = {
  "bash",
  "fish",
  "gitcommit",
  "gitignore",
  "git_config",
  "git_rebase",
  "c",
  "cpp",
  "css",
  "go",
  "html",
  "javascript",
  "jsdoc",
  "json",
  "json5",
  "lua",
  "luadoc",
  "luap",
  "markdown",
  "markdown_inline",
  "python",
  "query",
  "rust",
  "svelte",
  "toml",
  "tsx",
  "typescript",
  "vim",
  "vimdoc",
  "vue",
  "yaml",
  "dockerfile",
  "sxhkdrc",
}

return {
  {
    "nvim-treesitter/nvim-treesitter",
    version = "main",
    build = ":TSUpdate",
    config = function()
      require("nvim-treesitter").setup({})
      vim.api.nvim_create_user_command("TreesitterInstallConfigured", function()
        require("nvim-treesitter").install(ensure_installed)
      end, { desc = "Install configured Treesitter parsers" })
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    opts = {},
    keys = {
      {
        "]/",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_start("@comment.outer")
        end,
        mode = { "n", "x", "o" },
        desc = "Next comment start",
      },
      {
        "[/",
        function()
          require("nvim-treesitter-textobjects.move").goto_previous_start("@comment.outer")
        end,
        mode = { "n", "x", "o" },
        desc = "Previous comment start",
      },
      {
        "]?",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_end("@comment.outer")
        end,
        mode = { "n", "x", "o" },
        desc = "Next comment end",
      },
      {
        "[?",
        function()
          require("nvim-treesitter-textobjects.move").goto_previous_end("@comment.outer")
        end,
        mode = { "n", "x", "o" },
        desc = "Previous comment end",
      },
      {
        "<localleader>k",
        function()
          require("nvim-treesitter-textobjects.swap").swap_next("@block.outer")
        end,
        desc = "Swap next block",
      },
      {
        "<localleader>K",
        function()
          require("nvim-treesitter-textobjects.swap").swap_previous("@block.outer")
        end,
        desc = "Swap prev block",
      },
      {
        "<localleader>f",
        function()
          require("nvim-treesitter-textobjects.swap").swap_next("@function.outer")
        end,
        desc = "Swap next function",
      },
      {
        "<localleader>F",
        function()
          require("nvim-treesitter-textobjects.swap").swap_previous("@function.outer")
        end,
        desc = "Swap prev function",
      },
      {
        "<localleader>a",
        function()
          require("nvim-treesitter-textobjects.swap").swap_next("@parameter.inner")
        end,
        desc = "Swap next parameter",
      },
      {
        "<localleader>A",
        function()
          require("nvim-treesitter-textobjects.swap").swap_previous("@parameter.inner")
        end,
        desc = "Swap prev parameter",
      },
    },
    config = function(_, opts)
      require("nvim-treesitter-textobjects").setup(opts)
    end,
  },
  {
    "tronikelis/ts-autotag.nvim",
    ft = { "html", "javascript", "javascriptreact", "typescript", "typescriptreact", "xml", "php", "templ" },
    opts = {
      auto_close = { enabled = true },
      auto_rename = { enabled = false },
    },
  },
  {
    "nvim-treesitter/nvim-treesitter-context",
    event = { "BufReadPost", "BufNewFile" },
    keys = {
      {
        "<localleader>c",
        function()
          require("treesitter-context").go_to_context(vim.v.count1)
        end,
        desc = "Jump to context",
      },
      {
        "<localleader><localleader>",
        function()
          require("treesitter-context").go_to_context(vim.v.count1)
        end,
        desc = "Jump to context",
      },
    },
    opts = {
      max_lines = 3,
      multiline_threshold = 1,
      trim_scope = "inner",
      mode = "cursor",
      on_attach = function(buf)
        return not vim.b[buf].bigfile
      end,
    },
  },
}
