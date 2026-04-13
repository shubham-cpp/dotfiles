return {
  {
    url = "nvim-treesitter/nvim-treesitter",
    version = "main",
    config = function()
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
      }
      require("nvim-treesitter").setup({
        -- ensure_installed = ensure_installed,
        -- auto_install = true,
      })
      require("nvim-treesitter").install(ensure_installed)
    end,
  },
  {
    url = "nvim-treesitter/nvim-treesitter-textobjects",
    config = function()
      require("nvim-treesitter-textobjects").setup({})
    end,
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
  },
  {
    url = "JoosepAlviste/nvim-ts-context-commentstring",
    config = function()
      require("ts_context_commentstring").setup({ enable_autocmd = false })
    end,
  },
  {
    url = "tronikelis/ts-autotag.nvim",
    config = function()
      require("ts-autotag").setup({})
    end,
  },
  {
    url = "nvim-treesitter/nvim-treesitter-context",
    config = function()
      require("treesitter-context").setup({
        on_attach = function(buf)
          return not vim.b[buf].bigfile
        end,
      })
    end,
    keys = {
      {
        "<localleader>c",
        function()
          require("treesitter-context").go_to_context()
        end,
        desc = "Jump to context",
      },
      {
        "<localleader><localleader>",
        function()
          require("treesitter-context").go_to_context()
        end,
        desc = "Jump to context",
      },
    },
  },
}
