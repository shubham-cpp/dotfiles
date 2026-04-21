return {
  {
    "nvim-treesitter/nvim-treesitter",
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
        "sxhkdrc",
      }
      require("nvim-treesitter").setup({})
      require("nvim-treesitter").install(ensure_installed)
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    opts = {},
    config = function(_, opts)
      require("nvim-treesitter-textobjects").setup(opts)
      vim.keymap.set({ "n", "x", "o" }, "]/", function()
        require("nvim-treesitter-textobjects.move").goto_next_start("@comment.outer")
      end, { desc = "Next comment start" })
      vim.keymap.set({ "n", "x", "o" }, "[/", function()
        require("nvim-treesitter-textobjects.move").goto_previous_start("@comment.outer")
      end, { desc = "Previous comment start" })
      vim.keymap.set({ "n", "x", "o" }, "]?", function()
        require("nvim-treesitter-textobjects.move").goto_next_end("@comment.outer")
      end, { desc = "Next comment end" })
      vim.keymap.set({ "n", "x", "o" }, "[?", function()
        require("nvim-treesitter-textobjects.move").goto_previous_end("@comment.outer")
      end, { desc = "Previous comment end" })
      vim.keymap.set("n", "<localleader>k", function()
        require("nvim-treesitter-textobjects.swap").swap_next("@block.outer")
      end, { desc = "Swap next block" })
      vim.keymap.set("n", "<localleader>K", function()
        require("nvim-treesitter-textobjects.swap").swap_previous("@block.outer")
      end, { desc = "Swap prev block" })
      vim.keymap.set("n", "<localleader>f", function()
        require("nvim-treesitter-textobjects.swap").swap_next("@function.outer")
      end, { desc = "Swap next function" })
      vim.keymap.set("n", "<localleader>F", function()
        require("nvim-treesitter-textobjects.swap").swap_previous("@function.outer")
      end, { desc = "Swap prev function" })
      vim.keymap.set("n", "<localleader>a", function()
        require("nvim-treesitter-textobjects.swap").swap_next("@parameter.inner")
      end, { desc = "Swap next parameter" })
      vim.keymap.set("n", "<localleader>A", function()
        require("nvim-treesitter-textobjects.swap").swap_previous("@parameter.inner")
      end, { desc = "Swap prev parameter" })
    end,
  },
  { "tronikelis/ts-autotag.nvim", opts = {} },
  {
    "nvim-treesitter/nvim-treesitter-context",
    config = function()
      require("treesitter-context").setup({
        on_attach = function(buf)
          return not vim.b[buf].bigfile
        end,
      })
      vim.keymap.set("n", "<localleader>c", function()
        require("treesitter-context").go_to_context()
      end, { desc = "Jump to context" })
      vim.keymap.set("n", "<localleader><localleader>", function()
        require("treesitter-context").go_to_context()
      end, { desc = "Jump to context" })
    end,
  },
}
