---@type LazySpec
return {
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = {
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
    },
  },
  {
    "AstroNvim/astrocore",
    optional = true,
    ---@type AstroCoreOpts
    opts = {
      mappings = {
        n = {
          ["<localleader>\\"] = {
            function() require("treesitter-context").go_to_context(vim.v.count1) end,
            desc = "Jumping to context (upwards)",
            silent = true,
          },
          --- Swap the default bindings for ut & uT
          ["<Leader>uT"] = { function() require("astrocore.toggles").tabline() end, desc = "Toggle tabline" },
          ["<Leader>ut"] = { "<cmd>TSContext toggle<CR>", desc = "Toggle treesitter context" },
        },
      },
    },
  },
  {
    "nvim-treesitter/nvim-treesitter-context",
    optional = true,
    opts = { max_lines = 3 },
  },
}
