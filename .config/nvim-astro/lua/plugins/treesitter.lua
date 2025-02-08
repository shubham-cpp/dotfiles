---@type LazySpec
return {
  "nvim-treesitter/nvim-treesitter",
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
}
