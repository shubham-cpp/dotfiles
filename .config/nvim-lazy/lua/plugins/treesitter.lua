---@type LazySpec
return {
  "nvim-treesitter/nvim-treesitter",
  opts = {
    textobjects = {
      move = {
        enable = true,
        goto_next_start = {
          ["]f"] = "@function.outer",
          ["]c"] = "@class.outer",
          ["]a"] = "@parameter.inner",
          ["]l"] = "@loop.*",
        },
        goto_next_end = {
          ["]F"] = "@function.outer",
          ["]C"] = "@class.outer",
          ["]A"] = "@parameter.inner",
          ["]L"] = "@loop.*",
        },
        goto_previous_start = {
          ["[f"] = "@function.outer",
          ["[c"] = "@class.outer",
          ["[a"] = "@parameter.inner",
        },
        goto_previous_end = {
          ["[F"] = "@function.outer",
          ["[C"] = "@class.outer",
          ["[A"] = "@parameter.inner",
        },
        goto_next = {
          ["]i"] = "@conditional.outer",
        },
        goto_previous = {
          ["[i"] = "@conditional.outer",
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
}
