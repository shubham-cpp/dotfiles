---@type LazySpec
return {
  "echasnovski/mini.surround",
  enabled = true,
  keys = {
    { "gz", "", desc = "+surround" },
    { "ys", mode = { "n", "v" }, desc = "Surround add" },
    { "ds", mode = { "n" }, desc = "Surround delete" },
    { "cs", mode = { "n" }, desc = "Surround replace" },
    { "gzf", mode = { "n" }, desc = "Surround find" },
    { "gzh", mode = { "n" }, desc = "Surround highlight" },
  },
  opts = {
    n_lines = 500,
    mappings = {
      add = "ys", -- Add surrounding in Normal and Visual modes
      delete = "ds", -- Delete surrounding
      replace = "cs", -- Replace surrounding
      find = "gzf", -- Find surrounding (to the right)
      find_left = "sF", -- Find surrounding (to the left)
      highlight = "gzh", -- Highlight surrounding
      update_n_lines = "gzn", -- Update `n_lines`
    },
  },
}
