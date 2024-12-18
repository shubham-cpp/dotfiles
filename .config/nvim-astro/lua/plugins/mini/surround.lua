---@type LazySpec
return {
  "echasnovski/mini.surround",
  enabled = true,
  keys = {
    { "s", "", desc = "+surround" },
    { "sa", mode = { "n", "v" }, desc = "Surround add" },
    { "sd", mode = { "n" }, desc = "Surround delete" },
    { "sr", mode = { "n" }, desc = "Surround replace" },
    { "sf", mode = { "n" }, desc = "Surround find" },
    { "sh", mode = { "n" }, desc = "Surround highlight" },
  },
  opts = {
    search_method = "cover_or_prev",
    n_lines = 500,
    mappings = {
      add = "sa", -- Add surrounding in Normal and Visual modes
      delete = "sd", -- Delete surrounding
      replace = "sr", -- Replace surrounding
      find = "sf", -- Find surrounding (to the right)
      find_left = "sF", -- Find surrounding (to the left)
      highlight = "sh", -- Highlight surrounding
      update_n_lines = "sn", -- Update `n_lines`
    },
  },
}
