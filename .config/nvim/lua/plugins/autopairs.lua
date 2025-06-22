---@type LazySpec
return {
  "windwp/nvim-autopairs",
  event = "InsertEnter",
  opts = {
    enable_check_bracket_line = true,
    disable_in_visualblock = true,
    check_ts = true,
    fast_wrap = {},
  },
  config = function(_, opts)
    require("nvim-autopairs").setup(opts)
  end,
}
