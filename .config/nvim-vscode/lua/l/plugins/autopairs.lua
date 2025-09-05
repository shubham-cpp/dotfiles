---@type LazySpec
return {
  "windwp/nvim-autopairs",
  enabled = vim.g.vscode == nil,
  opts = {
    enable_check_bracket_line = false,
    fast_wrap = {},
  },
  event = "InsertEnter",
}
