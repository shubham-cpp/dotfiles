---@type LazySpec
return {
  "cohama/lexima.vim",
  branch = "master",
  enabled = true,
  config = function()
    vim.cmd([[call lexima#add_rule({'at': '\%#\w', 'char': '(', 'input': '('})]])
  end,
}
