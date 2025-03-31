---@type LazySpec
return {
  "cohama/lexima.vim",
  branch = "master",
  config = function() vim.cmd [[call lexima#add_rule({'at': '\%#\w', 'char': '(', 'input': '('})]] end,
}
