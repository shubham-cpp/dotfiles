local M = {}

M.config = function(_, opts)
  local options = vim.tbl_deep_extend('force', {
    disable_filetype = { 'TelescopePrompt' },
    ignored_next_char = '[%w%.]',
    check_ts = true,
    map_c_w = true,
    fast_wrap = {},
  }, opts)
  local npairs = require 'nvim-autopairs'
  npairs.setup(options)
  local ok, cmp = pcall(require, 'cmp')
  if ok then
    local cmp_autopairs = require 'nvim-autopairs.completion.cmp'
    cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())
  end
end
return M
