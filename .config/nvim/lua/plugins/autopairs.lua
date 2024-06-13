return {
  'windwp/nvim-autopairs',
  event = 'InsertEnter',
  config = function()
    local npairs = require 'nvim-autopairs'
    npairs.setup({
      disable_filetype = { 'TelescopePrompt' },
      ignored_next_char = '[%w%.]',
      check_ts = true,
      map_c_w = true,
      fast_wrap = {},
    })
    local cmp_autopairs = require 'nvim-autopairs.completion.cmp'
    local cmp = require 'cmp'
    cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())
  end,
}
