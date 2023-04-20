return {
    'windwp/nvim-autopairs',
    event = 'InsertEnter',
    config = function()
      local npairs = require 'nvim-autopairs'
      npairs.setup({
        disable_filetype = { 'TelescopePrompt' },
        ignored_next_char = '[%w%.]',
        check_ts = true,
        ts_config = {
          -- lua = {'string'},-- it will not add a pair on that treesitter node
          javascript = { 'template_string' },
          typescript = { 'template_string' },
          javascriptreact = { 'template_string' },
          typescriptreact = { 'template_string' },
        },
      })
      local cmp_autopairs = require 'nvim-autopairs.completion.cmp'

      local Rule = require 'nvim-autopairs.rule'
      local cmp = require 'cmp'
      cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())
      local ts_conds = require 'nvim-autopairs.ts-conds'
      -- press % => %% only while inside a comment or string
      npairs.add_rules({
        Rule('%', '%', 'lua'):with_pair(ts_conds.is_ts_node({ 'string', 'comment' })),
        Rule('$', '$', 'lua'):with_pair(ts_conds.is_not_ts_node({ 'function' })),
      })
    end,
  }
