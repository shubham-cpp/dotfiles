return {
  'hrsh7th/nvim-cmp',
  dependencies = {
    'hrsh7th/cmp-nvim-lsp-signature-help',
    'hrsh7th/cmp-cmdline',
    'hrsh7th/cmp-nvim-lua',
  },
  opts = function(_, opts)
    local cmp = require 'cmp'
    local sources = require 'cmp.config.sources'
    opts.mapping['<C-x><C-s>'] = cmp.mapping.complete({
      config = { sources = { { name = 'luasnip' } } },
    })
    opts.mapping['<C-x><C-f>'] = cmp.mapping.complete({
      config = { sources = { { name = 'path' } } },
    })
    opts.mapping['<CR>'] = cmp.mapping({
      i = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
      c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false }),
    })

    opts.sources = cmp.config.sources({
      { name = 'nvim_lsp',                priority = 1000 },
      { name = 'nvim_lua',                priority = 1000 },
      { name = 'nvim_lsp_signature_help', priority = 900 },
      { name = 'luasnip',                 priority = 750 },
      {
        name = 'buffer',
        priority = 300,
        option = {
          get_bufnrs = function()
            return vim.api.nvim_list_bufs()
          end,
        },
      },
      { name = 'path', priority = 250 },
    })
    -- opts.sorting = {
    --   priority_weight = 2,
    --   comparators = {
    --     cmp.config.compare.recently_used,
    --     cmp.config.compare.score,
    --     cmp.config.compare.locality,
    --     cmp.config.compare.offset,
    --     cmp.config.compare.order,
    --     cmp.config.compare.exact,
    --     cmp.config.compare.kind,
    --   },
    -- }

    cmp.setup.cmdline(':', {
      sources = sources({
        {
          name = 'cmdline',
          option = {
            ignore_cmds = { 'Man', '!' },
          },
        },
        { name = 'path' },
      }),
      mapping = cmp.mapping.preset.cmdline({}),
    })

    cmp.setup.cmdline('/', {
      sources = sources({
        { name = 'nvim_lsp_signature_help' },
        { name = 'buffer',                 keyword_pattern = [=[[^[:blank:]].*]=] },
      }),
      mapping = cmp.mapping.preset.cmdline({}),
    })
    return opts
  end,
}
