local function map(mode, lhs, rhs, opts) -- {{{
  opts.buffer = opts.buffer == nil and true or opts.buffer
  opts.noremap = opts.noremap == nil and true or opts.noremap
  opts.silent = opts.silent == nil and true or opts.silent
  vim.keymap.set(mode, lhs, rhs, opts)
end --- }}}

local config = {
  'mickael-menu/zk-nvim',
  enabled = true,
  ft = { 'markdown' },
  keys = {
    { '<leader>zn', "<Cmd>ZkNew { title = vim.fn.input('Title: ') }<CR>", desc = '[Z]k [N]ew' },
    { '<leader>zo', "<Cmd>ZkNotes { sort = { 'modified' } }<CR>", desc = '[Z]k [O]pen Notes' },
    { '<leader>zt', '<Cmd>ZkTags<CR>', desc = '[Z]k [T]ags' },
    {
      '<leader>zf',
      "<Cmd>ZkNotes { sort = { 'modified' }, match = { vim.fn.input('Search: ') } }<CR>",
      desc = '[Z]k Search Notes',
    },
    {
      '<leader>zf',
      ":'<,'>ZkMatch<CR>",
      mode = 'v',
      desc = '[Z]k Search Notes',
    },
    {
      '<leader>zc',
      '<Cmd>ZkCd<cr>',
      desc = '[Z]k [C]d into notes',
    },
  },
  config = function()
    require('zk').setup({
      picker = 'fzf_lua', -- "telescope", "fzf", "fzf_lua" or "select"
      lsp = {
        config = {
          -- cmd = { 'zk', 'lsp' },
          -- name = 'zk',
          -- on_attach = function(client, bufnr)
          --   -- vim.keymap.set('n', '<cr>', '<cmd>ZkCd<cr>', { buffer = bufnr, desc = '[Z]k [C]d into notes' })
          --   vim.keymap.set('n', '<leader>zl', '<cmd>ZkLinks<cr>', { buffer = bufnr, desc = '[Z]k [L]inks picker' })
          --   vim.keymap.set('n', 'K', vim.lsp.buf.hover, { buffer = bufnr })
          --   vim.keymap.set('n', '<cr>', vim.lsp.buf.definition, { buffer = bufnr })
          --   map(
          --     'v',
          --     '<leader>za',
          --     ":'<,'>lua vim.lsp.buf.range_code_action()<CR>",
          --     { buffer = bufnr, desc = '[Z]k Code [A]ctions' }
          --   )
          --   map('n', '<leader>zb', '<cmd>ZkBacklinks<cr>', { buffer = bufnr, desc = '[Z]k [B]acklinks' })
          --   -- Create a new note in the same directory as the current buffer, using the current selection for title.
          --   map(
          --     'v',
          --     '<leader>znt',
          --     ":'<,'>ZkNewFromTitleSelection { dir = vim.fn.expand('%:p:h') }<cr>",
          --     { buffer = bufnr }
          --   )
          --   -- Create a new note in the same directory as the current buffer, using the current selection for note content and asking for its title.
          --   map(
          --     'v',
          --     '<leader>znc',
          --     ":'<,'>ZkNewFromContentSelection { dir = vim.fn.expand('%:p:h'), title = vim.fn.input('Title: ') }<CR>",
          --     { buffer = bufnr }
          --   )
          --   map('n', '<leader>zi', '<cmd>ZkIndex<cr>', { desc = '[Z]k [I]ndex', buffer = bufnr })
          --   map(
          --     'n',
          --     '<leader>zo',
          --     "<Cmd>ZkNew { title = vim.fn.input('Title: ') }<CR>",
          --     { desc = '[Z]k [N]new', buffer = bufnr, silent = false }
          --   )
          --   map(
          --     'n',
          --     '<leader>zo',
          --     "<Cmd>ZkNotes { sort = { 'modified' } }<CR>",
          --     { desc = '[Z]k [O]pen Notes', buffer = bufnr }
          --   )
          --   map(
          --     'n',
          --     '<leader>zf',
          --     "<Cmd>ZkNotes { sort = { 'modified' }, match = { vim.fn.input('Search: ') } }<CR>",
          --     { buffer = bufnr, desc = '[Z]k Notes [F]ind' }
          --   )
          --   map('v', '<leader>zf', ":'<,'>ZkMatch<CR>", { buffer = bufnr, desc = '[Z]k Notes [F]ind' })
          --   map('n', '<leader>zc', '<cmd>ZkCd<cr>', { desc = '[Z]k [C]d into notes', buffer = bufnr })
          --   map('n', '<leader>zl', '<cmd>ZkLinks<cr>', { desc = '[Z]k [L]inks picker', buffer = bufnr })
          --   print 'ZK server attached'
          -- end,
        },
        auto_attach = {
          enabled = true,
          filetypes = { 'markdown' },
        },
      },
      auto_attach = {
        enabled = true,
        filetypes = { 'markdown' },
      },
    })
  end,
}
return config
