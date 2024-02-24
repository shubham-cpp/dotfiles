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
      picker = 'telescope', -- "telescope", "fzf", "fzf_lua" or "select"
      lsp = {
        config = {
          on_attach = function(_, bufnr)
            vim.keymap.set('n', '<leader>zl', '<cmd>ZkLinks<cr>', { buffer = bufnr, desc = '[Z]k [L]inks picker' })
            vim.keymap.set('n', 'K', vim.lsp.buf.hover, { buffer = bufnr })
            vim.keymap.set('n', '<cr>', vim.lsp.buf.definition, { buffer = bufnr })
            vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { buffer = bufnr })
            vim.keymap.set(
              'v',
              '<leader>za',
              ":'<,'>lua vim.lsp.buf.range_code_action()<CR>",
              { buffer = bufnr, desc = '[Z]k Code [A]ctions' }
            )
            vim.keymap.set('n', '<leader>zb', '<cmd>ZkBacklinks<cr>', { buffer = bufnr, desc = '[Z]k [B]acklinks' })
          --   -- Create a new note in the same directory as the current buffer, using the current selection for title.
            vim.keymap.set(
              'v',
              '<leader>znt',
              ":'<,'>ZkNewFromTitleSelection { dir = vim.fn.expand('%:p:h') }<cr>",
              { buffer = bufnr }
            )
          --   -- Create a new note in the same directory as the current buffer, using the current selection for note content and asking for its title.
            vim.keymap.set(
              'v',
              '<leader>znc',
              ":'<,'>ZkNewFromContentSelection { dir = vim.fn.expand('%:p:h'), title = vim.fn.input('Title: ') }<CR>",
              { buffer = bufnr, desc='[Z]k [N]ew [C]ontent'}
            )
          --   vim.keymap.set('n', '<leader>zi', '<cmd>ZkIndex<cr>', { desc = '[Z]k [I]ndex', buffer = bufnr })
            vim.keymap.set('v', '<leader>zf', ":'<,'>ZkMatch<CR>", { buffer = bufnr, desc = '[Z]k Notes [F]ind' })
          end,
        },
        auto_attach = {
          enabled = true,
          filetypes = { 'markdown' },
        },
      },
      -- auto_attach = {
      --   enabled = true,
      --   filetypes = { 'markdown' },
      -- },
    })
  end,
}
return config
