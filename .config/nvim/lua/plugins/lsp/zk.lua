local config = {
  {
    'mickael-menu/zk-nvim',
    enabled = true,
    ft = { 'markdown' },
    keys = {
      { '<leader>zn', "<Cmd>ZkNew { title = vim.fn.input('Title: ') }<CR>", desc = '[Z]k [N]ew' },
      { '<leader>zf', "<Cmd>ZkNotes { sort = { 'modified' } }<CR>", desc = '[Z]k [O]pen Notes' },
      { '<leader>zo', "<Cmd>ZkNotes { sort = { 'modified' } }<CR>", desc = '[Z]k [O]pen Notes' },
      { '<leader>zt', '<Cmd>ZkTags<CR>', desc = '[Z]k [T]ags' },
      {
        '<leader>zs',
        "<Cmd>ZkNotes { sort = { 'modified' }, match = { vim.fn.input('Search: ') } }<CR>",
        desc = '[Z]k Search Notes',
      },
      {
        '<leader>zs',
        ":'<,'>ZkMatch<CR>",
        mode = 'v',
        desc = '[Z]k Search Notes',
      },
      {
        '<leader>zc',
        '<Cmd>ZkCd<cr>',
        desc = '[Z]k [C]d into notes',
      },
      {
        '<leader>zl',
        '<Cmd>ZkLinks<cr>',
        desc = '[Z]k [L]inks picker',
      },
    },
    config = function()
      local picker = 'select'
      local ok_fzf, _ = pcall(require, 'fzf-lua')
      if ok_fzf then
        picker = 'fzf_lua'
      else
        local ok_telescope, _ = pcall(require, 'telescope')
        if ok_telescope then
          picker = 'telescope'
        end
      end

      require('zk').setup({
        picker = picker, -- "telescope", "fzf", "fzf_lua" or "select"
        lsp = {
          config = {
            on_attach = function(_, bufnr)
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
              vim.keymap.set(
                'v',
                '<leader>znt',
                ":'<,'>ZkNewFromTitleSelection { dir = vim.fn.expand('%:p:h') }<cr>",
                { buffer = bufnr }
              )
              vim.keymap.set(
                'v',
                '<leader>znc',
                ":'<,'>ZkNewFromContentSelection { dir = vim.fn.expand('%:p:h'), title = vim.fn.input('Title: ') }<CR>",
                { buffer = bufnr, desc = '[Z]k [N]ew [C]ontent' }
              )
              vim.keymap.set('v', '<leader>zf', ":'<,'>ZkMatch<CR>", { buffer = bufnr, desc = '[Z]k Notes [F]ind' })
            end,
          },
          auto_attach = {
            enabled = true,
            filetypes = { 'markdown' },
          },
        },
      })
    end,
  },
  {
    'williamboman/mason.nvim',
    opts = function(_, opts)
      opts.ensure_installed = require('my_config.util').unique_append_table(opts.ensure_installed, {
        'zk',
      })
      return opts
    end,
  },
  { 'AstroNvim/astrolsp', opts = { handlers = { zk = false } } },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = require('my_config.util').unique_append_table(opts.ensure_installed, {
        'markdown',
        'markdown_inline',
        'comment',
        'todotxt',
        'embedded_template',
      })
      return opts
    end,
  },
}
return config
