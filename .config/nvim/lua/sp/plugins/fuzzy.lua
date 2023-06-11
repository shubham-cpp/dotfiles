local opts = {
  winopts = {
    preview = {
      layout = 'vertical',
      vertical = 'up:40%',
      -- horizontal = 'right:60%',
    },
  },
}
return {
  'ibhagwan/fzf-lua',
  keys = {
    { '<C-p>', '<cmd>FzfLua files<cr>', desc = 'Find Files' },
    { '<leader>ff', '<cmd>FzfLua files<cr>', desc = '[F]ind [F]iles' },
    { '<leader>fr', '<cmd>FzfLua resume<cr>', desc = '[F]ind [R]esume' },
    { '<leader>fb', '<cmd>FzfLua buffers<cr>', desc = '[F]ind [B]uffers' },
    { '<leader>fz', '<cmd>FzfLua spell_suggest<cr>', desc = '[F]ind Spellings' },
    { '<leader>fg', '<cmd>FzfLua git_status<cr>', desc = '[F]ind [G]it Status' },
    { '<leader>fB', '<cmd>FzfLua git_branches<cr>', desc = '[F]ind Git [B]ranches' },
    { '<leader>fh', '<cmd>FzfLua help_tags<cr>', desc = '[F]ind [H]elp Tags' },
    { '<leader>fk', '<cmd>FzfLua keymaps<cr>', desc = '[F]ind [K]eymaps' },
    { '<leader>flr', '<cmd>FzfLua lsp_references<cr>', desc = '[F]ind [L]sp [R]eferences' },
    { '<leader>fls', '<cmd>FzfLua lsp_document_symbols<cr>', desc = '[F]ind [L]sp [S]ymbols' },
    {
      '<leader>fs',
      -- '<cmd>FzfLua live_grep_native<cr>',
      function()
        require('fzf-lua').live_grep_native(opts)
      end,
      desc = '[F]ind [s]earch Project',
    },
    {
      '<leader>fS',
      function()
        require('fzf-lua').lgrep_curbuf(opts)
      end,
      desc = '[F]ind [S]earch Current File',
    },
    {
      '<leader>fw',
      '<cmd>FzfLua grep_cWORD<cr>',
      desc = '[F]ind Whole [W]ord',
    },
    {
      '<leader>fW',
      '<cmd>FzfLua grep_cword<cr>',
      desc = '[F]ind [W]ord',
    },
    {
      '<leader>fn',
      function()
        require('fzf-lua').files({
          cwd = vim.fn.stdpath 'config',
        })
      end,
      desc = '[F]ind [n]eovim config',
    },
    { '<leader>fd', '<cmd>FzfLua files cwd=~/Documents/dotfiles<cr>', desc = '[F]ind [d]otfiles' },
  },
  config = function()
    local fzf = require 'fzf-lua'
    local actions = require 'fzf-lua.actions'
    local m_keys = {
      ['alt-enter'] = actions.file_tabedit,
      ['ctrl-x'] = actions.file_split,
    }
    local map = function(mode, lhs, rhs, mopts)
      local options = { noremap = true, silent = true }
      if next(mopts) then
        options = vim.tbl_extend('force', options, mopts)
      end
      vim.keymap.set(mode, lhs, rhs, options)
    end
    fzf.setup({
      fzf_opts = { ['--info'] = 'hidden' },
      winopts = {
        preview = { default = 'bat_native' },
      },
      on_create = function()
        map('t', '<C-j>', '<Down>', { buffer = true })
        map('t', '<C-k>', '<Up>', { buffer = true })
      end,
      previewers = {
        bat = {
          cmd = 'bat',
          args = '--style=changes',
        },
      },
      icons = {
        ['?'] = { icon = '?', color = 'magenta' },
        ['M'] = { icon = '★', color = 'red' },
        ['D'] = { icon = '✗', color = 'red' },
        ['A'] = { icon = '+', color = 'green' },
      },
      files = {
        winopts = {
          height = 0.55,
          width = 0.65,
          row = 0.52,
          col = 0.47,
        },
        previewer = { _ctor = false },
        actions = m_keys,
      },
      git = {
        status = {
          actions = m_keys,
          prompt = ' ❯ ',
        },
        bcommits = { actions = m_keys },
      },
      buffers = { actions = m_keys },
      blines = { actions = m_keys },
    })
  end,
}
