return {
  'ibhagwan/fzf-lua',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  cmd = 'FzfLua',
  keys = function()
    local fzf = require 'fzf-lua'
    return {
      { '<leader>pf', fzf.files, desc = '[F]iles' },
      { '<leader>pr', fzf.resume, desc = '[R]esume' },
      { '<leader>ps', fzf.live_grep_native, desc = '[S]earch(Project)' },
      {
        '<leader>pS',
        function()
          fzf.grep_curbuf({
            winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
          })
        end,
        desc = '[S]earch Current Buffer',
      },
      { '<leader>pw', fzf.grep_cWORD, desc = '[W]ord under cursor' },
      { '<leader>pw', fzf.grep_visual, mode = 'v', desc = 'Selection' },
      { '<leader>pb', fzf.buffers, desc = '[B]uffers' },
      { '<leader>pz', fzf.spell_suggest, desc = '[S]pelling' },
      { '<leader>pk', fzf.keymaps, desc = '[K]eymaps' },
      { '<leader>ph', fzf.help_tags, desc = '[H]elp Tags' },
      { '<leader>pd', fzf.diagnostics, desc = '[D]iagnostics' },
      { '<leader>pgg', fzf.git_status, desc = '[S]tatus' },
      { '<leader>pgb', fzf.git_branches, desc = '[B]ranches' },
      { '<leader>pgc', fzf.git_commits, desc = '[C]ommits' },
      { '<leader>pgC', fzf.git_bcommits, desc = '[B]ranch Commits' },
    }
  end,
  config = function()
    local fzf = require 'fzf-lua'
    local actions = require 'fzf-lua.actions'
    local m_keys = {
      ['alt-enter'] = actions.file_tabedit,
      ['ctrl-x'] = actions.file_split,
      ['ctrl-q'] = actions.file_edit_or_qf,
    }
    -- calling `setup` is optional for customization
    fzf.setup({
      'telescope',

      fzf_opts = {
        ['--layout'] = 'reverse',
        ['--info'] = 'inline-right',
        -- ['--tiebreak'] = 'end',
      },
      keymap = {
        fzf = {
          ['ctrl-j'] = 'down',
          ['ctrl-k'] = 'up',
          ['ctrl-f'] = 'half-page-down',
          ['ctrl-b'] = 'half-page-up',
          ['alt-a'] = 'toggle-all',
          ['pgdn'] = 'preview-page-down',
          ['pgup'] = 'preview-page-up',
          ['alt-j'] = 'preview-down',
          ['alt-k'] = 'preview-up',
          ['shift-down'] = 'preview-page-down',
          ['shift-up'] = 'preview-page-up',
        },
      },
      icons = {
        ['?'] = { icon = '?', color = 'magenta' },
        ['M'] = { icon = '★', color = 'red' },
        ['D'] = { icon = '✗', color = 'red' },
        ['A'] = { icon = '+', color = 'green' },
      },
      files = {
        fzf_opts = {
          ['--layout'] = 'reverse',
          ['--tiebreak'] = 'end',
        },
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
        bcommits = {
          actions = m_keys,
          winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
        },
        commits = {
          actions = m_keys,
          winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
        },
      },
      buffers = {
        ignore_current_buffer = true,
        winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
        actions = vim.tbl_extend('force', m_keys, {
          ['ctrl-d'] = actions.buf_delete,
          ['ctrl-x'] = actions.buf_split,
          ['ctrl-v'] = actions.buf_vsplit,
          ['ctrl-q'] = actions.buf_edit_or_qf,
        }),
      },
      blines = {
        actions = m_keys,
        no_term_buffers = false,
        winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
      },
      lsp = {
        finder = {
          actions = m_keys,
          winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
        },
        code_actions = {
          actions = m_keys,
          winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
        },
      },
      diagnostics = {
        actions = m_keys,
        winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
      },
      lines = { actions = m_keys },
    })
  end,
}
