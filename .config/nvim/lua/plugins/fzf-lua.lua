return {
  'ibhagwan/fzf-lua',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  cmd = 'FzfLua',
  keys = function()
    local fzf = require 'fzf-lua'
    return {
      { '<C-p>', fzf.files, desc = '[F]iles' },
      { '<leader>ff', fzf.files, desc = '[F]iles' },
      { '<leader>fr', fzf.resume, desc = '[R]esume' },
      { '<leader>fs', fzf.live_grep_native, desc = '[S]earch(Project)' },
      {
        '<leader>fS',
        function()
          fzf.grep_curbuf({
            winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
          })
        end,
        desc = '[S]earch Current Buffer',
      },
      { '<leader>fw', fzf.grep_cWORD, desc = '[W]ord under cursor' },
      { '<leader>fw', fzf.grep_visual, mode = 'v', desc = 'Selection' },
      { '<leader>fb', fzf.buffers, desc = '[B]uffers' },
      { '<leader>fz', fzf.spell_suggest, desc = '[S]pelling' },
      { '<leader>fk', fzf.keymaps, desc = '[K]eymaps' },
      { '<leader>fh', fzf.help_tags, desc = '[H]elp Tags' },
      { '<leader>fD', fzf.diagnostics, desc = '[D]iagnostics' },
      {
        '<leader>fd',
        function()
          fzf.files({ cwd = vim.fn.expand '~/Documents/dotfiles' })
        end,
        desc = '[D]otfiles',
      },
      {
        '<leader>fn',
        function()
          fzf.files({ cwd = vim.fn.stdpath 'config' })
        end,
        desc = '[N]eovim Config',
      },
      { '<leader>fgg', fzf.git_status, desc = '[S]tatus' },
      { '<leader>fgb', fzf.git_branches, desc = '[B]ranches' },
      { '<leader>fgc', fzf.git_commits, desc = '[C]ommits' },
      { '<leader>fgC', fzf.git_bcommits, desc = '[B]ranch Commits' },
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
      defaults = { formatter = 'path.filename_first' },
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
          ['alt-p'] = 'toggle-preview',
          ['pgdn'] = 'preview-page-down',
          ['pgup'] = 'preview-page-up',
          ['alt-j'] = 'preview-down',
          ['alt-k'] = 'preview-up',
          ['shift-down'] = 'preview-page-down',
          ['shift-up'] = 'preview-page-up',
          ['ctrl-q'] = 'select-all+accept',
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
          -- ['--tiebreak'] = 'end',
          -- ['--tiebreak'] = 'chunk',
        },
        winopts = {
          height = 0.55,
          width = 0.65,
          row = 0.52,
          col = 0.47,
        },
        -- preview = { default = false, horizontal = 'right:45%' },
        previewer = false,
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
        branches = {
          winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
          cmd = "git branch --all --color | sed 's#remotes/origin/##g'",
          cmd_add = { 'git', 'checkout', '-b' },
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
      helptags = { winopts = { height = 0.55, width = 1.0, row = 1.0 } },
      grep = {
        winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
        actions = m_keys,
        rg_glob = true,
        glob_flah = '--glob',
        glob_separator = '%s%-%-',
      },
      blines = {
        actions = m_keys,
        no_term_buffers = false,
        winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
      },
      lsp = {
        definitions = {
          jump_to_single_result = true,
        },
        references = {
          ignore_current_line = true,
        },
        symbols = {
          actions = m_keys,
          winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
        },
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
    fzf.register_ui_select()

    vim.lsp.handlers['textDocument/codeAction'] = fzf.lsp_code_actions
    vim.lsp.handlers['textDocument/definition'] = function()
      fzf.lsp_definitions({
        jump_to_single_result = true,
      })
    end
    vim.lsp.handlers['textDocument/declaration'] = function()
      fzf.lsp_declarations({
        jump_to_single_result = true,
      })
    end
    vim.lsp.handlers['textDocument/typeDefinition'] = fzf.lsp_typedefs
    vim.lsp.handlers['textDocument/implementation'] = fzf.lsp_implementations
    vim.lsp.handlers['textDocument/references'] = function()
      fzf.lsp_references({
        ignore_current_line = true,
        includeDeclaration = false,
      })
    end
    vim.lsp.handlers['textDocument/documentSymbol'] = fzf.lsp_document_symbols
    vim.lsp.handlers['workspace/symbol'] = fzf.lsp_workspace_symbols
    vim.lsp.handlers['callHierarchy/incomingCalls'] = fzf.lsp_incoming_calls
    vim.lsp.handlers['callHierarchy/outgoingCalls'] = fzf.lsp_outgoing_calls
  end,
}
