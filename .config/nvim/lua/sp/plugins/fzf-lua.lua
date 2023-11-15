local opts = { winopts = { preview = { layout = 'vertical', vertical = 'up:40%' } } }

local function fzf_mru(opts)
  local fzf = require 'fzf-lua'
  local hash = require('sp.util').get_hash()
  local cmd =
    -- string.format("command cat <(fre --sorted --store_name %s) <(fd -t f --color never) | awk '!x[$0]++'", hash) -- https://lib.rs/crates/fre
    string.format('command cat <(mru_tracker --store %s --list) <(fd -t f --color never) | my_uniq', hash)

  cmd = string.gsub(cmd, '[\n\r]+', ' ')

  fzf.files({
    cmd = cmd,
    fzf_opts = { ['--tiebreak'] = 'index' },
    actions = {
      ['default'] = function(selected, opts)
        local path = require 'fzf-lua.path'
        local filename = path.entry_to_file(selected[1], opts, opts.force_uri).path
        local cmd = string.format('mru_tracker --store %s --add %s', hash, filename)
        cmd = string.gsub(cmd, '[\n\r]+', ' ')
        vim.fn.jobstart(cmd, {
          stdout_buffered = true,
          on_stderr = function(...)
            vim.print(...)
          end,
        })
        fzf.actions.file_edit(selected, opts)
      end,
      ['ctrl-d'] = {
        fn = function(selected)
          if #selected < 1 then
            return
          end
          local path = require 'fzf-lua.path'
          local filename = path.entry_to_file(selected[1], opts, opts.force_uri).path
          -- vim.fn.system('fre --delete ' .. sel[1] .. ' --store_name ' .. hash)
          local c = string.format('mru_tracker --store %s --delete %s', hash, filename)
          c = c:gsub('[\n\t]+', ' ')
          vim.fn.system(c)
        end,
        reload = true,
      },
    },
  })
end
-- horizontal = 'right:60%',
return {
  'ibhagwan/fzf-lua',
  keys = {
    -- { '<C-p>',       '<cmd>FzfLua files<cr>',                desc = 'Find Files' },
    { '<C-p>', fzf_mru, desc = 'Find Files' },
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
        require('fzf-lua').live_grep_native({
          winopts = { preview = { layout = 'vertical' } },
          multiprocess = true,
        })
      end,
      desc = '[F]ind [s]earch Project',
    },
    {
      '<leader>fs',
      -- '<cmd>FzfLua live_grep_native<cr>',
      function()
        require('fzf-lua').grep_visual({
          winopts = { preview = { layout = 'vertical' } },
          multiprocess = true,
        })
      end,
      desc = '[F]ind [s]earch Visual',
      mode = 'x',
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
    {
      'gr',
      function()
        require('fzf-lua').lsp_references(vim.tbl_extend('keep', opts, {
          ignore_current_line = true,
          multiprocess = true,
          winopts = { preview = { 'builtin' } },
        }))
      end,
      desc = '[G]oto [R]eferences(FzfLua)',
    },
    {
      'gd',
      function()
        require('fzf-lua').lsp_definitions(vim.tbl_extend('keep', opts, {
          jump_to_single_result = true,
          multiprocess = true,
          winopts = { preview = { 'builtin' } },
        }))
      end,
      desc = '[G]oto [D]efinition(FzfLua)',
    },
    {
      'gD',
      function()
        require('fzf-lua').lsp_declarations(vim.tbl_extend('keep', opts, {
          jump_to_single_result = true,
          multiprocess = true,
          winopts = { preview = { 'builtin' } },
        }))
      end,
      desc = '[G]oto [D]eclaration(FzfLua)',
    },
    {
      'gt',
      function()
        require('fzf-lua').lsp_typedefs(vim.tbl_extend('keep', opts, {
          jump_to_single_result = true,
          multiprocess = true,
          winopts = { preview = { 'builtin' } },
        }))
      end,
      desc = '[G]oto [T]ypedef(FzfLua)',
    },
    {
      'gw',
      function()
        require('fzf-lua').lsp_document_symbols(opts)
      end,
      desc = 'Documents Symbols (FzfLua)',
    },
    {
      'gW',
      function()
        require('fzf-lua').lsp_workspace_symbols(opts)
      end,
      desc = 'Workspace Symbols (FzfLua)',
    },
    {
      '<leader>gb',
      function()
        require('fzf-lua').git_branches(vim.tbl_deep_extend('force', opts, {
          multiprocess = true,
        }))
      end,
      desc = '[G]oto [B]ranches(FzfLua)',
    },
    {
      '<leader>gc',
      function()
        require('fzf-lua').git_commits()
      end,
      desc = '[G]it [C]ommits(FzfLua)',
    },
    {
      '<leader>gC',
      function()
        require('fzf-lua').git_bcommits()
      end,
      desc = '[G]it Buffer [C]ommits(FzfLua)',
    },
  },
  config = function()
    local fzf = require 'fzf-lua'
    local actions = require 'fzf-lua.actions'
    local m_keys = {
      ['alt-enter'] = actions.file_tabedit,
      ['ctrl-x'] = actions.file_split,
      ['ctrl-q'] = actions.file_edit_or_qf,
    }
    fzf.setup({
      fzf_opts = {
        ['--info'] = 'hidden',
        --[[ ['--history'] = vim.fn.stdpath("data") .. '/fzf-lua-history',  ]]
      },
      winopts = {
        preview = { default = 'bat_native' },
      },
      keymap = {
        fzf = {
          ['ctrl-j'] = 'down',
          ['ctrl-k'] = 'up',
          ['ctrl-f'] = 'half-page-down',
          ['ctrl-b'] = 'half-page-up',
          ['alt-a'] = 'toggle-all',
          ['shift-down'] = 'preview-page-down',
          ['shift-up'] = 'preview-page-up',
        },
      },
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
        fzf_opts = {
          ['--history'] = vim.fn.stdpath 'data' .. '/fzf-lua-files-history',
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
        actions = vim.tbl_extend('force', m_keys, {
          ['ctrl-d'] = actions.buf_delete,
          ['ctrl-x'] = actions.buf_vsplit,
          ['ctrl-q'] = actions.buf_edit_or_qf,
        }),
      },
      blines = {
        actions = m_keys,
        no_term_buffers = false,
        winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
      },
      lines = { actions = m_keys },
      grep = {
        winopts = { preview = { layout = 'vertical', vertical = 'up:40%' } },
        actions = m_keys,
        fzf_opts = {
          ['--history'] = vim.fn.stdpath 'data' .. '/fzf-lua-grep-history',
        },
      },
    })
    fzf.register_ui_select()
  end,
}
