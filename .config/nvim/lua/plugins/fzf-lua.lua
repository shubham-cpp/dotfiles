return {
  'ibhagwan/fzf-lua',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  cmd = 'FzfLua',
  enabled = true,
  keys = {
    '<C-p>',
    '<leader>ff',
    '<leader>fr',
    '<leader>fs',
    '<leader>fc',
    '<leader>fS',

    { '<leader>fw', mode = { 'n', 'v' } },
    '<leader>fW',
    '<leader>fo',
    '<leader>fb',
    '<leader>fz',
    '<leader>fk',
    '<leader>fh',
    '<leader>fD',
    '<leader>fd',

    '<leader>fn',

    '<leader>fg',
    '<leader>gt',
    '<leader>gS',
    '<leader>gb',
    '<leader>gc',
    '<leader>gC',

    { '<C-x><C-k>', mode = 'i', desc = 'Complete bline' },
    { '<C-x><C-l>', mode = 'i', desc = 'Complete line' },
    { '<C-x><C-f>', mode = 'i', desc = 'Complete path' },
  },
  config = function()
    local fzf = require 'fzf-lua'
    local actions = require 'fzf-lua.actions'
    local utils = require 'fzf-lua.utils'

    local function hl_validate(hl)
      return not utils.is_hl_cleared(hl) and hl or nil
    end

    local rg_cmd =
      'rg --files -l ".*" --follow --color=never --sortr=modified -g "!.git/" -g "!*.png"  -g "!node_modules/" -g "!*.jpeg" -g "!*.jpg" -g "!*.ico" -g "!*.exe" -g "!*.out"'

    local m_keys = {
      ['alt-enter'] = actions.file_tabedit,
      ['ctrl-t'] = actions.file_tabedit,
      ['ctrl-x'] = actions.file_split,
      -- ['ctrl-q'] = actions.file_edit_or_qf,
    }
    -- calling `setup` is optional for customization
    fzf.setup({
      defaults = { formatter = { 'path.filename_first', 2 } },
      fzf_opts = {
        ['--layout'] = 'reverse',
        ['--info'] = 'inline-right',
        -- ['--tiebreak'] = 'end',
      },
      winopts = { preview = { default = 'bat' } },
      hls = {
        normal = hl_validate 'TelescopeNormal',
        border = hl_validate 'TelescopeBorder',
        title = hl_validate 'TelescopePromptTitle',
        help_normal = hl_validate 'TelescopeNormal',
        help_border = hl_validate 'TelescopeBorder',
        preview_normal = hl_validate 'TelescopeNormal',
        preview_border = hl_validate 'TelescopeBorder',
        preview_title = hl_validate 'TelescopePreviewTitle',
        -- builtin preview only
        cursor = hl_validate 'Cursor',
        cursorline = hl_validate 'TelescopeSelection',
        cursorlinenr = hl_validate 'TelescopeSelection',
        search = hl_validate 'IncSearch',
      },
      keymap = {
        builtin = {
          true,
          ['<esc>'] = 'hide',
        },
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
          ['--tiebreak'] = 'length',
        },
        winopts = {
          height = 0.55,
          width = 0.65,
          row = 0.52,
          col = 0.47,
          preview = {
            ---@type 'wrap'|'nowrap'
            wrap = 'nowrap',
            ---@type 'hidden'|'nohidden'
            hidden = 'hidden',
          },
        },
        actions = m_keys,
      },
      git = {
        files = {
          cmd = 'git ls-files --exclude-standard --cached --others', -- '--others' is used to show untracked files
          actions = m_keys,
          winopts = {
            height = 0.55,
            width = 0.65,
            row = 0.52,
            col = 0.47,
          },
          previewer = false,
        },
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
      keymaps = { winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } } },
      helptags = { winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } } },
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
          actions = m_keys,
        },
        declarations = {
          jump_to_single_result = true,
          actions = m_keys,
        },
        references = {
          ignore_current_line = true,
          actions = m_keys,
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
    local ok_dressing, _ = pcall(require, 'dressing')
    if not ok_dressing then
      fzf.register_ui_select()
    end

    local function fzf_create_file()
      local fzf = require 'fzf-lua'
      local path = require 'fzf-lua.path'
      local uv = vim.uv or vim.loop
      local cmd = 'fd -t d . ' .. uv.cwd()
      local function get_full_path(selected)
        if #selected < 1 then
          return
        end
        local entry = path.entry_to_file(selected[1], { cwd = uv.cwd() })
        if entry.path == '<none>' then
          return
        end

        local fullpath = entry.path or entry.uri and entry.uri:match '^%a+://(.*)'
        if not path.is_absolute(fullpath) then
          fullpath = path.join({ uv.cwd(), fullpath })
        end
        return fullpath
      end

      --- This will create a file in the selected directory
      ---@param split_dir ?"e" | "vs" | "sp" | "tabe" Default is "e"
      ---@return function
      local function perform_action(split_dir)
        split_dir = split_dir or 'e'
        return function(selected)
          local fullpath = get_full_path(selected)
          vim.ui.input({ prompt = 'File Name: ' }, function(name)
            if name == nil then
              return
            end
            vim.cmd(string.format('%s %s%s', split_dir, fullpath, name))
            vim.cmd 'w ++p'
          end)
        end
      end

      fzf.fzf_exec(cmd, {
        defaults = {},
        prompt = 'Create> ',
        cwd = uv.cwd(),
        cwd_prompt_shorten_len = 32,
        cwd_prompt_shorten_val = 1,
        fzf_opts = {
          ['--tiebreak'] = 'end',
          ['--preview-window'] = 'nohidden,50%',
          ['--preview'] = {
            type = 'cmd',
            fn = function(selected)
              local fullpath = get_full_path(selected)
              local ls_cmd = 'command ls --color -hsv1F --group-directories-first'
              local eza_cmd =
                'eza -al --color=always --icons=always --group-directories-first --no-user --no-permissions --no-time'
              return string.format('%s %s', vim.fn.executable 'eza' == 1 and eza_cmd or ls_cmd, fullpath)
            end,
          },
        },
        fn_transform = function(x)
          return fzf.make_entry.file(x, { file_icons = true, color_icons = true, cwd = uv.cwd() })
        end,
        actions = {
          ['default'] = perform_action(),
          ['ctrl-x'] = perform_action 'sp',
          ['ctrl-v'] = perform_action 'vs',
          ['ctrl-t'] = perform_action 'tabe',
        },
      })
    end

    local function project_files(default_opts)
      local opts = vim.tbl_extend('force', {
        fzf_opts = {
          ['--layout'] = 'reverse',
          ['--info'] = 'inline-right',
          ['--tiebreak'] = 'index',
        },
      }, default_opts or {})
      local fzf = require 'fzf-lua'
      if vim.b.gitsigns_head then
        -- Either use one of the following .local/bin/myscripts/sort_file.rs or .local/bin/myscripts/sorting_filev3.cpp
        -- compile and then add to `PATH`
        --`sort_files` is a program that sorts files based on modified time, recently modified files will be shown first
        if vim.fn.executable 'sort_files' == 1 then
          opts.cmd = 'git ls-files --exclude-standard --cached --others | sort_files' -- '--others' is used to show untracked files
        else
          vim.cmd 'echohl WarningMsg | echo "`sort_files` not found in `PATH`. Please compile the program" | echohl None'
        end
        fzf.git_files(opts)
      else
        opts.cmd = rg_cmd
        fzf.files(opts)
      end
    end

    local keys = {
      ['<C-p>'] = { project_files, { desc = 'Git [F]iles' } },
      ['<leader>ff'] = {
        function()
          fzf.files({
            cmd = rg_cmd,
            fzf_opts = {
              ['--layout'] = 'reverse',
              ['--info'] = 'inline-right',
              ['--tiebreak'] = 'index',
            },
          })
        end,
        { desc = '[F]iles' },
      },
      ['<leader>fr'] = { fzf.resume, { desc = '[R]esume' } },
      ['<leader>fs'] = { fzf.live_grep_native, { desc = '[S]earch(Project)' } },
      ['<leader>fc'] = { fzf_create_file, { desc = 'Create File' } },
      ['<leader>fS'] = {
        function()
          fzf.grep_curbuf({ winopts = { preview = { layout = 'vertical', { vertical = 'up:60%' } } } })
        end,
        { desc = '[S]earch Current Buffer' },
      },
      ['<leader>fw'] = { fzf.grep_cword, { desc = '[W]ord under cursor' } },
      ['<leader>fW'] = { fzf.grep_cWORD, { desc = '[W]ord under cursor' } },
      ['<leader>fo'] = {
        function()
          fzf.oldfiles({ path_shorten = true })
        end,
        { desc = '[O]ld Files' },
      },
      ['<leader>f/'] = {
        function()
          fzf.lgrep_curbuf({
            prompt = 'Buffer❫ ',
          })
        end,
        { desc = 'Grep buffer' },
      },
      ['<leader>*'] = {
        function()
          fzf.grep_curbuf({
            prompt = 'Buffer❫ ',
            search = vim.fn.expand '<cword>',
          })
        end,
        { desc = 'Grep buffer with cword' },
      },
      ['<leader>fb'] = { fzf.buffers, { desc = '[B]uffers' } },
      ['<leader>fz'] = { fzf.spell_suggest, { desc = '[S]pelling' } },
      ['<leader>fk'] = { fzf.keymaps, { desc = '[K]eymaps' } },
      ['<leader>fh'] = { fzf.help_tags, { desc = '[H]elp Tags' } },
      ['<leader>fD'] = { fzf.diagnostics_document, { desc = '[D]iagnostics' } },
      ['<leader>fd'] = {
        function()
          fzf.files({
            cwd = vim.fn.expand '~/Documents/dotfiles',
            cmd = rg_cmd .. ' --hidden',
            fzf_opts = {
              ['--layout'] = 'reverse',
              ['--info'] = 'inline-right',
              ['--tiebreak'] = 'index',
            },
          })
        end,
        { desc = '[D]otfiles' },
      },
      ['<leader>fn'] = {
        function()
          fzf.files({
            cwd = vim.fn.stdpath 'config',
            cmd = rg_cmd,
            fzf_opts = {
              ['--layout'] = 'reverse',
              ['--info'] = 'inline-right',
              ['--tiebreak'] = 'index',
            },
          })
        end,
        { desc = '[N]eovim Config' },
      },
      ['<leader>fg'] = { fzf.git_status, { desc = '[S]tatus' } },
      ['<leader>gt'] = { fzf.git_status, { desc = '[S]tatus' } },
      ['<leader>gS'] = { fzf.git_stash, { desc = 'Stash' } },
      ['<leader>gb'] = { fzf.git_branches, { desc = '[B]ranches' } },
      ['<leader>gc'] = { fzf.git_commits, { desc = '[C]ommits' } },
      ['<leader>gC'] = { fzf.git_bcommits, { desc = '[B]ranch Commits' } },
    }

    vim.keymap.set('v', '<leader>fw', fzf.grep_visual, { desc = 'Selection' })
    vim.keymap.set('i', '<C-x><C-k>', fzf.complete_bline, { desc = 'Complete bline', silent = true, noremap = true })
    vim.keymap.set('i', '<C-x><C-l>', fzf.complete_line, { desc = 'Complete line', silent = true, noremap = true })
    vim.keymap.set('i', '<C-x><C-f>', fzf.complete_path, { desc = 'Complete path', silent = true, noremap = true })
    for key, value in pairs(keys) do
      if value[1] == nil then
        vim.print(key .. ' is nil')
        goto continue
      end
      vim.keymap.set('n', key, value[1], vim.tbl_extend('force', { noremap = true, silent = true }, value[2] or {}))
      ::continue::
    end
  end,
}
