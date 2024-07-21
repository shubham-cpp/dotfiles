local fzf = require 'fzf-lua'
local actions = require 'fzf-lua.actions'
local m_keys = {
  ['alt-enter'] = actions.file_tabedit,
  ['ctrl-x'] = actions.file_split,
  ['ctrl-q'] = actions.file_edit_or_qf,
}
-- calling `setup` is optional for customization
fzf.setup({
  defaults = { formatter = { 'path.filename_first', 2 } },
  fzf_opts = {
    ['--layout'] = 'reverse',
    ['--info'] = 'inline-right',
    -- ['--tiebreak'] = 'end',
  },
  winopts = {
    border = { '┏', '━', '┓', '┃', '┛', '━', '┗', '┃' },
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
      ['--tiebreak'] = 'chunk',
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
fzf.register_ui_select()

vim.lsp.handlers['textDocument/codeAction'] = fzf.lsp_code_actions
vim.lsp.handlers['textDocument/references'] = fzf.lsp_references

vim.lsp.handlers['textDocument/definition'] = fzf.lsp_definitions
vim.lsp.handlers['textDocument/declaration'] = fzf.lsp_declarations
vim.lsp.handlers['textDocument/typeDefinition'] = fzf.lsp_typedefs
vim.lsp.handlers['textDocument/implementation'] = fzf.lsp_implementations

vim.lsp.handlers['textDocument/documentSymbol'] = fzf.lsp_document_symbols
vim.lsp.handlers['workspace/symbol'] = fzf.lsp_workspace_symbols

vim.lsp.handlers['callHierarchy/incomingCalls'] = fzf.lsp_incoming_calls
vim.lsp.handlers['callHierarchy/outgoingCalls'] = fzf.lsp_outgoing_calls
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
      ['default'] = function(selected)
        local fullpath = get_full_path(selected)
        vim.ui.input({ prompt = 'File Name: ' }, function(name)
          if name == nil then
            return
          end
          vim.cmd('e ' .. fullpath .. name)
          vim.cmd 'w ++p'
        end)
      end,
    },
  })
end

local function project_files(default_opts)
  local opts = vim.tbl_extend('force', {}, default_opts or {})
  local fzf = require 'fzf-lua'
  if vim.b.gitsigns_head then
    fzf.git_files(opts)
  else
    fzf.files(opts)
  end
end

local keys = {
  ['<C-p>'] = { project_files, { desc = 'Git [F]iles' } },
  ['<leader>ff'] = { fzf.files, { desc = '[F]iles' } },
  ['<leader>fr'] = { fzf.resume, { desc = '[R]esume' } },
  ['<leader>fs'] = { fzf.live_grep_native, { desc = '[S]earch(Project)' } },
  ['<leader>fc'] = { fzf_create_file, { desc = 'Create File' } },
  ['<leader>fS'] = {
    function()
      fzf.grep_curbuf({ winopts = { preview = { layout = 'vertical', { vertical = 'up:60%' } } } })
    end,
    { desc = '[S]earch Current Buffer' },
  },
  ['<leader>fw'] = { fzf.grep_cWORD, { desc = '[W]ord under cursor' } },
  ['<leader>fo'] = {
    function()
      fzf.oldfiles({ path_shorten = true })
    end,
    { desc = '[O]ld Files' },
  },
  ['<leader>fb'] = { fzf.buffers, { desc = '[B]uffers' } },
  ['<leader>fz'] = { fzf.spell_suggest, { desc = '[S]pelling' } },
  ['<leader>fk'] = { fzf.keymaps, { desc = '[K]eymaps' } },
  ['<leader>fh'] = { fzf.help_tags, { desc = '[H]elp Tags' } },
  ['<leader>fD'] = { fzf.diagnostics_document, { desc = '[D]iagnostics' } },
  ['<leader>fd'] = {
    function()
      fzf.files({ cwd = vim.fn.expand '~/Documents/dotfiles' })
    end,
    { desc = '[D]otfiles' },
  },
  ['<leader>fn'] = {
    function()
      fzf.files({ cwd = vim.fn.stdpath 'config' })
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
for key, value in pairs(keys) do
  if value[1] == nil then
    vim.print(key .. ' is nil')
    goto continue
  end
  vim.keymap.set('n', key, value[1], vim.tbl_extend('force', { noremap = true, silent = true }, value[2] or {}))
  ::continue::
end
