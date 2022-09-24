require('which-key').setup({
  plugins = {
    spelling = {
      enabled = true,
      suggestions = 20,
    },
    presets = {
      operators = false,
      motions = false,
      windows = true,
      nav = true,
      z = true,
      g = true,
    },
  },
  window = {
    border = 'single',
    position = 'bottom',
    margin = { 1, 0, 1, 0 },
    padding = { 2, 2, 2, 2 },
  },
  hidden = { '<silent>', '<cmd>', '<Cmd>', '<CR>', 'call', 'lua', '^:', '^ ', 'require' },
  triggers_blacklist = {
    i = { 'j', 'k' },
    v = { 'j', 'k' },
  },
})

local mappings = {
  ['1'] = { '1gt', 'Goto tab 1' },
  ['2'] = { '2gt', 'Goto tab 2' },
  ['3'] = { '3gt', 'Goto tab 3' },
  ['4'] = { '4gt', 'Goto tab 4' },
  ['5'] = { '5gt', 'Goto tab 5' },
  ['6'] = { '6gt', 'Goto tab 6' },
  ['7'] = { '7gt', 'Goto tab 7' },
  ['8'] = { '8gt', 'Goto tab 8' },
  ['9'] = { '9gt', 'Goto tab 9' },
  -- single
  ['+'] = { '<cmd>vertical resize +4<CR>', 'Increase win size' },
  ['-'] = { '<cmd>vertical resize -4<CR>', 'Decrease win size' },
  ['='] = 'Format Code',
  ['/'] = { '<Plug>(comment_toggle_linewise_current)', 'Toggle Comment' },
  ['v'] = { 'v$', 'Select till end' },

  c = {
    name = 'Change',
    d = { '<cmd>cd %:p:h<cr>:pwd<cr>', 'Directory' },
  },
  b = {
    name = 'Buffers',
    a = { ':badd<space>', 'Add', silent = false },
    d = { '<cmd>bd<cr>', 'Delete' },
    n = { '<cmd>bn<cr>', 'Next' },
    p = { '<cmd>bp<cr>', 'Prev' },
  },

  e = {
    name = 'Edit',
    e = { ':edit <C-r>=expand("%:p:h")<cr>/', 'here', silent = false },
    v = { ':vnew <C-r>=expand("%:p:h")<cr>/', 'in vsplit', silent = false },
    t = { ':tabedit <C-r>=expand("%:p:h")<cr>/', 'in new tab', silent = false },
  },

  g = {
    name = 'Git',
    a = { '<cmd>!git add %:p<CR>', 'stage current' },
    A = { '<cmd>!git add .<CR>', 'stage all' },
    h = {
      name = 'Hunk',
      b = { '<cmd>Gitsigns blame_line<cr>', 'blame line' },
      p = { '<cmd>Gitsigns preview_hunk<cr>', 'preview' },
      r = { '<cmd>Gitsigns reset_buffer<cr>', 'reset buffer' },
      l = { '<cmd>Gitsigns reset_hunk<cr>', 'reset line' },
      -- s ={":Gitsigns preview_hunk" , "stage" },
      -- u ={":Gitsigns preview_hunk" , "undo stage" },
      q = { '<cmd>Gitsigns setqflist<cr>', 'Send Hunks to Qflist' },
    },
    l = {
      name = 'Log',
      a = 'All commits',
      c = 'Buffer commits',
    },
    s = 'status',
    b = 'branches',
  },

  f = {
    name = 'Find',
    c = 'color schemes',
    b = 'buffers',
    f = 'Files',
    n = 'Neovim config',
    d = 'Dotfiles',
    g = 'Git status',
    s = 'Current file',
    S = 'Entire Project',
    h = 'Vim Help',
    H = 'Recent Files',
  },
  s = {
    name = 'Session/Switch',
    s = { '<cmd>SessionManager save_current_session<cr>', 'Save' },
    l = { '<cmd>SessionManager load_session<cr>', 'Load' },
    L = { '<cmd>SessionManager load_last_session<cr>', 'Load Last' },
    c = { '<cmd>SessionManager load_current_dir_session<cr>', 'Load Current dir ' },
    d = { '<cmd>SessionManager delete_session<cr>', 'Delete' },
    t = { ':Switch<cr>', 'Switch(Toggle true/false)' },
    T = { ':SwitchReverse<cr>', 'Switch Reverse(Toggle true/false)' },
  },
  -- n = {
  -- 	name = "Notes",
  -- 	a = "Show Tags",
  -- 	c = "Show Calendar",
  -- 	C = { "CalendarT", "Show Calendar(plugin)" },
  -- 	d = "Find Daily",
  -- 	f = "Find",
  -- 	i = "Insert Link",
  -- 	I = "Insert Link(Img)",
  -- 	n = "Create New",
  -- 	N = "Create Template",
  -- 	p = "Show Panel",
  -- 	r = "Rename",
  -- 	s = "Search",
  -- 	t = "Toggle Todo",
  -- 	T = { ":GenTocMarked<cr>", "Generate TOC" },
  -- 	y = "Yank Note Link",
  -- 	z = "Follow Link",
  -- },
  z = {
    name = 'Spelling',
    z = { '<cmd>setlocal spell!<cr>', 'Toggle Spelling' },
    a = { 'zg', 'Add Spelling' },
    ['='] = { "<cmd>lua require'telescope.builtin'.spell_suggest{}<CR>", 'First correction' },
  },
  t = {
    name = 'Terminal',
    m = { 'toggle' },
    t = { 'tableize' },
  },
}

local wk = require('which-key')
wk.register(mappings, { prefix = '<leader>' })
wk.register({
  s = { [[ :let @/='\<'.expand('<cword>').'\>'<CR>cgn ]], 'Replace word' },
  w = { ':w!<cr>', 'save file' },
  c = 'Neovim config',
  d = 'Dotfiles',
  z = 'Spell suggest',
}, { prefix = ',' })

wk.register({
  [']q'] = { '<cmd>cn<cr>', 'Quickfix Next ' },
  ['[q'] = { '<cmd>cn<cr>', 'Quickfix Prev ' },
  [']z'] = 'TreeSitter swap next',
  ['[z'] = 'TreeSitter swap prev',
  [']]'] = 'Next class start',
  [']['] = 'Next class end',
  ['[['] = 'Prev class start',
  ['[]'] = 'Prev class end',
})

wk.register({
  g = {
    name = 'Lsp',
    s = 'Switch',
    e = 'Line Diagnostics',
    t = 'Goto type definition',
    d = 'Goto definition',
    -- a = {  c = "Code action" },
  },
})
