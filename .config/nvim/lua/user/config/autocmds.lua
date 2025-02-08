local function augroup(name)
  return vim.api.nvim_create_augroup('sp_' .. name, { clear = true })
end

local au_buffer = augroup 'buffer'
local au_term = augroup 'term'
local au_ft = augroup 'fts'

vim.api.nvim_create_autocmd('CmdlineEnter', {
  group = au_buffer,
  command = 'set nosmartcase',
})
vim.api.nvim_create_autocmd('CmdlineLeave', {
  group = au_buffer,
  command = 'set smartcase',
})

vim.api.nvim_create_autocmd('TermOpen', {
  group = au_term,
  desc = 'set some mappings for terminal',
  pattern = 'term://*',
  callback = function()
    vim.keymap.set('t', '<C-]>', '<C-\\><C-n>', { buffer = true })
    vim.keymap.set('t', '<A-w>', '<C-\\><C-n><C-w>', { buffer = true })

    vim.keymap.set('n', 'A', 'A<C-k>', { buffer = true })
    vim.keymap.set('n', 'D', 'A<C-k><C-\\><C-n>', { buffer = true })
    vim.keymap.set('n', 'cc', 'A<C-e><C-u>', { buffer = true })
    vim.keymap.set('n', 'dd', 'A<C-e><C-u><C-\\><C-n>', { buffer = true })

    vim.opt_local.signcolumn = 'no'
    vim.opt_local.relativenumber = false
    vim.opt_local.number = false
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  group = au_term,
  pattern = 'toggleterm',
  desc = 'fix: to disable foldexpr for terminal to fix this issue. Takes too much time to open terminal.',
  callback = function()
    vim.opt_local.foldexpr = ''
    vim.opt_local.foldmethod = 'manual'
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  group = au_ft,
  pattern = 'lazy',
  callback = function()
    vim.opt_local.signcolumn = 'no'
    vim.opt_local.relativenumber = false
    vim.opt_local.number = false
  end,
})

vim.api.nvim_create_autocmd({ 'FocusGained', 'TermClose', 'TermLeave' }, {
  group = au_ft,
  command = 'checktime',
  desc = 'Check if the file was changed',
})

vim.api.nvim_create_autocmd({ 'VimResized' }, {
  group = au_ft,
  desc = 'resize splits if window got resized',
  callback = function()
    local current_tab = vim.fn.tabpagenr()
    vim.cmd 'tabdo wincmd ='
    vim.cmd('tabnext ' .. current_tab)
  end,
})

vim.api.nvim_create_autocmd('BufReadPost', {
  group = au_buffer,
  desc = 'go to last loc when opening a buffer',
  callback = function(event)
    local exclude = { 'gitcommit' }
    local buf = event.buf
    if vim.tbl_contains(exclude, vim.bo[buf].filetype) or vim.b[buf].lazyvim_last_loc then
      return
    end
    vim.b[buf].lazyvim_last_loc = true
    local mark = vim.api.nvim_buf_get_mark(buf, '"')
    local lcount = vim.api.nvim_buf_line_count(buf)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

vim.api.nvim_create_autocmd('TextYankPost', {
  group = au_buffer,
  callback = function()
    vim.highlight.on_yank({ higroup = 'Visual', timeout = 80 })
  end,
  desc = 'Highlight on yank',
})

vim.api.nvim_create_autocmd('FileType', {
  group = au_buffer,
  desc = 'close some filetypes with <q>',
  pattern = {
    'PlenaryTestPopup',
    'checkhealth',
    'dbout',
    'gitsigns-blame',
    'grug-far',
    'help',
    'lspinfo',
    'neotest-output',
    'neotest-output-panel',
    'neotest-summary',
    'notify',
    'qf',
    'spectre_panel',
    'startuptime',
    'tsplayground',
  },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.schedule(function()
      vim.keymap.set('n', 'q', function()
        vim.cmd 'close'
        pcall(vim.api.nvim_buf_delete, event.buf, { force = true })
      end, {
        buffer = event.buf,
        silent = true,
        desc = 'Quit buffer',
      })
    end)
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  group = au_ft,
  desc = 'make it easier to close man-files when opened inline',
  pattern = { 'man' },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  group = augroup 'webdev',
  desc = 'Some extra settings',
  pattern = {
    'typescriptreact',
    'typescript',
    'typescript.tsx',
    'typescript.jsx',
    'javascriptreact',
    'javascript',
    'javascript.jsx',
  },
  callback = function()
    vim.opt_local.path:append '$PWD/node_modules'
    vim.opt_local.suffixesadd:append({ '.js', '.jsx', '.tsx' })
    vim.opt_local.cinoptions:append({ 'j1', 'J1' })
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  group = augroup 'webdev',
  desc = 'Set spaces to 2',
  pattern = {
    'lua',
    'jsonc',
    'json',
    'yaml',
    'json5',
    'typescriptreact',
    'typescript',
    'typescript.tsx',
    'typescript.jsx',
    'javascriptreact',
    'javascript',
    'javascript.jsx',
    'svelte',
    'vue',
    'html',
    'css',
    'less',
    'scss',
    'php',
    'blade',
    'heex',
  },
  callback = function()
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.expandtab = true
    vim.opt_local.smartindent = true
    vim.opt_local.autoindent = true
    vim.opt_local.smarttab = true
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  group = au_ft,
  desc = 'Fix Comment Continuation',
  callback = function()
    vim.opt_local.formatoptions = 'jcrqlnt'
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  group = au_ft,
  desc = 'wrap and check for spell in text filetypes',
  pattern = { 'text', 'plaintex', 'typst', 'gitcommit', 'markdown' },
  callback = function()
    vim.opt_local.wrap = true
    vim.opt_local.spell = true
  end,
})

vim.api.nvim_create_autocmd({ 'FileType' }, {
  group = au_ft,
  desc = 'Fix conceallevel for json files',
  pattern = { 'json', 'jsonc', 'json5' },
  callback = function()
    vim.opt_local.conceallevel = 0
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  pattern = 'hyprlang',
  group = au_ft,
  callback = function(event)
    local bufnr = event.buf
    vim.bo[bufnr].commentstring = '# %s'
    vim.opt_local.formatoptions = 'jcrqlnt'
  end,
})
vim.filetype.add({
  extension = {
    fish = 'fish',
    ocaml = 'ocaml',
    rasi = 'rasi',
    roc = 'roc',
  },
  filename = {
    vimfrc = 'vim',
    dwm_sxhkdrc = 'sxhkdrc',
    ['.env'] = 'conf',
    ['.env.*'] = 'conf',
    ['package.json'] = 'jsonc',
  },
  pattern = {
    ['*profile'] = 'sh',
    ['*.postcss'] = 'css',
    ['*.kbd'] = 'lisp',
    ['.eslintrc'] = 'jsonc',
    ['tsconfig.*.json'] = 'jsonc',
    ['.*/waybar/config'] = 'jsonc',
    ['.*/kitty/.+%.conf'] = 'kitty',
    ['.*/hypr/.+%.conf'] = 'hyprlang',
  },
})
vim.treesitter.language.register('bash', 'kitty')
