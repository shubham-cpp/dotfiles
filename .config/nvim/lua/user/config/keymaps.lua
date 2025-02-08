vim.keymap.set('', '0', '^', { noremap = false, silent = true })
vim.keymap.set('n', ',w', '<cmd>w!<cr>', { desc = 'Save File' })
vim.keymap.set('n', ',W', '<cmd>noautocmd w!<cr>', { desc = 'Save File(without autocmds)' })
vim.keymap.set('n', '<Esc>', function()
  vim.cmd 'noh'
  return '<esc>'
end, { expr = true, silent = true, desc = 'Escape and Clear hlsearch' })

vim.keymap.set('n', '<C-h>', '<C-W>h')
vim.keymap.set('n', '<C-j>', '<C-W>j')
vim.keymap.set('n', '<C-k>', '<C-W>k')
vim.keymap.set('n', '<C-l>', '<C-W>l')

-- https://github.com/mhinz/vim-galore#saner-behavior-of-n-and-n
vim.keymap.set('n', 'n', "'Nn'[v:searchforward].'zv'", { expr = true, desc = 'Next Search Result' })
vim.keymap.set('x', 'n', "'Nn'[v:searchforward]", { expr = true, desc = 'Next Search Result' })
vim.keymap.set('o', 'n', "'Nn'[v:searchforward]", { expr = true, desc = 'Next Search Result' })
vim.keymap.set('n', 'N', "'nN'[v:searchforward].'zv'", { expr = true, desc = 'Prev Search Result' })
vim.keymap.set('x', 'N', "'nN'[v:searchforward]", { expr = true, desc = 'Prev Search Result' })
vim.keymap.set('o', 'N', "'nN'[v:searchforward]", { expr = true, desc = 'Prev Search Result' })

-- Add undo break-points
vim.keymap.set('i', ',', ',<c-g>u')
vim.keymap.set('i', '.', '.<c-g>u')
vim.keymap.set('i', ';', ';<c-g>u')

-- commenting
vim.keymap.set('n', 'gco', 'o<esc>Vcx<esc><cmd>normal gcc<cr>fxa<bs>', { desc = 'Add Comment Below' })
vim.keymap.set('n', 'gcO', 'O<esc>Vcx<esc><cmd>normal gcc<cr>fxa<bs>', { desc = 'Add Comment Above' })

vim.keymap.set('n', '[q', vim.cmd.cprev, { desc = 'Previous Quickfix' })
vim.keymap.set('n', ']q', vim.cmd.cnext, { desc = 'Next Quickfix' })
vim.keymap.set('n', '[l', '<cmd>lprev<cr>', { desc = 'Prev LocalFix' })
vim.keymap.set('n', ']l', '<cmd>lnext<cr>', { desc = 'Next LocalFix' })
vim.keymap.set('n', '[t', '<cmd>tabprev<cr>', { desc = 'Prev Tab' })
vim.keymap.set('n', ']t', '<cmd>tabnext<cr>', { desc = 'Next Tab' })
vim.keymap.set('n', '[b', '<cmd>bprevious<cr>', { desc = 'Prev Buffer' })
vim.keymap.set('n', ']b', '<cmd>bnext<cr>', { desc = 'Next Buffer' })

-- Pasting in visual mode doesn't copy
vim.keymap.set('x', 'p', [[ 'pgv"'.v:register.'y' ]], { expr = true })
vim.keymap.set('n', 'dl', '"_dl')
vim.keymap.set({ 'n', 'x' }, 'c', '"_c')
vim.keymap.set('n', 'C', '"_C')

vim.keymap.set('c', '<C-a>', '<Home>', { silent = false })
vim.keymap.set('c', '<C-e>', '<End>', { silent = false })

vim.keymap.set(
  'n',
  '<localleader>e',
  ':e <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = 'Edit in same dir' }
)
vim.keymap.set(
  'n',
  '<localleader>t',
  ':tabe <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = 'Edit in same dir(Tab)' }
)
vim.keymap.set(
  'n',
  '<localleader>v',
  ':vsplit <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = 'Edit in same dir(Split)' }
)

vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

for i = 1, 9 do
  vim.keymap.set('n', '<leader>' .. i, i .. 'gt', { desc = 'Goto tab ' .. i })
end

vim.keymap.set('n', '<leader>R', function()
  vim.cmd 'source %'
  local file = vim.fn.substitute(vim.fn.expand '%:r', 'lua/', '', '')
  local ok, mod = pcall(require, file)
  if ok and type(mod) ~= 'boolean' and next(mod or {}) ~= nil and mod.config then
    mod.config()
  end
end, { silent = false, desc = 'Reload module' })
