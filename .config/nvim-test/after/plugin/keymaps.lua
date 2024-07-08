vim.keymap.set('', '0', '^', { noremap = false, silent = true })
vim.keymap.set('n', ',w', '<cmd>w!<cr>', { desc = 'Save File' })
vim.keymap.set('n', ',W', '<cmd>noautocmd w!<cr>', { desc = 'Save File(without autocmds)' })
vim.keymap.set('n', '<Esc>', ':nohl<cr>')

vim.keymap.set('n', '<M-j>', 'mz:m+<cr>`z')
vim.keymap.set('n', '<M-k>', 'mz:m-2<cr>`z')
vim.keymap.set('n', '<S-Left>', ':tabp<cr>')
vim.keymap.set('n', '<S-Right>', ':tabn<cr>')

-- vim.keymap.set("n", "<C-h>", "<C-W>h")
-- vim.keymap.set("n", "<C-j>", "<C-W>j")
-- vim.keymap.set("n", "<C-k>", "<C-W>k")
-- vim.keymap.set("n", "<C-l>", "<C-W>l")

vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Pasting in visual mode doesn't copy
vim.keymap.set('x', 'p', [[ 'pgv"'.v:register.'y' ]], { expr = true })
vim.keymap.set('n', 'dl', '"_dl')
vim.keymap.set({ 'n', 'x' }, 'c', '"_c')
vim.keymap.set('n', 'C', '"_C')

vim.keymap.set('c', '<C-a>', '<Home>', { silent = false })
vim.keymap.set('c', '<C-e>', '<End>', { silent = false })

vim.cmd [[ nnoremap <silent> ,s :let @/='\<'.expand('<cword>').'\>'<CR>cgn ]]
vim.keymap.set('x', ',s', '"sy:let @/=@s<CR>cgn')

-- vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv")
-- vim.keymap.set('v', 'K', ":m '<-2<CR>gv=gv")
-- vim.keymap.set('x', '<', '<gv')
-- vim.keymap.set('x', '>', '>gv')

vim.keymap.set('i', ',', ',<C-g>u')
vim.keymap.set('i', '.', '.<C-g>u')
vim.keymap.set('i', '?', '?<C-g>u')

vim.keymap.set('x', '<leader>y', '"+y')
vim.keymap.set('x', '<leader>p', '"+p')

vim.keymap.set('o', 'ie', ':exec "normal! ggVG"<cr>')
vim.keymap.set('o', 'iv', ':exec "normal! HVL"<cr>')
vim.keymap.set('n', ',e', ':e <C-R>=expand("%:p:h") . "/" <CR>', { silent = false, desc = 'Edit in same dir' })
vim.keymap.set('n', ',t', ':tabe <C-R>=expand("%:p:h") . "/" <CR>', { silent = false, desc = 'Edit in same dir(Tab)' })
vim.keymap.set(
  'n',
  ',v',
  ':vsplit <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = 'Edit in same dir(Split)' }
)

vim.keymap.set('n', '<leader>x', function()
  vim.cmd 'source %'
  local file = vim.fn.substitute(vim.fn.expand '%:r', 'lua/', '', '')
  local ok, mod = pcall(require, file)
  if ok and type(mod) ~= 'boolean' and next(mod or {}) ~= nil and mod.config then
    mod.config()
  end
end, { silent = false, desc = 'Reload module' })
