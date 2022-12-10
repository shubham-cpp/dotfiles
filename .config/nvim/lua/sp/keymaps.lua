local map = require('sp.helper').map

map('', '0', '^', { noremap = false, silent = true })
map('n', '<Esc>', ':nohl<cr>')
map('n', '*', '*zz')
map('n', '#', '#zz')
map('n', 'n', 'nzz')
map('n', 'N', 'Nzz')

map('n', '<M-j>', 'mz:m+<cr>`z')
map('n', '<M-k>', 'mz:m-2<cr>`z')
map('n', '<S-Left>', ':tabp<cr>')
map('n', '<S-Right>', ':tabn<cr>')
map('n', '<M-h>', ':tabp<cr>')
map('n', '<M-l>', ':tabn<cr>')

map('n', '<C-h>', '<C-W>h')
map('n', '<C-j>', '<C-W>j')
map('n', '<C-k>', '<C-W>k')
map('n', '<C-l>', '<C-W>l')

map('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
map('n', 'j', "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

-- Pasting in visual mode doesn't copy
map('x', 'p', [[ 'pgv"'.v:register.'y' ]], { noremap = true, expr = true })
map('n', 'dl', '"_dl')
map('n', 'c', '"_c')
map('n', 'C', '"_C')
map('x', 'c', '"_c')

map('c', '<C-a>', '<Home>', { silent = false })
map('c', '<C-e>', '<End>', { silent = false })

vim.cmd [[ nnoremap <silent> ,s :let @/='\<'.expand('<cword>').'\>'<CR>cgn ]]
map('x', ',s', '"sy:let @/=@s<CR>cgn')

map('v', 'J', ":m '>+1<CR>gv=gv")
map('v', 'K', ":m '<-2<CR>gv=gv")
map('x', '<', '<gv')
map('x', '>', '>gv')

map('i', ',', ',<C-g>u')
map('i', '.', '.<C-g>u')
map('i', '?', '?<C-g>u')

map('n', '<M-->', ':exe "vertical resize -10"<CR>')
map('n', '<M-=>', ':exe "vertical resize +10"<CR>')

map('x', '<leader>y', '"+y')
map('x', '<leader>p', '"+p')

-- Plugin mappings
map('n', '<F7>', ':ColorizerToggle<cr>')

map('o', 'ie', ':exec "normal! ggVG"<cr>', { noremap = true, silent = true })
map('o', 'iv', ':exec "normal! HVL"<cr>', { noremap = true, silent = true })
