local map = require('sp.helper').map

map('n', ',w', ':w!<cr>')
map('n', ',W', ':noautocmd w!<cr>')
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
-- map("", "<leader>ss", ":setlocal spell!<cr>")
-- map("", "<leader>sa", "zg")
map('n', '<leader>yp', '"ayy"ap')

-- Pasting in visual mode doesn't copy
map('x', 'p', [[ 'pgv"'.v:register.'y' ]], { noremap = true, expr = true })

map('c', '<C-a>', '<Home>', { silent = false })
map('c', '<C-e>', '<End>', { silent = false })

vim.cmd([[ nnoremap <silent> ,s :let @/='\<'.expand('<cword>').'\>'<CR>cgn ]])
-- map("n", ",s", [[ :let @/='\<'.expand('<cword>').'\>'<CR>cgn ]])
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

-- Plugin mappings
map('', 's', ':HopWord<cr>')
map('n', 'S', ':HopChar2<cr>')
map('n', '<C-e>', ':NvimTreeToggle<cr>')
map('n', '<C-n>', ':NvimTreeFindFileToggle<cr>')
map('n', '<F7>', ':ColorizerToggle<cr>')
map('n', '<leader>gg', ':Neogit<cr>')

map('x', '<leader>/', '<Plug>(comment_toggle_linewise_visual)')
-- Copy and Comment
map('n', '<A-/>', '"ayy"apk<Plug>(comment_toggle_linewise_current)j')

map('o', 'ie', ':exec "normal! ggVG"<cr>', { noremap = true, silent = true })
map('o', 'iv', ':exec "normal! HVL"<cr>', { noremap = true, silent = true })
