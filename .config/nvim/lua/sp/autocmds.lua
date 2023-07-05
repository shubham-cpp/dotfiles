local function augroup(name)
  return vim.api.nvim_create_augroup('sp_' .. name, { clear = true })
end

local au_buffer = augroup 'buffer'
local au_term = augroup 'term'

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
  command = 'startinsert',
  pattern = 'term://*',
  desc = 'Start with insert mode in terminal',
})

vim.api.nvim_create_autocmd('TermOpen', {
  group = au_term,
  callback = function()
    vim.keymap.set('t', '<C-]>', '<C-\\><C-n>', { buffer = true })
    vim.keymap.set('n', 'A', 'A<C-k>', { buffer = true })
    vim.keymap.set('n', 'D', 'A<C-k><C-\\><C-n>', { buffer = true })
    vim.keymap.set('n', 'cc', 'A<C-e><C-u>', { buffer = true })
    vim.keymap.set('n', 'cc', 'A<C-e><C-u>', { buffer = true })
    vim.keymap.set('n', 'dd', 'A<C-e><C-u><C-><C-n>', { buffer = true })
    vim.opt_local.signcolumn = 'no'
    vim.opt_local.relativenumber = false
    vim.opt_local.number = false
  end,
})

vim.api.nvim_create_autocmd({ 'FocusGained', 'TermClose', 'TermLeave' }, {
  group = augroup 'checktime',
  command = 'checktime',
  desc = 'Check if the file was changed',
})

vim.api.nvim_create_autocmd('TextYankPost', {
  group = augroup 'highlight_yank',
  callback = function()
    vim.highlight.on_yank()
  end,
  desc = 'Highlight on yank',
})

-- vim.api.nvim_create_autocmd({ 'VimResized' }, {
--   group = augroup 'resize_splits',
--   callback = function()
--     vim.cmd 'tabdo wincmd ='
--   end,
--   desc = 'Resize splits if window got resized',
-- })

-- vim.api.nvim_create_autocmd('BufReadPost', {
--   group = augroup 'last_loc',
--   callback = function()
--     local mark = vim.api.nvim_buf_get_mark(0, '"')
--     local lcount = vim.api.nvim_buf_line_count(0)
--     if mark[1] > 0 and mark[1] <= lcount then pcall(vim.api.nvim_win_set_cursor, 0, mark) end
--   end,
--   desc = 'Go to last loc when opening a buffer',
-- })

vim.api.nvim_create_autocmd('FileType', {
  group = augroup 'close_with_q',
  pattern = {
    'qf',
    'help',
    'man',
    'notify',
    'lspinfo',
    'spectre_panel',
    'startuptime',
    'tsplayground',
    'PlenaryTestPopup',
  },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set('n', 'q', '<cmd>close<cr>', { buffer = event.buf, silent = true })
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  group = augroup 'webdev',
  pattern = {
    'jsonc',
    'json',
    'json5',
    'typescriptreact',
    'typescript',
    'typescript.tsx',
    'typescript.jsx',
    'javascriptreact',
    'javascript',
    'javascript.jsx',
    'vue',
    'html',
    'css',
    'less',
    'scss',
  },
  callback = function(event)
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.expandtab = true
    vim.opt_local.smartindent = true
    vim.opt_local.autoindent = true
    vim.opt_local.smarttab = true
  end,
})

-- wrap and check for spell in text filetypes
vim.api.nvim_create_autocmd('FileType', {
  group = augroup 'wrap_spell',
  pattern = { 'gitcommit', 'markdown' },
  callback = function()
    vim.opt_local.wrap = true
    vim.opt_local.spell = true
  end,
})

vim.api.nvim_create_autocmd({ 'BufAdd', 'BufEnter', 'TabNewEntered' }, {
  desc = 'Update buffers when adding new buffers',
  group = au_buffer,
  callback = function(args)
    if not vim.t.bufs then
      vim.t.bufs = {}
    end
    local bufs = vim.t.bufs
    if not vim.tbl_contains(bufs, args.buf) then
      table.insert(bufs, args.buf)
      vim.t.bufs = bufs
    end
    vim.t.bufs = vim.tbl_filter(require('sp.util').is_valid, vim.t.bufs)
  end,
})

vim.api.nvim_create_autocmd('BufDelete', {
  desc = 'Update buffers when deleting buffers',
  group = au_buffer,
  callback = function(args)
    for _, tab in ipairs(vim.api.nvim_list_tabpages()) do
      local bufs = vim.t[tab].bufs
      if bufs then
        for i, bufnr in ipairs(bufs) do
          if bufnr == args.buf then
            table.remove(bufs, i)
            vim.t[tab].bufs = bufs
            break
          end
        end
      end
    end
    vim.t.bufs = vim.tbl_filter(require('sp.util').is_valid, vim.t.bufs)
    vim.cmd.redrawtabline()
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  desc = 'Unlist quickfist buffers',
  group = au_buffer,
  pattern = 'qf',
  callback = function()
    vim.opt_local.buflisted = false
  end,
})

-- vim.filetype.add({
--   pattern = {
--     ['*.xml'] = 'xml',
--   }
-- })
