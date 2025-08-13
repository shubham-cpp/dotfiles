local o = vim.opt
local g = vim.g

o.tabstop = 2
o.shiftwidth = 2
o.softtabstop = -1
o.expandtab = true
o.number = true
o.relativenumber = true
o.autoindent = true
o.smartindent = true
o.ignorecase = true
o.smartcase = true
o.mouse = "a"
o.iskeyword:append "-"
o.path:append "**"
o.backup = true

o.undofile = true
o.undolevels = 10000

o.wildignorecase = true
o.splitbelow = true
o.splitright = true
o.undodir = { vim.fn.stdpath "state" .. "/undodir//" }
o.directory = { vim.fn.stdpath "state" .. "/swap//" }
o.backupdir = { vim.fn.stdpath "state" .. "/backup//" }
o.sessionoptions:append({
  "globals",
})
o.sessionoptions:remove "folds"
o.jumpoptions = 'stack'

-- vim.schedule(function()
--   o.clipboard:append(vim.env.SSH_TTY and "" or "unnamedplus") -- Sync with system clipboard
-- end)

if vim.fn.executable "rg" == 1 then
  o.grepprg = "rg --vimgrep --no-heading --smart-case --sort=path"
  ---  set grepformat+=%f:%l:%c:%m
end

if not vim.g.vscode then
  o.scrolloff = 12
  o.wildmode = "longest:full,full"
  o.signcolumn = 'yes'
end

vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight copied text",
  group = vim.api.nvim_create_augroup("sp_buffer", {}),
  callback = function()
    vim.highlight.on_yank({ higroup = "IncSearch", timeout = 100 })
  end,
})
