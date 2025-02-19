-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
vim.opt.wrap = true
vim.opt.showbreak = "󰄾 "
vim.opt.iskeyword:append("-")
vim.opt.scrolloff = 8
vim.opt.sessionoptions = "buffers,curdir,tabpages,winsize,help,globals,skiprtp"

vim.opt.wildignore:append({
  "*.o",
  "*.obj",
  "*.out",
  "*.class",

  "*.jpg",
  "*.jpeg",
  "*.png",
  "*.gif",

  "*.zip",
  "*.tar.gz",
  "*.tar.xz",
  "*.tar.bz2",
  "*.rar",

  "*.pyc",
  "*.pyo",

  "*.DS_Store",
  "*.swp",

  "node_modules",
  "vendor",
  ".git",
})

if vim.fn.executable("rg") == 1 then
  vim.opt.grepprg = "rg --vimgrep --smart-case" -- Also check RIPGREP_CONFIG_PATH="$HOME/.config/ripgreprc"
end
