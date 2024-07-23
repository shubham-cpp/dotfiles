-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here
local function augroup(name)
  return vim.api.nvim_create_augroup("sp_" .. name, { clear = true })
end

local au_buffer = augroup("buffer")

vim.api.nvim_create_autocmd("CmdlineEnter", {
  group = au_buffer,
  command = "set nosmartcase",
  desc = "Make search case-sensitive by default",
})
vim.api.nvim_create_autocmd("CmdlineLeave", {
  group = au_buffer,
  command = "set smartcase",
  desc = "Make search case-insensitive by default",
})

vim.api.nvim_create_autocmd("FileType", {
  group = augroup("webdev"),
  desc = "Set spaces to 2",
  pattern = {
    "lua",
    "jsonc",
    "json",
    "json5",
    "typescriptreact",
    "typescript",
    "typescript.tsx",
    "typescript.jsx",
    "javascriptreact",
    "javascript",
    "javascript.jsx",
    "vue",
    "html",
    "css",
    "less",
    "scss",
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

vim.api.nvim_create_autocmd("FileType", {
  group = au_buffer,
  desc = "Fix Comment Continuation",
  callback = function()
    vim.opt_local.formatoptions = "jcrqlnt"
  end,
})

vim.filetype.add({
  extension = {
    fish = "fish",
    ocaml = "ocaml",
    rasi = "rasi",
    roc = "roc",
  },
  filename = {
    vimfrc = "vim",
    dwm_sxhkdrc = "sxhkdrc",
    [".env"] = "conf",
    [".env.*"] = "conf",
    ["package.json"] = "jsonc",
  },
  pattern = {
    ["*profile"] = "sh",
    ["*.postcss"] = "css",
    ["*.kbd"] = "lisp",
    [".eslintrc.*"] = "jsonc",
    ["tsconfig.*.json"] = "jsonc",
    [".*/hyprland%.conf"] = "hyprlang",
  },
})
