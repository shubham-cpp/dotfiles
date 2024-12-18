local sp_group = vim.api.nvim_create_augroup("sp_custom", { clear = true })

vim.api.nvim_create_autocmd("TermOpen", {
  group = sp_group,
  pattern = "term://*",
  desc = "Keybindings for Terminal",
  callback = function(args)
    local bufnr = args.buf
    vim.keymap.set("t", "<C-]>", "<C-\\><C-n>", { buffer = bufnr })
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  group = sp_group,
  pattern = "toggleterm",
  desc = "fix: to disable foldexpr for terminal to fix this issue. Takes too much time to open terminal.",
  callback = function()
    vim.opt_local.foldexpr = ""
    vim.opt_local.foldmethod = "manual"
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  group = sp_group,
  pattern = "qf",
  desc = "load cfilter",
  command = "packadd cfilter",
})

-- Set up custom filetypes
vim.filetype.add {
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
    ["tsconfig.*.json"] = "jsonc",
    [".*/hyprland%.conf"] = "hyprlang",
  },
}
