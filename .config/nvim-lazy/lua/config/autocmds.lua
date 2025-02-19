local function augroup(name)
  return vim.api.nvim_create_augroup("sp_" .. name, { clear = true })
end

local au_buffer = augroup("buffer")

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

vim.api.nvim_create_autocmd("FileType", {
  group = au_buffer,
  desc = "Fix Comment Continuation",
  callback = function()
    vim.opt_local.formatoptions = "jcrqlnt"
  end,
})

vim.cmd.packadd("cfilter")
vim.g.snacks_animate = false
