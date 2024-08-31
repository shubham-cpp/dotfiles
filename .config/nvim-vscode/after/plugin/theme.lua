if vim.g.vscode == nil then
  local ok_pairs, pairs = pcall(require, "nvim-autopairs")
  if ok_pairs then
    pairs.setup({ map_cr = true, fast_wrap = {} })
  end
  local ok_tokyodark, tokyodark = pcall(require, "tokyodark")
  if ok_tokyodark then
    tokyodark.setup({
      transparent_background = false,
      gamma = 1.00,
    })
    vim.cmd.colorscheme "tokyodark"
  end
end
