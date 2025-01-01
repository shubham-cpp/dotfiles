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
  if vim.fn.isdirectory(vim.fn.stdpath "data" .. "/plugged/mono-jade") == 1 then
    vim.cmd.colorscheme "mono-jade"
  end
  if vim.fn.isdirectory(vim.fn.stdpath "data" .. "/plugged/mellow.nvim") == 1 then
    vim.cmd.colorscheme "mellow"
  end
  local ok_nordic, nordic = pcall(require, "nordic")
  if ok_nordic then
    nordic.setup({
      transparent = { bg = true },
    })
    require("nordic").load()
  end
  local ok_gruber_darker, gruber_darker = pcall(require, "gruber-darker")
  if ok_gruber_darker then
    ---@diagnostic disable-next-line: missing-fields
    gruber_darker.setup({})
    vim.cmd.colorscheme "gruber-darker"
    local c = require "gruber-darker.palette"
    vim.api.nvim_set_hl(0, "MatchParenCur", { bold = true })
    vim.api.nvim_set_hl(0, "@lsp.type.keyword.lua", { fg = c["brown"]:to_string(), bold = false })
    vim.api.nvim_set_hl(0, "@lsp.type.event.lua", { link = "GruberDarkerYellowBold" })
    vim.api.nvim_set_hl(
      0,
      "QuickScopePrimary",
      { fg = c["yellow"]:to_string(), bg = c["bg+1"]:to_string(), underline = true, bold = true }
    )
    vim.api.nvim_set_hl(
      0,
      "QuickScopeSecondary",
      { fg = c["brown"]:to_string(), bg = c["bg+1"]:to_string(), underline = true, bold = true }
    )
  end
end
