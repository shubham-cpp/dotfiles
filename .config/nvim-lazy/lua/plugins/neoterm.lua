---@type LazySpec
return {
  "nyngwang/NeoTerm.lua",
  cmd = { "NeoTermToggle", "NeoTermEnterNormal" },
  keys = {
    { "<M-\\>", "<cmd>NeoTermToggle<cr>" },
    { "<M-\\>", "<C-\\><C-n><cmd>NeoTermToggle<cr>", mode = "t" },
    { "<M-/>", "<cmd>NeoTermEnterNormal<cr>" },
    { "<M-/>", "<C-\\><C-n><cmd>NeoTermEnterNormal<cr>", mode = "t" },
  },
  config = function()
    require("neo-term").setup({
      exclude_filetypes = { "oil", "qf", "TelescopePrompt" },
      -- exclude_buftypes = {}, -- 'terminal' will always be added by NeoTerm.lua
      -- enabled by default!
      -- presets = {
      --   'vim-test',
      -- }
    })
  end,
}
