local gh = require("config.utils").gh

vim.pack.add({
  gh "folke/snacks.nvim",
})

vim.api.nvim_set_hl(0, "EyelinerPrimary", { fg = "#f3be7c", bold = true, underline = true })
vim.api.nvim_set_hl(0, "EyelinerSecondary", { fg = "#7e98e8", underline = true })
vim.api.nvim_set_hl(0, "QuickScopePrimary", { fg = "#f3be7c", bold = true, underline = true })
vim.api.nvim_set_hl(0, "QuickScopeSecondary", { fg = "#7e98e8", underline = true })

require("config.quick_scope_lite").setup()

require("snacks").setup(
  ---@type snacks.Config
  {
    input = {},
    terminal = {},
    quickfile = {},
    statuscolumn = { folds = { open = true }, refresh = 150 },
    bigfile = {
      setup = function(ctx)
        local buf = ctx.buf
        pcall(vim.treesitter.stop, buf)
        vim.diagnostic.enable(false, { bufnr = buf })

        vim.bo[buf].swapfile = false
        vim.bo[buf].undofile = false
        vim.bo[buf].foldmethod = "indent"

        vim.bo[buf].minicursorword_disable = true
        vim.bo[buf].miniclue_disable = true
        vim.bo[buf].miniclue_disable = true
        vim.bo[buf].miniindentscope_disable = true
        vim.bo[buf].minihipatterns_disable = true
        vim.bo[buf].minidiff_disable = true
        vim.b[buf].quick_scope_lite_disable = true
        vim.b[buf].is_bigfile = true
        vim.bo[buf].list = false
        vim.bo[buf].spell = false
      end,
    },
  }
)
vim.g.previous_term_count = { 0 }
vim.keymap.set({ "n", "t" }, "<C-\\>", function()
  if vim.v.count ~= 0 then
    vim.g.previous_term_count[1] = vim.v.count1
  end
  Snacks.terminal.toggle(nil, {
    win = { border = "rounded" },
    count = vim.g.previous_term_count[1] or 0,
  })
end, { desc = "Terminal Toggle" })
