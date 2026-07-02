local gh = require("config.utils").gh

vim.g.qs_highlight_on_keys = { "f", "F", "t", "T" }
vim.g.qs_lazy_highlight = 1
vim.g.qs_buftype_blacklist = { "terminal", "nofile", "dashboard", "startify" }

vim.pack.add({
  gh "unblevable/quick-scope",
  gh "folke/snacks.nvim",
})

vim.api.nvim_set_hl(0, "EyelinerPrimary", { fg = "#f3be7c", bold = true, underline = true })
vim.api.nvim_set_hl(0, "EyelinerSecondary", { fg = "#7e98e8", underline = true })
vim.api.nvim_set_hl(0, "QuickScopePrimary", { fg = "#f3be7c", bold = true, underline = true })
vim.api.nvim_set_hl(0, "QuickScopeSecondary", { fg = "#7e98e8", underline = true })

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
        vim.bo[buf].list = false
        vim.bo[buf].spell = false
      end,
    },
  }
)
vim.g.previous_term_count = { 0 }
-- vim.api.nvim_create_autocmd("ModeChanged", {
--   group = vim.api.nvim_create_augroup("ConfigSnacks", { clear = true }),
--   desc = "Remember insert mode for snacks.terminal",
--   callback = function(arg)
--     -- {
--     --   buf = 9,
--     --   event = "ModeChanged",
--     --   file = "",
--     --   group = 61,
--     --   id = 267,
--     --   match = "t:nt"
--     -- }
--     local is_snacks_terminal = vim.bo[arg.buf].filetype == "snacks_terminal"
--     if not is_snacks_terminal then
--       return
--     end
--
--     local changes = vim.split(arg.match, ":")
--     local from = changes[1]
--     local to = changes[2]
--
--     vim.g.previous_term_count["t:" .. vim.g.previous_term_count[1]] = to ~= "nt"
--   end,
-- })
vim.keymap.set({ "n", "t" }, "<C-\\>", function()
  if vim.v.count ~= 0 then
    vim.g.previous_term_count[1] = vim.v.count1
  end
  Snacks.terminal.toggle(nil, {
    win = { border = "rounded" },
    -- auto_insert = false,
    -- start_insert = vim.g.previous_term_count["t:" .. vim.g.previous_term_count[1]] or true,
    count = vim.g.previous_term_count[1] or 0,
  })
end, { desc = "Terminal Toggle" })
