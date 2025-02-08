---@type LazySpec
return {
  {
    "toggleterm.nvim",
    keys = { "<C-\\>" },
    opts = {
      open_mapping = [[<c-\>]],
      direction = "float",
      -- shell = "fish",
      on_open = function(t)
        local bufnr = t.bufnr
        vim.opt_local.foldexpr = ""
        vim.opt_local.foldmethod = "manual"
        vim.keymap.set("t", "<C-]>", "<C-\\><C-n>", { buffer = bufnr, desc = "Goto normal mode" })
        vim.keymap.set("n", "A", "A<C-k>", { buffer = bufnr })
        vim.keymap.set("n", "D", "A<C-k><C-\\><C-n>", { buffer = bufnr })
        vim.keymap.set("n", "cc", "A<C-e><C-u>", { buffer = bufnr })
        vim.keymap.set("n", "dd", "A<C-e><C-u><C-\\><C-n>", { buffer = bufnr })
      end,
    },
  },
  -- {
  --
  --   "AstroNvim/astrocore",
  --   ---@type AstroCoreOpts
  --   opts = {
  --     autocmds = {
  --       togglerm_fixes = {
  --         {
  --           event = "FileType",
  --           pattern = "toggleterm",
  --           desc = "fix: to disable foldexpr for terminal to fix this issue. Takes too much time to open terminal.",
  --           callback = function()
  --             vim.opt_local.foldexpr = ""
  --             vim.opt_local.foldmethod = "manual"
  --           end,
  --         },
  --       },
  --     },
  --   },
  -- },
}
