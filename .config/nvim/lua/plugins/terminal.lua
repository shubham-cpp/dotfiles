---@type LazySpec
return {
  "akinsho/toggleterm.nvim",
  optional = true,
  opts = {},
  on_open = function(t)
    local bufnr = t.bufnr

    vim.opt_local.foldexpr = ""
    vim.opt_local.foldmethod = "manual"

    vim.keymap.set("t", "<C-]>", "<C-\\><C-n>", { buffer = bufnr, desc = "Goto normal mode" })
  end,
  specs = {
    {
      "AstroNvim/astrocore",
      opts = function(_, opts)
        local maps = opts.mappings

        maps.n["<C-\\>"] = { '<Cmd>execute v:count . "ToggleTerm direction=float"<CR>', desc = "Toggle terminal" } -- requires terminal that supports binding <C-'>
        maps.t["<C-\\>"] = { "<Cmd>ToggleTerm direction=float<CR>", desc = "Toggle terminal" } -- requires terminal that supports binding <C-'>
      end,
    },
  },
}
