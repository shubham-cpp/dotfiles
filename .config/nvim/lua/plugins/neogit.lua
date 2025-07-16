---@type LazySpec
return {
  {
    "NeogitOrg/neogit",
    cmd = "Neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
    },
    keys = {
      { "<leader>gn", "<cmd>Neogit kind=floating<cr>", desc = "Neogit" },
    },
    opts = {},
  },
  -- {
  --    "sindrets/diffview.nvim", opts = {
  --
  --   } ,
  -- }
}
