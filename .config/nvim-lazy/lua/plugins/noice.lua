---@type LazySpec
return {
  "folke/noice.nvim",
  enabled = false,
  opts = {
    popupmenu = {
      ---@type 'nui'|'cmp'
      backend = "cmp",
    },
  },
}
