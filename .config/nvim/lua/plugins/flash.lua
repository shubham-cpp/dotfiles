---@type LazySpec
return {
  "folke/flash.nvim",
  specs = {
    {
      "AstroNvim/astrocore",
      opts = function(_, opts)
        local maps = opts.mappings
        maps.x["S"] = false
      end,
    },
  },
  opts = {
    modes = {
      char = {
        enabled = false,
        jump_labels = true,
      },
    },
  },
}
