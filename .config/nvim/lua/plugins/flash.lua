return {
  "folke/flash.nvim",
  optional = true,
  ---@type Flash.Config
  opts = {
    modes = {
      char = {
        enabled = false,
        jump_labels = true,
      },
    },
  },
  specs = {
    {
      "AstroNvim/astrocore",
      opts = function(_, opts)
        local maps = opts.mappings
        maps.x["S"] = false
      end,
    },
  },
}
