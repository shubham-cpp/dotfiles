---@type LazySpec
return {
  "folke/snacks.nvim",
  optional = true,
  opts = {
    lazygit = {},
    zen = {
      on_open = function() end,
      on_close = function() end,
      ---@type snacks.zen.Config
      zoom = {
        toggles = {},
        show = { statusline = true, tabline = true },
        win = {
          wo = {
            number = true,
            relativenumber = true,
            signcolumn = "yes",
          },
          backdrop = false,
          width = 0, -- full width
          height = 0, -- full width
        },
      },
    },
  },
  specs = {
    {
      "AstroNvim/astrocore",
      opts = function(_, opts)
        local maps = opts.mappings
        maps.n["<C-w>m"] = {
          function() require("snacks").zen.zoom() end,
          desc = "Window Zoom",
        }
        maps.n["<Leader>gg"] = {
          function() require("snacks").lazygit.open() end,
          desc = "Lazygit",
        }
      end,
    },
  },
}
