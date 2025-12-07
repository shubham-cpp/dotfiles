---@type LazySpec
return {
  {
    "jake-stewart/multicursor.nvim",
    branch = "1.0",
    enabled = true,
    opts = {},
    keys = function()
      local mc = require("multicursor-nvim")
      return {
        {
          "<esc>",
          function()
            if not mc.cursorsEnabled() then
              mc.enableCursors()
            elseif mc.hasCursors() then
              mc.clearCursors()
            else
              vim.cmd("nohl")
              return "<esc>"
            end
          end,
          desc = "Escape cursor, No-Highlight",
          expr = true,
        },
      }
    end,
    specs = {
      {
        "nvimtools/hydra.nvim",
        optional = true,
        opts = {
          ["MultiCursors"] = {
            name = "MultiCursors",
            mode = { "n", "x" },
            body = "<leader>m",
            heads = {
              {
                "n",
                function()
                  require("multicursor-nvim").matchAddCursor(1)
                end,
                { desc = "next" },
              },
              {
                "p",
                function()
                  require("multicursor-nvim").matchAddCursor(-1)
                end,
                { desc = "prev" },
              },
              {
                "N",
                function()
                  require("multicursor-nvim").matchSkipCursor(1)
                end,
                { desc = "next(skip)" },
              },
              {
                "P",
                function()
                  require("multicursor-nvim").matchSkipCursor(-1)
                end,
                { desc = "prev(skip)" },
              },
              {
                "j",
                function()
                  require("multicursor-nvim").lineAddCursor(1)
                end,
                { desc = "down" },
              },
              {
                "k",
                function()
                  require("multicursor-nvim").lineAddCursor(-1)
                end,
                { desc = "up" },
              },
              {
                "J",
                function()
                  require("multicursor-nvim").lineSkipCursor(1)
                end,
                { desc = "down(skip)" },
              },
              {
                "K",
                function()
                  require("multicursor-nvim").lineSkipCursor(-1)
                end,
                { desc = "up(skip)" },
              },
              {
                "t",
                function()
                  require("multicursor-nvim").toggleCursor()
                end,
                { desc = "toggle" },
              },
              {
                "h",
                function()
                  require("multicursor-nvim").prevCursor()
                end,
                { desc = "prev(goto)" },
              },
              {
                "l",
                function()
                  require("multicursor-nvim").nextCursor()
                end,
                { desc = "next(goto)" },
              },
              {
                "a",
                function()
                  require("multicursor-nvim").matchAllAddCursors()
                end,
                { desc = "all", exit = true },
              },
              {
                "<Esc>",
                nil,
                { exit = true, desc = false },
              },
            },
          },
        },
      },
    },
  },
}
