---@type LazySpec
return {
  {
    "jake-stewart/multicursor.nvim",
    version = "^1",
    dependencies = { "nvimtools/hydra.nvim" },
    keys = {
      { "<leader>m", desc = "MultiCursors" },
    },
    config = function()
      local mc = require "multicursor-nvim"
      mc.setup()

      vim.keymap.set("n", "<Esc>", function()
        if not mc.cursorsEnabled() then
          mc.enableCursors()
        elseif mc.hasCursors() then
          mc.clearCursors()
        else
          vim.cmd "nohl"
          return "<esc>"
        end
      end, { expr = true })

      local Hydra = require "hydra"
      Hydra {
        name = "MultiCursors",
        mode = { "n", "x" },
        body = "<leader>m",
        heads = {
          {
            "n",
            function() mc.matchAddCursor(1) end,
            { desc = "next" },
          },
          {
            "p",
            function() mc.matchAddCursor(-1) end,
            { desc = "prev" },
          },
          {
            "N",
            function() mc.matchSkipCursor(1) end,
            { desc = "next(skip)" },
          },
          {
            "P",
            function() mc.matchSkipCursor(-1) end,
            { desc = "prev(skip)" },
          },
          {
            "j",
            function() mc.lineAddCursor(1) end,
            { desc = "down" },
          },
          {
            "k",
            function() mc.lineAddCursor(-1) end,
            { desc = "up" },
          },
          {
            "J",
            function() mc.lineSkipCursor(1) end,
            { desc = "down(skip)" },
          },
          {
            "K",
            function() mc.lineSkipCursor(-1) end,
            { desc = "up(skip)" },
          },
          {
            "t",
            function() mc.toggleCursor() end,
            { desc = "toggle" },
          },
          {
            "h",
            function() mc.prevCursor() end,
            { desc = "prev(goto)" },
          },
          {
            "l",
            function() mc.nextCursor() end,
            { desc = "next(goto)" },
          },
          {
            "a",
            function() mc.matchAllAddCursors() end,
            { desc = "all", exit = true },
          },
          { "<Esc>", nil, { exit = true, desc = false } },
        },
      }
    end,
  },
}
