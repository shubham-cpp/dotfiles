---@type LazySpec
return {
  {
    "mg979/vim-visual-multi",
    branch = "master",
    version = false,
    enabled = false,
    init = function()
      vim.g.VM_maps = {}
      vim.g.VM_mouse_mappings = 1
      vim.g.VM_maps = {
        ["Find Under"] = "<M-d>",
        ["Find Subword Under"] = "<M-d>",
        ["Skip Region"] = "<C-x>",
        ["Select All"] = "<M-a>",
        ["Start Regex Search"] = "\\/",
      }
    end,
    keys = { { "<M-d>", mode = { "n", "v" } }, { "<M-a>", mode = { "n", "v" } } },
  },
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
            function() require("multicursor-nvim").matchAddCursor(1) end,
            { desc = "next" },
          },
          {
            "p",
            function() require("multicursor-nvim").matchAddCursor(-1) end,
            { desc = "prev" },
          },
          {
            "N",
            function() require("multicursor-nvim").matchSkipCursor(1) end,
            { desc = "next(skip)" },
          },
          {
            "P",
            function() require("multicursor-nvim").matchSkipCursor(-1) end,
            { desc = "prev(skip)" },
          },
          {
            "j",
            function() require("multicursor-nvim").lineAddCursor(1) end,
            { desc = "down" },
          },
          {
            "k",
            function() require("multicursor-nvim").lineAddCursor(-1) end,
            { desc = "up" },
          },
          {
            "J",
            function() require("multicursor-nvim").lineSkipCursor(1) end,
            { desc = "down(skip)" },
          },
          {
            "K",
            function() require("multicursor-nvim").lineSkipCursor(-1) end,
            { desc = "up(skip)" },
          },
          {
            "t",
            function() require("multicursor-nvim").toggleCursor() end,
            { desc = "toggle" },
          },
          {
            "h",
            function() require("multicursor-nvim").prevCursor() end,
            { desc = "prev(goto)" },
          },
          {
            "k",
            function() require("multicursor-nvim").nextCursor() end,
            { desc = "next(goto)" },
          },
          {
            "a",
            function() require("multicursor-nvim").matchAllAddCursors() end,
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
  {
    "jake-stewart/multicursor.nvim",
    branch = "1.0",
    lazy = true,
    opts = {},
    keys = function()
      local mc = require "multicursor-nvim"
      return {
        -- { "<leader>mj", function() mc.lineAddCursor(1) end, desc = "add cursor dowm", mode = { "n", "x" } },
        -- { "<leader>mk", function() mc.lineAddCursor(-1) end, desc = "add cursor up", mode = { "n", "x" } },
        -- { "<leader>mJ", function() mc.lineSkipCursor(1) end, desc = "skip cursor down", mode = { "n", "x" } },
        -- { "<leader>mK", function() mc.lineSkipCursor(-1) end, desc = "skip cursor up", mode = { "n", "x" } },
        -- { "<leader>mn", function() mc.matchAddCursor(1) end, desc = "add cursor to next match", mode = { "n", "x" } },
        -- { "<leader>mp", function() mc.matchAddCursor(-1) end, desc = "add cursor to prev match", mode = { "n", "x" } },
        -- { "<leader>mN", function() mc.matchSkipCursor(1) end, desc = "skip cursor to next match", mode = { "n", "x" } },
        -- {
        --   "<leader>mP",
        --   function() mc.matchSkipCursor(-1) end,
        --   desc = "skip cursor to prev match",
        --   mode = { "n", "x" },
        -- },
        -- { "<leader>ma", mc.matchAllAddCursors, desc = "all matches", mode = { "n", "x" } },
        { "<c-leftmouse>", mc.handleMouse, desc = "multicursor using mouse" },
        -- { "<leader>mt", mc.toggleCursor, desc = "toggle cursor" },
        -- { "<leader>mh", mc.prevCursor, desc = "goto prev cursor" },
        -- { "<leader>ml", mc.nextCursor, desc = "goto next cursor" },
        -- { "<leader>md", mc.deleteCursor, desc = "delete cursor" },
        {
          "<esc>",
          function()
            if not mc.cursorsEnabled() then
              mc.enableCursors()
            elseif mc.hasCursors() then
              mc.clearCursors()
            else
              vim.cmd "nohl"
              return "<esc>"
            end
          end,
          desc = "Escape cursor, No-Highlight",
          expr = true,
        },
      }
    end,
  },
}
