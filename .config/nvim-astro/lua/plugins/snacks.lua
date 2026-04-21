---@type LazySpec
return {
  "folke/snacks.nvim",
  optional = true,
  ---@type snacks.Config
  opts = {
    terminal = {},
    lazygit = {},
    zen = {
      zoom = {
        show = { statusline = true, tabline = true },
        wo = {
          number = true,
          relativenumber = true,
          signcolumn = "yes",
        },
        win = {
          width = 0, -- full width
          height = 0, -- full width
        },
      },
    },
    picker = {
      layout = { preset = "dropdown" },
      matcher = { frecency = true, history_bonus = true },
      ---@class snacks.picker.formatters.Config
      formatters = { file = { filename_first = true } },
      sources = {
        git_files = { untracked = true },
        git_grep = { untracked = true },
      },
    },
  },
  specs = {
    {
      "AstroNvim/astrocore",
      opts = function(_, opts)
        local maps = opts.mappings

        maps.n["<Leader>fN"] = {
          function() require("snacks").picker.notifications { layout = { preset = "vertical" } } end,
          desc = "Find notifications",
        }
        maps.n["<Leader>gg"] = { function() require("snacks.lazygit").open() end, desc = "Lazygit" }
        local toggle_terminal = {
          function()
            if vim.v.count ~= 0 then vim.g.previous_term_count = vim.v.count1 end

            require("snacks.terminal").toggle(nil, {
              win = { position = "float", border = "rounded" },
              count = vim.g.previous_term_count,
            })
          end,
          desc = "Terminal",
        }
        maps.n["<C-\\>"] = toggle_terminal
        maps.t["<C-\\>"] = toggle_terminal

        maps.n["<C-w>m"] = {
          function() require("snacks").zen.zoom() end,
          desc = "Window Zoom",
        }
      end,
    },
  },
}
