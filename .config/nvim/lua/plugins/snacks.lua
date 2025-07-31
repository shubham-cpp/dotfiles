---@type LazySpec
return {
  "folke/snacks.nvim",
  optional = true,
  ---@type snacks.Config
  opts = {
    dashboard = { enabled = false },
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
    lazygit = {},
    picker = {
      layout = { preset = "dropdown" },
      matcher = { frecency = true, history_bonus = true },
      ---@class snacks.picker.formatters.Config
      formatters = { file = { filename_first = true } },
      sources = {
        buffers = {
          win = {
            input = {
              keys = {
                ["<c-x>"] = { "edit_split", mode = { "i", "n" } },
                ["<a-x>"] = { "bufdelete", mode = { "n", "i" } },
              },
            },
            list = { keys = { ["dd"] = "bufdelete" } },
          },
        },
        git_files = { untracked = true },
        git_grep = { untracked = true },
      },
      win = {
        -- input window
        input = {
          keys = {
            ["<c-u>"] = { "preview_scroll_up", mode = { "i", "n" } },
            ["<c-d>"] = { "preview_scroll_down", mode = { "i", "n" } },
            ["<c-f>"] = { "list_scroll_down", mode = { "i", "n" } },
            ["<c-b>"] = { "list_scroll_up", mode = { "i", "n" } },
            ["<c-x>"] = { "edit_split", mode = { "i", "n" } },
            ["<c-t>"] = { "edit_tab", mode = { "i", "n" } },
            ["<c-c>"] = { "copy", mode = { "i", "n" } },
          },
        },
        list = {
          keys = {
            ["<c-u>"] = "preview_scroll_up",
            ["<c-d>"] = "preview_scroll_down",
            ["<c-f>"] = "list_scroll_down",
            ["<c-b>"] = "list_scroll_up",
            ["<c-x>"] = "edit_split",
          },
        },
      },
    },
  },
  specs = {
    {
      "AstroNvim/astrocore",
      opts = function(_, opts)
        local maps = opts.mappings

        maps.n["<Leader><Leader>"] = {
          function()
            local is_git = vim.g.gitsigns_head or vim.b.gitsigns_head
            if is_git then
              require("snacks").picker.git_files { layout = { preset = "vscode" } }
            else
              require("snacks").picker.files { layout = { preset = "vscode" } }
            end
          end,
          desc = "Find files",
        }
        -- maps.n["<c-p>"] = {
        --   function() require("snacks").picker.files { layout = { preset = "vscode" } } end,
        --   desc = "Find files",
        -- }
        -- maps.n["<Leader>fg"] = {
        --   function() require("snacks").picker.git_files { layout = { preset = "vscode" } } end,
        --   desc = "Find git files",
        -- }

        maps.n["<Leader>fH"] = { function() require("snacks").picker.highlights() end, desc = "highlights" }
        maps.n["<Leader>fr"] = { function() require("snacks").picker.resume() end, desc = "Resume" }
        maps.n["<Leader>fz"] = { function() require("snacks").picker.zoxide() end, desc = "Zoxide" }

        -- maps.n["<Leader>fn"] = {
        --   function()
        --     require("snacks").picker.files {
        --       dirs = { vim.fn.stdpath "config" },
        --       layout = { preset = "vscode" },
        --       desc = "Config Files",
        --     }
        --   end,
        --   desc = "Find AstroNvim config files",
        -- }
        -- maps.n["<Leader>fd"] = {
        --   function()
        --     require("snacks").picker.git_files {
        --       cwd = vim.fn.expand "~/Documents/dotfiles/",
        --       layout = { preset = "vscode" },
        --       desc = "Dotfiles",
        --     }
        --   end,
        --   desc = "Dotfiles",
        -- }
        maps.n["<Leader>fN"] = { function() require("snacks").picker.notifications() end, desc = "Find notifications" }
        maps.n["<Leader>fL"] = { function() require("snacks").picker.lazy() end, desc = "Lazy" }

        maps.n["<Leader>fB"] = { function() require("snacks").picker.grep_buffers() end, desc = "Grep Buffers" }
        maps.n["<Leader>fs"] = { function() require("snacks").picker.grep() end, desc = "Grep" }
        maps.n["<Leader>fS"] = {
          function() require("snacks").picker.grep { cwd = vim.fn.expand "%:p:h" } end,
          desc = "Grep(cwd)",
        }
        maps.n["<Leader>fw"] = { function() require("snacks").picker.grep_word() end, desc = "Grep cword" }
        maps.n["<Leader>fW"] = {
          function() require("snacks").picker.grep_word { cwd = vim.fn.expand "%:p:h" } end,
          desc = "Grep cword(cwd)",
        }
        maps.x["<Leader>fw"] = { function() require("snacks").picker.grep_word() end, desc = "Grep cword" }
        maps.x["<Leader>fW"] = {
          function() require("snacks").picker.grep_word { cwd = vim.fn.expand "%:p:h" } end,
          desc = "Grep cword(cwd)",
        }
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
