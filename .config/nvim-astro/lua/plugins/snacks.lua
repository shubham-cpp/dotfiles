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
        buffers = {
          layout = { preset = "vscode" },
          win = {
            input = {
              keys = {
                ["<a-x>"] = { "bufdelete", mode = { "n", "i" } },
                ["<c-x>"] = { "edit_split", mode = { "i", "n" } },
              },
            },
            list = { keys = { ["dd"] = "bufdelete" } },
          },
        },
        git_files = { untracked = true },
        git_grep = { untracked = true },
      },
      win = {
        input = {
          ["<c-u>"] = { "preview_scroll_up", mode = { "i", "n" } },
          ["<c-d>"] = { "preview_scroll_down", mode = { "i", "n" } },
          ["<c-f>"] = { "list_scroll_down", mode = { "i", "n" } },
          ["<c-b>"] = { "list_scroll_up", mode = { "i", "n" } },
          ["<c-x>"] = { "edit_split", mode = { "i", "n" } },
          ["<c-t>"] = { "edit_tab", mode = { "i", "n" } },
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
        local has_fzf = pcall(require, "fzf-lua")

        maps.n["<Leader>fN"] = {
          function() require("snacks.picker").notifications { layout = { preset = "vertical" } } end,
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

        if not has_fzf then
          maps.n["<Leader>fg"] = {
            function() require("snacks.picker").git_files { layout = { preset = "vscode" } } end,
            desc = "Git Files",
          }
          maps.n["<C-p>"] = {
            function() require("snacks.picker").files { layout = { preset = "vscode" } } end,
            desc = "Files",
          }

          maps.n["<Leader>fB"] = {
            function() require("snacks.picker").grep_buffers {} end,
            desc = "Buffers(Grep)",
          }
          maps.n["<Leader>fs"] = {
            function() require("snacks.picker").grep {} end,
            desc = "Grep",
          }
          maps.n["<Leader>fS"] = {
            function() require("snacks.picker").grep { cwd = vim.fn.expand "%:p:h" } end,
            desc = "Grep(Cwd)",
          }
          maps.x["<Leader>fs"] = {
            function() require("snacks.picker").grep_word {} end,
            desc = "Grep",
          }
          maps.x["<Leader>fS"] = {
            function() require("snacks.picker").grep_word { cwd = vim.fn.expand "%:p:h" } end,
            desc = "Grep(Cwd)",
          }
          -- maps.n["<Leader>fW"] = {
          --   function() require("snacks.picker").grep { cwd = vim.fn.expand "%:p:h" } end,
          --   desc = "Grep(Cwd)",
          -- }

          maps.n["<Leader>fn"] = {
            function() require("snacks.picker").files { cwd = vim.fn.stdpath "config", layout = { preset = "vscode" } } end,
            desc = "Neovim",
          }
          maps.n["<Leader>fd"] = {
            function()
              require("snacks.picker").git_files {
                cwd = vim.fn.expand "~/Documents/dotfiles",
                layout = { preset = "vscode" },
              }
            end,
            desc = "Dotfiles",
          }
          maps.n["<Leader>fa"] = {
            function() require("snacks.picker").autocmds { layout = { preset = "vertical" } } end,
            desc = "Autocomds",
          }
          maps.n["<Leader>fz"] = {
            function()
              require("snacks.picker").zoxide {
                layout = { preset = "vscode" },
              }
            end,
            desc = "Zoxide",
          }
          maps.n["<Leader>fu"] = {
            function() require("snacks.picker").undo { layout = { preset = "vertical" } } end,
            desc = "Undo",
          }
          maps.n["<Leader>fr"] = {
            function() require("snacks.picker").resume {} end,
            desc = "Resume",
          }
          maps.n["<Leader>fR"] = {
            function() require("snacks.picker").registers { layout = { preset = "vertical" } } end,
            desc = "Registers",
          }
          maps.n["z="] = {
            function() require("snacks.picker").spelling {} end,
            desc = "Spelling",
          }
        end
      end,
    },
    {
      "AstroNvim/astrolsp",
      optional = true,
      opts = {
        mappings = {
          n = {
            grr = {
              function() require("snacks.picker").lsp_references { include_declaration = true } end,
              desc = "References",
              cond = "textDocument/references",
            },
            gri = {
              function() require("snacks.picker").lsp_implementations() end,
              desc = "Implementation",
              cond = "textDocument/implementation",
            },
            grs = {
              function() require("snacks.picker").lsp_symbols { workspace = false } end,
              desc = "Document symbols",
              cond = "textDocument/documentSymbol",
            },
            grS = {
              function() require("snacks.picker").lsp_workspace_symbols {} end,
              desc = "Workspace symbols",
              cond = "workspace/symbol",
            },
          },
        },
      },
    },
  },
}
