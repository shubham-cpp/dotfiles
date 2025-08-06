---@type LazySpec
return {
  "ibhagwan/fzf-lua",
  dependencies = "echasnovski/mini.icons",
  cmd = "FzfLua",
  opts = function()
    local actions = require "fzf-lua.actions"

    local vscode = {
      height = 0.55,
      width = 0.6,
      row = 0,
    }

    local action_keys = {
      ["ctrl-q"] = {
        fn = actions.file_edit_or_qf,
        prefix = "select-all+",
      },
    }

    return {
      { "border-fused", "hide" },
      defaults = {
        formatter = { "path.filename_first", 2 },
        fzf_opts = { ["--scheme"] = "default" },
      },
      winopts = { preview = { default = "bat", layout = "vertical" } },
      files = {
        actions = action_keys,
        previewer = false,
        winopts = vscode,
      },
      git = {
        files = {
          actions = action_keys,
          previewer = false,
          winopts = vscode,
        },
      },
      grep = {
        actions = action_keys,
        rg_glob = true,
        glob_flag = "--iglob",
        glob_separator = "%s%-%-",
      },
    }
  end,
  specs = {
    {
      "AstroNvim/astrocore",
      optional = true,
      opts = function(_, opts)
        local maps = opts.mappings

        maps.n["<c-p>"] = {
          function() require("fzf-lua").files {} end,
          desc = "Find files",
        }

        maps.n["<Leader>fg"] = {
          function() require("fzf-lua").git_files {} end,
          desc = "Find git files",
        }
        maps.n["<Leader>fn"] = {
          function()
            require("fzf-lua").files {
              cwd = vim.fn.stdpath "config",
            }
          end,
          desc = "Find AstroNvim config files",
        }
        maps.n["<Leader>fd"] = {
          function()
            require("fzf-lua").git_files {
              cwd = vim.fn.expand "~/Documents/dotfiles/",
            }
          end,
          desc = "Dotfiles",
        }
        maps.n["<Leader>fh"] = {
          function() require("fzf-lua").help_tags {} end,
          desc = "Help tags",
        }
        maps.n["<Leader>fk"] = {
          function() require("fzf-lua").keymaps {} end,
          desc = "Keymaps",
        }
      end,
    },
  },
}
