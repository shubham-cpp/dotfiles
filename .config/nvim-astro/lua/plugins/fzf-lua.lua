---@type LazySpec
return {
  "ibhagwan/fzf-lua",
  dependencies = "echasnovski/mini.icons",
  cmd = "FzfLua",
  lazy = true,
  opts = function()
    local actions = require "fzf-lua.actions"

    local vscode = {
      height = 0.55,
      width = 0.6,
      col = 0.4,
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
        fzf_opts = { ["--scheme"] = "path" },
        rg_glob = true,
        ---@param query string - first returned string is the new search query
        ---@param opts table - second returned string are (optional) additional rg flags
        ---@return string, string?
        rg_glob_fn = function(query, opts)
          local regex, flags = query:match "^(.-)%s%-%-(.*)$"
          -- If no separator is detected will return the original query
          return (regex or query), flags
        end,
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

        maps.n["<Leader>fG"] = {
          function()
            require("fzf-lua").live_grep {
              cmd = "git grep -i --line-number --column --color=always",
              fn_transform_cmd = function(query, cmd, _)
                local search_query, glob_str = query:match "(.-)%s-%-%-(.*)"
                if not glob_str then return end
                local new_cmd = string.format("%s %s %s", cmd, vim.fn.shellescape(search_query), glob_str)
                return new_cmd, search_query
              end,
            }
          end,
          desc = "Git grep",
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
      end,
    },
  },
}
