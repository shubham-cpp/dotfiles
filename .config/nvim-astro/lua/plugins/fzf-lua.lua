---@type LazySpec
return {
  "ibhagwan/fzf-lua",
  optional = true,
  opts = function(_, opts)
    local rg_glob_fn = function(query)
      local split_index = query:find " --"
      if split_index then
        local search = query:sub(1, split_index - 1)
        local glob_str = query:sub(split_index + 3)
        return search, glob_str
      end
      return query
    end

    local actions = require "fzf-lua.actions"
    local vscode = {
      height = 0.55,
      width = 0.6,
      row = 0,
    }
    local action_keys = {
      ["ctrl-x"] = actions.file_split,
    }
    opts.defaults = {
      formatter = { "path.filename_first", 2 },
    }
    opts.winopts = { preview = { default = "bat", layout = "vertical" } }
    opts.files = {
      actions = action_keys,
      previewer = false,
      winopts = vscode,
    }
    opts.git = {
      files = {
        actions = action_keys,
        previewer = false,
        winopts = vscode,
        cmd = "git ls-files --cached --others --exclude-standard",
      },
      branches = {
        cmd_add = { "git", "switch", "-c" },
      },
    }
    opts.grep = {
      -- rg_glob = true,
      rg_glob_fn = rg_glob_fn,
      actions = action_keys,
    }
  end,
  dependencies = {
    {
      "AstroNvim/astrocore",
      opts = function(_, opts)
        local function dotfiles_cwd() return vim.fn.expand "~/Documents/dotfiles" end
        local maps = opts.mappings
        maps.n["<Leader>fn"] =
          { function() require("fzf-lua").files { cwd = vim.fn.stdpath "config" } end, desc = "Neovim Config" }
        maps.n["<Leader>fd"] = {
          function() require("fzf-lua").git_files { cwd = dotfiles_cwd() } end,
          desc = "DotFiles",
        }

        maps.n["<Leader>fa"] = { "<cmd>FzfLua autocmds<cr>", desc = "Autocmds" }
        maps.n["<Leader>fz"] = { "<cmd>FzfLua zoxide<cr>", desc = "Zoxide" }

        maps.n["<Leader>fs"] = { "<cmd>FzfLua live_grep<cr>", desc = "Search" }
        maps.n["<Leader>fS"] = {
          function() require("fzf-lua").live_grep { cwd = vim.fn.expand "%:p:h" } end,
          desc = "Search(Current Dir)",
        }
        maps.n["<Leader>fW"] = {
          function() require("fzf-lua").grep_cword { cwd = vim.fn.expand "%:p:h" } end,
          desc = "Grep Current Word(Current Dir)",
        }
        maps.n["<Leader>fw"] = { "<cmd>FzfLua grep_cword<cr>", desc = "Grep Current Word" }
        maps.v["<Leader>fs"] = { "<cmd>FzfLua grep_visual<cr>", desc = "Grep" }
        maps.v["<Leader>fS"] =
          { function() require("fzf-lua").grep_visual { cwd = vim.fn.expand "%:p:h" } end, desc = "Grep(Current Dir)" }

        maps.n["<Leader>fu"] = { "<cmd>FzfLua undotree<cr>", desc = "Undotree" }
        maps.n["<Leader>fg"] = { "<cmd>FzfLua git_files<cr>", desc = "Git Files" }
        maps.n["<C-p>"] = { "<cmd>FzfLua files<cr>", desc = "Files" }
        -- Git related
        maps.n["<Leader>gw"] = { "<cmd>FzfLua git_worktree<cr>", desc = "Worktree" }
        maps.n["<Leader>gD"] = { "<cmd>FzfLua git_diff<cr>", desc = "FzfLua Diff" }
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
      end,
    },
    {
      "AstroNvim/astrolsp",
      optional = true,
      opts = {
        mappings = {
          n = {
            grr = {
              function() require("fzf-lua").lsp_references() end,
              desc = "References",
              cond = "textDocument/references",
            },
            gri = {
              function() require("fzf-lua").lsp_implementations() end,
              desc = "Implementation",
              cond = "textDocument/implementation",
            },
            grs = {
              function() require("fzf-lua").lsp_document_symbols { fzf_cli_args = "--tiebreak=end,index" } end,
              desc = "Document symbols",
              cond = "textDocument/documentSymbol",
            },
            grS = {
              function() require("fzf-lua").lsp_live_workspace_symbols { fzf_cli_args = "--nth 1..2" } end,
              desc = "Workspace symbols",
              cond = "workspace/symbol",
            },
          },
        },
      },
    },
  },
}
