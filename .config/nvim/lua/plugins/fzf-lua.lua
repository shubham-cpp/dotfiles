---@type LazySpec
return {
  "ibhagwan/fzf-lua",
  optional = true,
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
      { "border-fused", "skim" },
      defaults = {
        -- formatter = { "path.filename_first", 1 },
      },
      -- fzf_opts = { ["--algo"] = "frizbee" },
      winopts = { preview = { default = "bat", layout = "vertical" } },
      keymap = {
        builtin = {
          true,
          ["<C-d>"] = "preview-page-down",
          ["<C-u>"] = "preview-page-up",
        },
        fzf = {
          true,
          ["ctrl-d"] = "preview-page-down",
          ["ctrl-u"] = "preview-page-up",
          ["ctrl-q"] = "select-all+accept",
        },
      },
      files = {
        actions = action_keys,
        previewer = false,
        winopts = vscode,
      },
      git = {
        files = {
          cmd = "git ls-files --cached --others --exclude-standard",
          actions = action_keys,
          previewer = false,
          winopts = vscode,
        },
        branches = {
          cmd_add = { "git", "switch", "-c" },
        },
      },
      grep = {
        actions = action_keys,
        -- fzf_opts = { ["--scheme"] = "path" },
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
      opts = function(_, opts)
        local maps = opts.mappings
        maps.n["<C-p>"] = { function() require("fzf-lua").files() end, desc = "Find files" }
        maps.n["<Leader>fr"] = { function() require("fzf-lua").resume() end, desc = "Resume previous search" }
        maps.n["<Leader>fl"] = { function() require("fzf-lua").grep_curbuf() end, desc = "Search(buf)" }

        if maps.n["<Leader>lR"] then
          maps.n["<Leader>lR"][1] = function()
            require("fzf-lua").lsp_references {
              ignore_current_line = true,
              includeDeclaration = false,
            }
          end
        end

        maps.n["<Leader>fn"] =
          { function() require("fzf-lua").files { cwd = vim.fn.stdpath "config" } end, desc = "Neovim Config" }
        maps.n["<Leader>fd"] = {
          function() require("fzf-lua").git_files { cwd = vim.fn.expand "~/Documents/dotfiles" } end,
          desc = "Dotfiles",
        }

        maps.n["<Leader>fw"] = { function() require("fzf-lua").grep_cword() end, desc = "Find current word" }
        maps.n["<Leader>fW"] = {
          function() require("fzf-lua").grep_cword { cwd = vim.fn.expand "%:p:h" } end,
          desc = "Find current word(cur_dir)",
        }

        maps.n["<Leader>fs"] = { function() require("fzf-lua").live_grep() end, desc = "Search" }
        maps.n["<Leader>fS"] = {
          function() require("fzf-lua").live_grep { cwd = vim.fn.expand "%:p:h" } end,
          desc = "Search(current directory)",
        }
        maps.x["<Leader>fs"] = { function() require("fzf-lua").grep_visual() end, desc = "Search" }
        maps.x["<Leader>fS"] = {
          function() require("fzf-lua").grep_visual { cwd = vim.fn.expand "%:p:h" } end,
          desc = "Search(current directory)",
        }
        maps.n["<Leader>fT"] = { function() require("fzf-lua").colorschemes() end, desc = "Find themes" }
        maps.n["<Leader>fu"] = { function() require("fzf-lua").undotree() end, desc = "undotree" }
        maps.n["<Leader>fz"] = { function() require("fzf-lua").zoxide() end, desc = "zoxide" }
      end,
    },
  },
}
