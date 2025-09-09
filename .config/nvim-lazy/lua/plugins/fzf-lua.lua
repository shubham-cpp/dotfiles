---@type LazySpec
return {
  "ibhagwan/fzf-lua",
  dependencies = "echasnovski/mini.icons",
  cmd = "FzfLua",
  opts = function()
    local actions = require("fzf-lua.actions")
    -- local config = require("fzf-lua").config

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

    -- config.set_action_helpstr(config.defaults.actions.files["ctrl-r"], "toggle-root-dir")

    return {
      { "border-fused", "hide" },
      defaults = {
        formatter = { "path.filename_first", 2 },
        fzf_opts = { ["--scheme"] = "default" },
        -- actions = {
        --   files = {
        --     ["ctrl-r"] = function(_, ctx)
        --       local o = vim.deepcopy(ctx.__call_opts)
        --       o.root = o.root == false
        --       o.cwd = nil
        --       o.buf = ctx.__CTX.bufnr
        --       LazyVim.pick.open(ctx.__INFO.cmd, o)
        --     end,
        --   },
        -- },
        keymap = {
          fzf = {
            ["ctrl-q"] = "select-all+accept",
            ["ctrl-u"] = "half-page-up",
            ["ctrl-d"] = "half-page-down",
            ["ctrl-x"] = "jump",
            ["ctrl-f"] = "preview-page-down",
            ["ctrl-b"] = "preview-page-up",
          },
          builtin = {
            ["<c-f>"] = "preview-page-down",
            ["<c-b>"] = "preview-page-up",
          },
        },
      },
      winopts = { preview = { layout = "vertical" } },
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
          local regex, flags = query:match("^(.-)%s%-%-(.*)$")
          -- If no separator is detected will return the original query
          return (regex or query), flags
        end,
      },
    }
  end,
  keys = {
    {
      "<c-p>",
      "<cmd>FzfLua files<cr>",
      desc = "Find files",
    },

    {
      "<Leader>fb",
      function()
        require("fzf-lua").buffers({})
      end,
      desc = "Find git files",
    },

    {
      "<Leader>fg",
      function()
        require("fzf-lua").git_files({})
      end,
      desc = "Find git files",
    },
    {
      "<Leader>fG",
      function()
        require("fzf-lua").live_grep({
          cmd = "git grep -i --line-number --column --color=always",
          fn_transform_cmd = function(query, cmd, _)
            local search_query, glob_str = query:match("(.-)%s-%-%-(.*)")
            if not glob_str then
              return
            end
            local new_cmd = string.format("%s %s %s", cmd, vim.fn.shellescape(search_query), glob_str)
            return new_cmd, search_query
          end,
        })
      end,
      desc = "Git grep",
    },

    {
      "<Leader>fn",
      function()
        require("fzf-lua").files({
          cwd = vim.fn.stdpath("config"),
        })
      end,
      desc = "Find AstroNvim config files",
    },
    {
      "<Leader>fd",
      function()
        require("fzf-lua").git_files({
          cwd = vim.fn.expand("~/Documents/dotfiles/"),
        })
      end,
      desc = "Dotfiles",
    },
  },
}
