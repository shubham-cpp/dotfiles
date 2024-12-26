local rg_cmd =
  -- 'rg --files -l ".*" --follow --color=never --sortr=modified -g "!.git/" -g "!*.png" -g "!node_modules/" -g "!*.jpeg" -g "!*.jpg" -g "!*.ico" -g "!*.exe" -g "!*.out"'
  {
    "rg",
    "--files",
    "--color",
    "never",
    "--ignore",
    "--hidden",
    "--sortr",
    "modified",
    "--glob",
    "!{" .. vim.iter(vim.opt.wildignore:get()):join(",") .. "}",
  }

---@type LazySpec
return {
  {
    "ibhagwan/fzf-lua",
    keys = {
      {
        "<c-p>",
        function()
          LazyVim.pick.open("files", {
            find_command = rg_cmd,
          })
        end,
        desc = "Pick files",
      },
      { "<leader>fn", LazyVim.pick.config_files(), desc = "Find Config File" },
      {
        "<leader>fd",
        LazyVim.pick("files", { cwd = vim.fn.expand("~/Documents/dotfiles/.config/") }),
        desc = "Find Dotfiles",
      },
      {
        "<leader>fN",
        LazyVim.pick("files", {
          cwd = vim.fn.stdpath("data") .. "/lazy/LazyVim",
          find_command = rg_cmd,
        }),
        desc = "Find Data files",
      },
      {
        "<leader>gB",
        function()
          require("fzf-lua").git_branches()
        end,
        desc = "Git Branch",
      },
      {
        "<leader>f/",
        function()
          require("fzf-lua").lgrep_curbuf({
            prompt = "Buffer❫ ",
          })
        end,
        desc = "Grep buffer",
      },

      {
        "<C-x><C-k>",
        function()
          require("fzf-lua").complete_bline()
        end,
        mode = "i",
        desc = "Complete bline",
        silent = true,
      },
      {
        "<C-x><C-l>",
        function()
          require("fzf-lua").complete_line()
        end,
        mode = "i",
        desc = "Complete line",
        silent = true,
      },
      {
        "<C-x><C-f>",
        function()
          require("fzf-lua").complete_path()
        end,
        mode = "i",
        desc = "Complete path",
        silent = true,
      },
    },
    opts = {
      defaults = {
        keymap = {
          fzf = {
            ["ctrl-d"] = "preview-page-down",
            ["ctrl-u"] = "preview-page-up",
          },
        },
      },
      files = {
        formatter = "path.filename_first",
        fzf_opts = {
          ["--layout"] = "reverse",
          ["--tiebreak"] = "length",
        },
        winopts = {
          height = 0.55,
          width = 0.65,
          row = 0.52,
          col = 0.47,
          preview = {
            ---@type 'wrap'|'nowrap'
            wrap = "nowrap",
            ---@type 'hidden'|'nohidden'
            hidden = "hidden",
          },
        },
      },
      git = {
        files = {
          cmd = "git ls-files --exclude-standard --cached --others", -- '--others' is used to show untracked files
          -- actions = m_keys,
          winopts = {
            height = 0.55,
            width = 0.65,
            row = 0.52,
            col = 0.47,
          },
          previewer = false,
        },
        bcommits = {
          -- actions = m_keys,
          winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
        },
        commits = {
          -- actions = m_keys,
          winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
        },
        branches = {
          winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
          cmd = "git branch --all --color | sed 's#remotes/origin/##g'",
          cmd_add = { "git", "checkout", "-b" },
        },
      },
      buffers = {
        ignore_current_buffer = true,
        winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
      },
      diagnostics = {
        winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
      },
      grep = {
        winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
        -- actions = m_keys,
        rg_glob = true,
        glob_flah = "--glob",
        glob_separator = "%s%-%-",
      },
      lsp = {
        smbols = {
          winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
        },
        finder = {
          winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
        },
        code_actions = {
          winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
        },
      },
    },
  },
  -- {
  --   "ibhagwan/fzf-lua",
  --   opts = function()
  --     local actions = require("fzf-lua.actions")
  --     local keys = {
  --       ["alt-enter"] = actions.file_tabedit,
  --       ["ctrl-t"] = actions.file_tabedit,
  --       ["ctrl-x"] = actions.file_split,
  --     }
  --     return {
  --       files = { actions = keys },
  --       buffers = { actions = keys },
  --       grep = { actions = keys },
  --       git = {
  --         files = { actions = keys },
  --         status = { actions = keys },
  --         bcommits = { actions = keys },
  --         commits = { actions = keys },
  --       },
  --       lsp = {
  --         declarations = { actions = keys },
  --         definitions = { actions = keys },
  --         references = { actions = keys },
  --         symbols = { actions = keys },
  --       },
  --     }
  --   end,
  -- },
}
