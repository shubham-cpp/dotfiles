local rg_cmd = {
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
    enabled = vim.g.lazyvim_picker == "fzf",
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
        LazyVim.pick("files", { find_command = rg_cmd, cwd = vim.fn.expand("~/Documents/dotfiles") }),
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
        formatter = { "path.filename_first", 2 },
        keymap = {
          fzf = {
            ["ctrl-d"] = "preview-page-down",
            ["ctrl-u"] = "preview-page-up",
          },
        },
      },
      files = {
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
          winopts = {
            height = 0.55,
            width = 0.65,
            row = 0.52,
            col = 0.47,
          },
          previewer = false,
        },
        bcommits = {
          winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
        },
        commits = {
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
        multiprocess = true,
        rg_glob = true,
        glob_flah = "--glob",
        glob_separator = "%s%-%-",
        rg_glob_fn = function(query, opts)
          -- this enables all `rg` arguments to be passed in after the `--` glob separator
          local search_query, glob_str = query:match("(.*)" .. opts.glob_separator .. "(.*)")
          local glob_args = glob_str:gsub("^%s+", ""):gsub("-", "%-") .. " "

          return search_query, glob_args
        end,
      },
      lsp = {
        symbols = {
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
  {
    "ibhagwan/fzf-lua",
    cmd = "FzfLua",
    enabled = vim.g.lazyvim_picker == "fzf",
    opts = function(_, opts)
      local fzf = require("fzf-lua")
      local config = fzf.config
      local actions = fzf.actions

      local keys = {
        ["alt-enter"] = actions.file_tabedit,
        ["ctrl-t"] = actions.file_tabedit,
        ["ctrl-x"] = actions.file_split,
        ["ctrl-i"] = actions.toggle_ignore,
      }
      opts.git = opts.git or {}
      for _, sub_map in pairs({ "files", "status", "bcommits", "commits" }) do
        for key, binding in pairs(keys) do
          opts.git[sub_map] = vim.tbl_deep_extend("force", opts.git[sub_map] or {}, { actions = { [key] = binding } })
        end
        if LazyVim.has("trouble.nvim") then
          opts.git[sub_map].actions["ctrl-d"] = require("trouble.sources.fzf").actions.open
        end
      end
      opts.lsp = opts.lsp or {}
      for _, sub_map in pairs({ "declarations", "definitions", "references", "symbols" }) do
        for key, binding in pairs(keys) do
          opts.lsp[sub_map] = vim.tbl_deep_extend("force", opts.lsp[sub_map] or {}, { actions = { [key] = binding } })
        end
        if LazyVim.has("trouble.nvim") then
          opts.lsp[sub_map].actions["ctrl-d"] = require("trouble.sources.fzf").actions.open
        end
      end

      for key, binding in pairs(keys) do
        opts.files = vim.tbl_deep_extend("force", opts.files, { actions = { [key] = binding } })
      end
      if LazyVim.has("trouble.nvim") then
        opts.files.actions["ctrl-d"] = require("trouble.sources.fzf").actions.open
      end
      opts.files.actions["alt-i"] = false
      opts.files.actions["alt-h"] = false

      -- opts.buffers = opts.buffers or { actions = {} }
      -- for key, binding in pairs(keys) do
      --   opts.buffers.actions[key] = binding
      -- end
      -- opts.buffers.actions["ctrl-d"] = { fn = actions.buf_del, reload = true }

      opts.grep = opts.grep or { actions = {} }
      for key, binding in pairs(keys) do
        opts.grep.actions[key] = binding
      end
      opts.grep.actions["ctrl-g"] = actions.grep_lgrep
    end,
  },
}
