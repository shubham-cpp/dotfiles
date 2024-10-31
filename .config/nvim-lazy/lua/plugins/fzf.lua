local rg_cmd =
  'rg --files -l ".*" --follow --color=never --sortr=modified -g "!.git/" -g "!*.png"  -g "!node_modules/" -g "!*.jpeg" -g "!*.jpg" -g "!*.ico" -g "!*.exe" -g "!*.out"'

local function fzf_create_file()
  local fzf = require("fzf-lua")
  local path = require("fzf-lua.path")
  local uv = vim.uv or vim.loop
  local cmd = "fd -t d . " .. uv.cwd()
  local function get_full_path(selected)
    if #selected < 1 then
      return
    end
    local entry = path.entry_to_file(selected[1], { cwd = uv.cwd() })
    if entry.path == "<none>" then
      return
    end

    local fullpath = entry.path or entry.uri and entry.uri:match("^%a+://(.*)")
    if not path.is_absolute(fullpath) then
      fullpath = path.join({ uv.cwd(), fullpath })
    end
    return fullpath
  end

  --- This will create a file in the selected directory
  ---@param split_dir ?"e" | "vs" | "sp" | "tabe" Default is "e"
  ---@return function
  local function perform_action(split_dir)
    split_dir = split_dir or "e"
    return function(selected)
      local fullpath = get_full_path(selected)
      vim.ui.input({ prompt = "File Name: " }, function(name)
        if name == nil then
          return
        end
        vim.cmd(string.format("%s %s%s", split_dir, fullpath, name))
        vim.cmd("w ++p")
      end)
    end
  end

  fzf.fzf_exec(cmd, {
    defaults = {},
    prompt = "Create> ",
    cwd = uv.cwd(),
    cwd_prompt_shorten_len = 32,
    cwd_prompt_shorten_val = 1,
    fzf_opts = {
      ["--tiebreak"] = "end",
      ["--preview-window"] = "nohidden,50%",
      ["--preview"] = {
        type = "cmd",
        fn = function(selected)
          local fullpath = get_full_path(selected)
          local ls_cmd = "command ls --color -hsv1F --group-directories-first"
          local eza_cmd =
            "eza -al --color=always --icons=always --group-directories-first --no-user --no-permissions --no-time"
          return string.format("%s %s", vim.fn.executable("eza") == 1 and eza_cmd or ls_cmd, fullpath)
        end,
      },
    },
    fn_transform = function(x)
      return fzf.make_entry.file(x, { file_icons = true, color_icons = true, cwd = uv.cwd() })
    end,
    actions = {
      ["default"] = perform_action(),
      ["ctrl-x"] = perform_action("sp"),
      ["ctrl-v"] = perform_action("vs"),
      ["ctrl-t"] = perform_action("tabe"),
    },
  })
end

local function project_files(default_opts)
  local opts = vim.tbl_extend("force", {
    fzf_opts = {
      ["--layout"] = "reverse",
      ["--info"] = "inline-right",
      ["--tiebreak"] = "index",
    },
  }, default_opts or {})
  local fzf = require("fzf-lua")
  if vim.b.gitsigns_head then
    -- Either use one of the following .local/bin/myscripts/sort_file.rs or .local/bin/myscripts/sorting_filev3.cpp
    -- compile and then add to `PATH`
    --`sort_files` is a program that sorts files based on modified time, recently modified files will be shown first
    if vim.fn.executable("sort_files") == 1 then
      opts.cmd = "git ls-files --exclude-standard --cached --others | sort_files" -- '--others' is used to show untracked files
    else
      vim.cmd('echohl WarningMsg | echo "`sort_files` not found in `PATH`. Please compile the program" | echohl None')
    end
    fzf.git_files(opts)
  else
    opts.cmd = rg_cmd
    fzf.files(opts)
  end
end

---@type LazySpec
return {
  "ibhagwan/fzf-lua",
  keys = {
    {
      "<C-p>",
      project_files,
      desc = "[F]iles",
    },
    {
      "<leader>fn",
      function()
        require("fzf-lua").files({ cwd = vim.fn.stdpath("config") })
      end,
      desc = "Config [N]eovim",
    },
    {
      "<leader>fd",
      function()
        require("fzf-lua").git_files({ cwd = vim.fn.expand("~/Documents/dotfiles/") })
      end,
      desc = "Config [D]otfiles",
    },
    {
      "<leader>fc",
      fzf_create_file,
      desc = "Create File",
    },
  },
  opts = function(_, opts)
    local actions = require("fzf-lua.actions")
    local m_keys = {
      ["alt-enter"] = actions.file_tabedit,
      ["ctrl-x"] = actions.file_split,
      ["ctrl-q"] = actions.file_edit_or_qf,
    }
    opts.defaults = { formatter = { "path.filename_first", 2 } }
    opts.keymap = vim.tbl_deep_extend("force", opts.keymap or {}, {
      fzf = {
        ["alt-a"] = "toggle-all",
        ["alt-p"] = "toggle-preview",
        ["alt-j"] = "preview-down",
        ["alt-k"] = "preview-up",
        ["ctrl-q"] = "select-all+accept",
      },
    })
    opts.winopts = vim.tbl_extend("force", opts.winopts or {}, {
      preview = {
        default = "bat", -- override the default previewer?
      },
    })
    opts.fzf_opts = vim.tbl_extend("force", opts.fzf_opts or {}, {
      ["--layout"] = "reverse",
      ["--info"] = "inline-right",
      -- ['--tiebreak'] = 'end',
    })
    opts.files = vim.tbl_deep_extend("force", opts.files or {}, {
      fzf_opts = { ["--layout"] = "reverse", ["--tiebreak"] = "chunk" },
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
      actions = m_keys,
    })
    opts.git = vim.tbl_deep_extend("force", opts.git or {}, {
      files = {
        cmd = "git ls-files --exclude-standard --cached --others", -- '--others' is used to show untracked files
        actions = m_keys,
        winopts = { height = 0.55, width = 0.65, row = 0.52, col = 0.47 },
        previewer = false,
      },
      bcommits = { actions = m_keys, winopts = { preview = { layout = "vertical", vertical = "up:60%" } } },
      commits = { actions = m_keys, winopts = { preview = { layout = "vertical", vertical = "up:60%" } } },
      branches = {
        winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
        cmd = "git branch --all --color | sed 's#remotes/origin/##g'",
        cmd_add = { "git", "checkout", "-b" },
      },
    })
    opts.buffers = vim.tbl_deep_extend("force", opts.buffers or {}, {
      ignore_current_buffer = true,
      winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
      actions = vim.tbl_extend("force", m_keys, {
        ["ctrl-d"] = actions.buf_delete,
        ["ctrl-x"] = actions.buf_split,
        ["ctrl-v"] = actions.buf_vsplit,
        ["ctrl-q"] = actions.buf_edit_or_qf,
      }),
    })
    opts.grep = vim.tbl_deep_extend("force", opts.grep or {}, {
      winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
      actions = m_keys,
      rg_glob = true,
      glob_flah = "--glob",
      glob_separator = "%s%-%-",
    })
    opts.blines = vim.tbl_deep_extend("force", opts.blines or {}, {
      actions = m_keys,
      no_term_buffers = false,
      winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
    })
    opts.lines = vim.tbl_deep_extend("force", opts.lines or {}, {
      actions = m_keys,
      winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
    })
    opts.lsp = vim.tbl_deep_extend("force", opts.lsp or {}, {
      definitions = { jump_to_single_result = true, actions = m_keys },
      references = { ignore_current_line = true, actions = m_keys },
      symbols = { actions = m_keys, winopts = { preview = { layout = "vertical", vertical = "up:60%" } } },
      finder = { actions = m_keys, winopts = { preview = { layout = "vertical", vertical = "up:60%" } } },
      code_actions = { actions = m_keys, winopts = { preview = { layout = "vertical", vertical = "up:60%" } } },
    })

    local ok_dressing, _ = pcall(require, "dressing")
    if not ok_dressing then
      require("fzf-lua").register_ui_select()
    end
  end,
}
