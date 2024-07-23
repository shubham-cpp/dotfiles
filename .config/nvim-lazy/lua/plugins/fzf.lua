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
      ["default"] = function(selected)
        local fullpath = get_full_path(selected)
        vim.ui.input({ prompt = "File Name: " }, function(name)
          if name == nil then
            return
          end
          vim.cmd("e " .. fullpath .. name)
          vim.cmd("w ++p")
        end)
      end,
    },
  })
end

---@type LazySpec
return {
  "ibhagwan/fzf-lua",
  keys = {
    {
      "<C-p>",
      LazyVim.pick("files"),
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
  opts = {
    defaults = { formatter = "path.filename_first" },
    fzf_opts = {
      ["--layout"] = "reverse",
      ["--info"] = "inline-right",
      -- ['--tiebreak'] = 'end',
    },
    files = {
      fzf_opts = {
        ["--layout"] = "reverse",
        ["--tiebreak"] = "chunk",
      },
      winopts = {
        height = 0.55,
        width = 0.65,
        row = 0.52,
        col = 0.47,
      },
      -- preview = { default = false, horizontal = 'right:45%' },
      previewer = false,
      -- actions = {
      --
      -- },
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
    grep = {
      winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
      -- actions = m_keys,
      rg_glob = true,
      glob_flah = "--glob",
      glob_separator = "%s%-%-",
    },
    blines = {
      -- actions = m_keys,
      no_term_buffers = false,
      winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
    },
    lsp = {
      definitions = {
        jump_to_single_result = true,
      },
      references = {
        ignore_current_line = true,
      },
      symbols = {
        -- actions = m_keys,
        winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
      },
      finder = {
        -- actions = m_keys,
        winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
      },
      code_actions = {
        -- actions = m_keys,
        winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
      },
    },
  },
}
