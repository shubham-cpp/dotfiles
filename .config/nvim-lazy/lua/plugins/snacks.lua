---@param picker snacks.Picker
local function explorer_add_in_parent(picker)
  local Tree = require("snacks.explorer.tree")
  local uv = vim.uv or vim.loop
  local default_dir = ""
  local parent = picker:current().parent
  if parent.dir == false then
    default_dir = picker:cwd()
  else
    default_dir = parent.file
  end
  Snacks.input({
    prompt = 'Add a new file or directory (directories end with a "/")',
    default = default_dir .. "/",
  }, function(value)
    if not value or value:find("^%s$") then
      return
    end
    local path = vim.fs.normalize(value)
    local is_file = value:sub(-1) ~= "/"
    local dir = is_file and vim.fs.dirname(path) or path
    if is_file and uv.fs_stat(path) then
      Snacks.notify.warn("File already exists:\n- `" .. path .. "`")
      return
    end
    vim.fn.mkdir(dir, "p")
    if is_file then
      io.open(path, "w"):close()
    end
    Tree:open(dir)
    Tree:refresh(dir)
    picker.update(picker, { target = path })
  end)
end

---@param picker snacks.Picker
local function copy_path_full(picker)
  local selected = picker:selected({ fallback = true })[1]
  if not selected or selected == nil then
    return
  end
  vim.schedule(function()
    local full_path = vim.fn.fnamemodify(selected.file, ":p")
    vim.fn.setreg(vim.v.register, full_path)
    vim.notify(full_path, vim.log.levels.INFO, { title = "File Path Copied" })
  end)
end

---@param picker snacks.Picker
local function copy_path_relative(picker)
  ---@type string[]
  local paths = vim.tbl_map(Snacks.picker.util.path, picker:selected())
  if #paths == 0 then
    vim.notify(
      "No files selected to move. Renaming instead.",
      vim.log.levels.WARN,
      { title = "Invalid use of `copy_path_relative` function" }
    )
    return
  end
  if #paths ~= 2 then
    vim.notify(
      "Exactly two files need to be selected.",
      vim.log.levels.WARN,
      { title = "Invalid use of `copy_path_relative` function" }
    )
    return
  end

  local from_file = paths[2]
  local to_file = paths[1]
  local script = vim.fn.expand("~/.local/bin/myscripts/cal_relative_path.py")
  local cmd = { script, from_file, to_file }

  local on_done = function(obj)
    if obj.stderr ~= "" or obj.stdout == "" then
      Snacks.notify.warn("Some error while calculating relative paths " .. obj.stderr)
      return
    end
    vim.fn.setreg(vim.v.register, obj.stdout)
    vim.notify(obj.stdout, vim.log.levels.INFO, { title = "File Path Copied" })
  end

  local obj = vim.system(cmd, { text = true }):wait()
  on_done(obj)
end
---@type LazySpec
return {
  {
    "folke/snacks.nvim",
    optional = true,
    ---@type snacks.Config
    opts = {
      dashboard = { enabled = false },
      terminal = {
        shell = vim.fn.exepath("fish") == "" and vim.o.shell or vim.fn.exepath("fish"),
      },
      picker = {
        enabled = true,
        ui_select = true,
        layout = { preset = "dropdown" },
        matcher = { frecency = true, history_bonus = true },
        formatters = { file = { filename_first = true } },
        sources = {
          buffers = {
            win = {
              input = {
                keys = {
                  ["<c-x>"] = { "edit_split", mode = { "i", "n" } },
                  ["<a-x>"] = { "bufdelete", mode = { "n", "i" } },
                },
              },
              list = { keys = { ["dd"] = "bufdelete" } },
            },
          },
          git_files = { untracked = true },
          git_grep = { untracked = true },
          explorer = {
            layout = { cycle = false, layout = { position = "right" } },
            actions = {
              explorer_add_in_parent = explorer_add_in_parent,
              copy_path_full = copy_path_full,
              copy_path_relative = copy_path_relative,
            },
            win = {
              list = {
                keys = {
                  ["/"] = false,
                  ["f"] = { "toggle_focus" },
                  ["A"] = { "explorer_add_in_parent" },
                  ["Y"] = { "copy_path_full" },
                  ["gy"] = { "copy_path_relative" },
                  ["gf"] = { "picker_files", desc = "Open File Picker" },
                  ["<leader>f"] = { "picker_files", desc = "Open File Picker" },
                },
              },
              input = {
                keys = {
                  ["<c-t>"] = { "edit_tab", mode = { "i", "n" } },
                },
              },
            },
          },
        },
        win = {
          -- input window
          input = {
            keys = {
              ["<c-u>"] = { "preview_scroll_up", mode = { "i", "n" } },
              ["<c-d>"] = { "preview_scroll_down", mode = { "i", "n" } },
              ["<c-f>"] = { "list_scroll_down", mode = { "i", "n" } },
              ["<c-b>"] = { "list_scroll_up", mode = { "i", "n" } },
              ["<c-x>"] = { "edit_split", mode = { "i", "n" } },
              ["<c-t>"] = { "edit_tab", mode = { "i", "n" } },
              ["<c-c>"] = { "copy", mode = { "i", "n" } },
            },
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
    keys = {
      ---{{{ remove defaults. My musle memory is for pickers to have <leader>f as prefix
      { "<leader>fc", false },
      { "<leader>sb", false },
      { "<leader>sB", false },
      { "<leader>sg", false },
      { "<leader>sG", false },
      { "<leader>sp", false },
      { "<leader>sw", false },
      { "<leader>sW", false },
      -- search
      { '<leader>s"', false },
      { "<leader>s/", false },
      { "<leader>sa", false },
      { "<leader>sc", false },
      { "<leader>sC", false },
      { "<leader>sd", false },
      { "<leader>sD", false },
      { "<leader>sh", false },
      { "<leader>sH", false },
      { "<leader>si", false },
      { "<leader>sl", false },
      { "<leader>sk", false },
      { "<leader>sl", false },
      { "<leader>sM", false },
      { "<leader>sm", false },
      { "<leader>sR", false },
      { "<leader>sq", false },
      { "<leader>su", false },
      { "<leader>sj", false },
      -- explorer
      { "<leader>e", false },
      { "<leader>E", false },
      ---}}}
      {
        "<Leader><Leader>",
        function()
          local is_git = vim.g.gitsigns_head or vim.b.gitsigns_head
          if is_git then
            require("snacks").picker.git_files({ layout = { preset = "vscode" } })
          else
            require("snacks").picker.files({ layout = { preset = "vscode" } })
          end
        end,
        desc = "Find files",
      },
      {
        "<C-p>",
        function()
          Snacks.picker.files({ layout = { preset = "vscode" } })
        end,
        desc = "Find Files",
      },
      {
        "<leader>ff",
        LazyVim.pick("files", { layout = { preset = "vscode" } }),
        desc = "Find Files (Root Dir)",
      },
      {
        "<leader>fF",
        LazyVim.pick("files", { layout = { preset = "vscode" }, root = false }),
        desc = "Find Files (cwd)",
      },
      {
        "<leader>fg",
        function()
          Snacks.picker.git_files({ layout = { preset = "vscode" } })
        end,
        desc = "Find Files(git)",
      },
      {
        "<leader>fn",
        function()
          Snacks.picker.files({ cwd = vim.fn.stdpath("config"), layout = { preset = "vscode" } })
        end,
        desc = "Find Config File",
      },
      {
        "<leader>fN",
        function()
          Snacks.picker.notifications()
        end,
        desc = "notifications",
      },
      {
        "<leader>fd",
        function()
          Snacks.picker.files({ cwd = vim.fn.expand("~/Documents/dotfiles/.config"), layout = { preset = "vscode" } })
        end,
        desc = "Find Dotfiles",
      },
      {
        "<leader>fz",
        function()
          Snacks.picker.zoxide()
        end,
        desc = "Zoxided",
      },
      {
        "<leader>uN",
        function()
          Snacks.notifier.show_history()
        end,
        desc = "Notification History",
      },
      {
        "<c-w>m",
        function()
          Snacks.zen.zoom()
        end,
        desc = "Toggle Zoom",
      },
      -- {
      --   "<C-\\>",
      --   function()
      --     Snacks.terminal(nil, { win = { style = "float", border = "rounded" } })
      --   end,
      --   desc = "Toggle terminal",
      -- },
      -- {
      --   "<C-\\>",
      --   "<cmd>close<cr>",
      --   mode = "t",
      --   desc = "Hide terminal",
      -- },
      {
        "<leader>fl",
        function()
          Snacks.picker.lines()
        end,
        desc = "Buffer Lines",
      },
      {
        "<leader>fG",
        function()
          Snacks.picker.grep_buffers()
        end,
        desc = "Grep Open Buffers",
      },
      { "<leader>fs", LazyVim.pick("live_grep"), desc = "Grep (Root Dir)" },
      { "<leader>fS", LazyVim.pick("live_grep", { root = false }), desc = "Grep (cwd)" },
      {
        "<leader>fL",
        function()
          Snacks.picker.lazy()
        end,
        desc = "Search for Plugin Spec",
      },
      { "<leader>fw", LazyVim.pick("grep_word"), desc = "Visual selection or word (Root Dir)", mode = { "n", "x" } },
      {
        "<leader>fW",
        LazyVim.pick("grep_word", { root = false }),
        desc = "Visual selection or word (cwd)",
        mode = { "n", "x" },
      },
      -- search
      {
        '<leader>f"',
        function()
          Snacks.picker.registers()
        end,
        desc = "Registers",
      },
      {
        "<leader>f/",
        function()
          Snacks.picker.search_history()
        end,
        desc = "Search History",
      },
      {
        "<leader>fa",
        function()
          Snacks.picker.autocmds()
        end,
        desc = "Autocmds",
      },
      {
        "<leader>fc",
        function()
          Snacks.picker.command_history()
        end,
        desc = "Command History",
      },
      {
        "<leader>fC",
        function()
          Snacks.picker.commands()
        end,
        desc = "Commands",
      },
      {
        "<leader>ld",
        function()
          Snacks.picker.diagnostics()
        end,
        desc = "Diagnostics",
      },
      {
        "<leader>lD",
        function()
          Snacks.picker.diagnostics_buffer()
        end,
        desc = "Buffer Diagnostics",
      },
      {
        "<leader>fh",
        function()
          Snacks.picker.help()
        end,
        desc = "Help Pages",
      },
      {
        "<leader>fH",
        function()
          Snacks.picker.highlights()
        end,
        desc = "Highlights",
      },
      {
        "<leader>fi",
        function()
          Snacks.picker.icons()
        end,
        desc = "Icons",
      },
      {
        "<leader>fk",
        function()
          Snacks.picker.keymaps()
        end,
        desc = "Keymaps",
      },
      {
        "<leader>fl",
        function()
          Snacks.picker.loclist()
        end,
        desc = "Location List",
      },
      {
        "<leader>fm",
        function()
          Snacks.picker.man()
        end,
        desc = "Man Pages",
      },
      {
        "<leader>fM",
        function()
          Snacks.picker.marks()
        end,
        desc = "Marks",
      },
      {
        "<leader>fr",
        function()
          Snacks.picker.resume()
        end,
        desc = "Resume",
      },
      {
        "<leader>fo",
        LazyVim.pick("oldfiles"),
        desc = "Resume",
      },
      {
        "<leader>fR",
        function()
          Snacks.picker.recent({ filter = { cwd = true } })
        end,
        desc = "Recent (cwd)",
      },
      {
        "<leader>fq",
        function()
          Snacks.picker.qflist()
        end,
        desc = "Quickfix List",
      },
      {
        "<leader>fu",
        function()
          Snacks.picker.undo()
        end,
        desc = "Undotree",
      },
      {
        "<leader>fp",
        function()
          Snacks.picker.projects()
        end,
        desc = "Projects",
      },
      {
        "<leader>fj",
        function()
          Snacks.picker.jumps()
        end,
        desc = "Jumps",
      },
    },
  },
  {
    "folke/todo-comments.nvim",
    optional = true,
    keys = {
      { "<leader>st", false },
      { "<leader>sT", false },
      {
        "<leader>ft",
        function()
          Snacks.picker.todo_comments()
        end,
        desc = "Todo",
      },
      {
        "<leader>fT",
        function()
          Snacks.picker.todo_comments({ keywords = { "TODO", "FIX", "FIXME" } })
        end,
        desc = "Todo/Fix/Fixme",
      },
    },
  },
}
