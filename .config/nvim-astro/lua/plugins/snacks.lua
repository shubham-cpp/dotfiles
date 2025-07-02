local branch = nil
vim.g.last_buffer = nil

---@param picker snacks.Picker
local function explorer_add_in_parent(picker)
  local Tree = require "snacks.explorer.tree"
  local uv = vim.uv or vim.loop
  local default_dir = ""
  local parent = picker:current().parent
  if parent.dir == false then
    default_dir = picker:cwd()
  else
    default_dir = parent.file
  end
  require("snacks").input({
    prompt = 'Add a new file or directory (directories end with a "/")',
    default = default_dir .. "/",
  }, function(value)
    if not value or value:find "^%s$" then return end
    local path = vim.fs.normalize(value)
    local is_file = value:sub(-1) ~= "/"
    local dir = is_file and vim.fs.dirname(path) or path
    if is_file and uv.fs_stat(path) then
      require("snacks").notify.warn("File already exists:\n- `" .. path .. "`")
      return
    end
    vim.fn.mkdir(dir, "p")
    if is_file then io.open(path, "w"):close() end
    Tree:open(dir)
    Tree:refresh(dir)
    picker.update(picker, { target = path })
  end)
end

---@param picker snacks.Picker
local function copy_path_full(picker)
  local selected = picker:selected({ fallback = true })[1]
  if not selected or selected == nil then return end
  vim.schedule(function()
    local full_path = vim.fn.fnamemodify(selected.file, ":p")
    vim.fn.setreg(vim.v.register, full_path)
    vim.notify(full_path, vim.log.levels.INFO, { title = "File Path Copied" })
  end)
end

---@param picker snacks.Picker
local function copy_path_relative(picker)
  ---@type string[]
  local paths = vim.tbl_map(require("snacks").picker.util.path, picker:selected { fallback = true })
  if #paths == 0 then
    vim.notify(
      "No files selected to move",
      vim.log.levels.WARN,
      { title = "Invalid use of `copy_path_relative` function" }
    )
    return
  end
  if #paths ~= 2 and vim.g.last_buffer == nil then
    vim.notify(
      "Exactly two files need to be selected or `last_buffer` needs to be defined",
      vim.log.levels.WARN,
      { title = "Invalid use of `copy_path_relative` function" }
    )
    return
  end

  local from_file = paths[2]
  local to_file = paths[1]
  if from_file == nil then
    from_file = paths[1]
    to_file = vim.g.last_buffer
  end
  local script = vim.fn.expand "~/.local/bin/myscripts/cal_relative_path.py"
  local cmd = { script, from_file, to_file }

  local on_done = function(obj)
    if obj.stderr ~= "" or obj.stdout == "" then
      require("snacks").notify.warn("Some error while calculating relative paths " .. obj.stderr)
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
    ---@type snacks.Config
    opts = {
      input = {},
      bigfile = {},
      lazygit = {},
      notifier = {},
      explorer = { enabled = false },
      picker = {
        enabled = true,
        layout = { preset = "dropdown" },
        matcher = { frecency = true, history_bonus = true },
        ---@class snacks.picker.formatters.Config
        formatters = { file = { filename_first = true } },
        sources = {
          explorer = {
            enabled = false,
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
      {
        "<leader>gg",
        function() require("snacks").lazygit() end,
        desc = "Toggle Lazygit",
      },
      {
        "<C-w>m",
        function() require("snacks").zen.zoom() end,
        desc = "Toggle Zoom",
      },
      {
        "<Leader><Leader>",
        function()
          local is_git = vim.g.gitsigns_head or vim.b.gitsigns_head
          if is_git then
            require("snacks").picker.git_files { layout = { preset = "vscode" } }
          else
            require("snacks").picker.files { layout = { preset = "vscode" } }
          end
        end,
        desc = "Find files",
      },
      {
        "<C-p>",
        function() require("snacks").picker.files { layout = { preset = "vscode" } } end,
        desc = "Find Files",
      },
      {
        "<leader>ff",
        function() require("snacks").picker.files { layout = { preset = "vscode" } } end,
        desc = "Find Files",
      },
      {
        "<leader>fF",
        function() require("snacks").picker.files { layout = { preset = "vscode" }, hidden = true, ignored = true } end,
        desc = "Find all files",
      },
      {
        "<leader>fn",
        function() require("snacks").picker.files { cwd = vim.fn.stdpath "config", layout = { preset = "vscode" } } end,
        desc = "Find Config File",
      },
      {
        "<leader>fN",
        function() require("snacks").picker.notifications() end,
        desc = "Notifications",
      },
      {
        "<leader>fd",
        function()
          require("snacks").picker.files {
            cwd = vim.fn.expand "~/Documents/dotfiles/.config",
            layout = { preset = "vscode" },
          }
        end,
        desc = "Find Dotfiles",
      },
      {
        "<leader>fg",
        function() require("snacks").picker.git_files { untracked = true, layout = { preset = "vscode" } } end,
        desc = "Find Git Files",
      },
      {
        "<leader>fo",
        function() require("snacks").picker.smart() end,
        desc = "Find buffers/recent/files",
      },
      {
        "<leader>fr",
        function() require("snacks").picker.resume() end,
        desc = "Resume",
      },
      {
        "<leader>fO",
        function() require("snacks").picker.recent { layout = { preset = "vertical" } } end,
        desc = "Old Files",
      },
      {
        "<leader>f,",
        function() require("snacks").picker.recent { layout = { preset = "vertical" }, filter = { cwd = true } } end,
        desc = "Old Files(cwd)",
      },
      {
        "<leader>fz",
        function() require("snacks").picker.zoxide { layout = { preset = "vertical" } } end,
        desc = "Zoxided",
      },
      {
        "<leader>fs",
        function() require("snacks").picker.grep() end,
        desc = "Search/Grep",
      },
      {
        "<leader>fs",
        function() require("snacks").picker.grep { cwd = vim.fn.expand "%:p:h" } end,
        desc = "Search/Grep",
      },
      {
        "<leader>fB",
        function() require("snacks").picker.grep_buffers() end,
        desc = "Search/Grep in Open Buffers",
      },
      {
        "<leader>fw",
        function() require("snacks").picker.grep_word() end,
        desc = "Visual selection or word",
        mode = { "n", "x" },
      },
      {
        "<leader>fW",
        function() require("snacks").picker.grep_word { cwd = vim.fn.expand "%:p:h" } end,
        desc = "Visual selection or word",
        mode = { "n", "x" },
      },
      { "<leader>fu", function() require("snacks").picker.undo() end, desc = "Undotree" },
      { "<Leader>fH", function() require("snacks").picker.highlights() end, desc = "highlights" },
      { "<Leader>ft", function() require("snacks").picker.todo_comments() end, desc = "Todo comments" },
      {
        "<Leader>fT",
        function() require("snacks").picker.todo_comments { keywords = { "TODO", "FIX", "FIXME" } } end,
        desc = "Todo/Fix/Fixme",
      },
      -- {
      --   "<leader>e",
      --   function()
      --     local last_buffer_path = vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf())
      --     if
      --       last_buffer_path ~= ""
      --       and string.match(last_buffer_path, "nvim/runtime/doc/") == nil
      --       and string.match(last_buffer_path, "quickfix%-%d+") == nil
      --     then
      --       vim.g.last_buffer = last_buffer_path
      --     end
      --     require('snacks').explorer()
      --   end,
      --   desc = "Explorer",
      -- },
    },
    dependencies = {
      {
        "AstroNvim/astrocore",
        opts = {
          mappings = {
            n = {
              ["<Leader>un"] = {
                function() require("snacks.notifier").show_history() end,
                desc = "Notification History",
              },
            },
          },
        },
      },
    },
  },
}
