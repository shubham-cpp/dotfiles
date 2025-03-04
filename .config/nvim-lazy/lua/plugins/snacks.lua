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
  "folke/snacks.nvim",
  optional = true,
  ---@type snacks.Config
  opts = {
    dashboard = { enabled = false },
    picker = {
      enabled = true,
      ui_select = true,
      layout = { preset = "dropdown" },
      matcher = { frecency = true, history_bonus = true },
      formatters = { file = { filename_first = true } },
      sources = {
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
          },
        },
      },
      win = {
        -- input window
        input = {
          keys = {
            ["<c-x>"] = { "edit_split", mode = { "i", "n" } },
            ["<c-t>"] = { "edit_tab", mode = { "i", "n" } },
            ["<c-c>"] = { "copy", mode = { "i", "n" } },
          },
        },
        list = { keys = { ["<c-x>"] = "edit_split" } },
      },
    },
  },
  keys = {
    {
      "<C-p>",
      function()
        local branch = vim.b.gitsigns_head or nil
        if branch ~= nil and branch ~= "" then
          Snacks.picker.git_files({ layout = { preset = "vscode" }, untracked = true })
        else
          Snacks.picker.files({ layout = { preset = "vscode" } })
        end
      end,
      desc = "Find Files",
    },
    {
      "<leader>ff",
      function()
        Snacks.picker.files({ layout = { preset = "vscode" } })
      end,
      desc = "Find Files",
    },
    {
      "<leader>fg",
      function()
        Snacks.picker.git_files({ untracked = true, layout = { preset = "vscode" } })
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
        Snacks.picker.files({ cwd = vim.fn.stdpath("data") .. "/lazy", layout = { preset = "vscode" } })
      end,
      desc = "Neovim Data dir",
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
    {
      "<C-\\>",
      function()
        Snacks.terminal(nil, { win = { style = "float", border = "rounded" } })
      end,
      desc = "Toggle terminal",
    },
    {
      "<C-\\>",
      "<cmd>close<cr>",
      mode = "t",
      desc = "Hide terminal",
    },
  },
}
