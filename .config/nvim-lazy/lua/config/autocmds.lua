vim.g.snacks_animate = false
vim.cmd.packadd("cfilter")

local function augroup(name)
  return vim.api.nvim_create_augroup("sp_" .. name, { clear = true })
end

local au_buffer = augroup("buffer")
local au_mini = augroup("mini_mappings")

vim.filetype.add({
  extension = {
    fish = "fish",
    ocaml = "ocaml",
    rasi = "rasi",
    roc = "roc",
  },
  filename = {
    vimfrc = "vim",
    dwm_sxhkdrc = "sxhkdrc",
    [".env"] = "conf",
    [".env.*"] = "conf",
    ["package.json"] = "jsonc",
  },
  pattern = {
    ["*profile"] = "sh",
    ["*.postcss"] = "css",
    ["*.kbd"] = "lisp",
    [".eslintrc"] = "jsonc",
    ["tsconfig.*.json"] = "jsonc",
    [".*/hyprland%.conf"] = "hyprlang",
  },
})

vim.api.nvim_create_autocmd("FileType", {
  group = au_buffer,
  desc = "Fix Comment Continuation",
  callback = function()
    vim.opt_local.formatoptions = "jcrqlnt"
  end,
})

vim.api.nvim_create_autocmd("BufReadPost", {
  group = au_buffer,
  desc = "set env ESLINT_D_PPID",
  callback = function()
    --- If this is not present, then eslint_d doesn't work
    vim.env.ESLINT_D_PPID = vim.fn.getpid()
  end,
  once = true,
})

---{{{ Mini.files mappings
vim.api.nvim_create_autocmd("User", {
  group = au_mini,
  desc = "Create mappings to select target window",
  pattern = "MiniFilesBufferCreate",
  callback = function(args)
    local buf_id = args.data.buf_id
    local files = require("mini.files")

    vim.keymap.set("n", "W", function()
      local win_id = require("window-picker").pick_window()
      if win_id then
        files.set_target_window(win_id)
        files.go_in({ close_on_file = true })
      end
    end, { desc = "Select window", buffer = buf_id })
    vim.keymap.set("n", "gw", function()
      local win_id = require("window-picker").pick_window()
      if win_id then
        files.set_target_window(win_id)
        files.go_in()
      end
    end, { desc = "Select window(no close)", buffer = buf_id })
  end,
})

vim.api.nvim_create_autocmd("User", {
  group = au_mini,
  pattern = "MiniFilesBufferUpdate",
  desc = "Integrate with picker",
  callback = function(args)
    local buf_id = args.data.buf_id
    local files = require("mini.files")
    local ok, picker = pcall(require, "snacks.picker")

    if not ok then
      return
    end

    vim.keymap.set("n", "gf", function()
      local entry = files.get_fs_entry() or {}
      files.close()

      local cwd = entry.path
      if not cwd then
        vim.notify("Invalid path", vim.log.levels.ERROR, { title = "MiniFiles" })
      end
      if entry.fs_type == "file" then
        cwd = vim.fs.dirname(entry.path)
      end

      picker.files({ cwd = cwd, layout = { preset = "vscode" } })
    end, { desc = "Files Dir", buffer = buf_id })

    vim.keymap.set("n", "gs", function()
      local entry = files.get_fs_entry() or {}
      files.close()

      local cwd = entry.path
      if not cwd then
        vim.notify("Invalid path", vim.log.levels.ERROR, { title = "MiniFiles" })
      end
      if entry.fs_type == "file" then
        cwd = vim.fs.dirname(entry.path)
      end

      picker.grep({ cwd = cwd })
    end, { desc = "Grep dir", buffer = buf_id })
  end,
})
---}}}
