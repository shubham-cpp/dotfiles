require("mini.files").setup({
  mappings = {
    go_in = "L",
    go_in_plus = "l",
  },
  options = { permanent_delete = false },
})

local toggle_files = function(path)
  if not MiniFiles.close() then
    MiniFiles.open(path)
  end
end

vim.keymap.set("n", "<Leader>e", function()
  local path = vim.api.nvim_buf_get_name(0)
  local ok = pcall(toggle_files, path ~= "" and path or nil)
  if not ok then
    toggle_files(vim.uv.cwd())
  end
end, { desc = "Explorer Current File" })

vim.keymap.set("n", "<Leader>E", function()
  toggle_files(vim.uv.cwd())
end, { desc = "Explorer Cwd" })

local map_split = function(buf_id, lhs, direction, desc)
  vim.keymap.set("n", lhs, function()
    local state = MiniFiles.get_explorer_state()
    if state == nil then
      return
    end

    local new_target = vim.api.nvim_win_call(state.target_window, function()
      vim.cmd(direction .. " split")
      return vim.api.nvim_get_current_win()
    end)

    MiniFiles.set_target_window(new_target)
    MiniFiles.go_in({ close_on_file = true })
  end, { buffer = buf_id, desc = desc })
end

-- Yank in register full path of entry under cursor
local yank_path = function()
  local path = (MiniFiles.get_fs_entry() or {}).path
  if path == nil then
    return vim.notify "Cursor is not on valid entry"
  end
  vim.fn.setreg(vim.v.register, path)
end

vim.api.nvim_create_autocmd("User", {
  group = vim.api.nvim_create_augroup("ConfigMini", { clear = true }),
  desc = "Setup mappings for mini.files",
  pattern = "MiniFilesBufferCreate",
  callback = function(args)
    local buf = args.data.buf_id
    map_split(buf, "<C-w>s", "belowright horizontal", "Open in horizontal split")
    map_split(buf, "<C-w>v", "belowright vertical", "Open in vertical split")

    vim.keymap.set("n", "gX", function()
      vim.ui.open(MiniFiles.get_fs_entry().path)
    end, { buffer = buf, desc = "OS open" })
    vim.keymap.set("n", "gy", yank_path, { buffer = buf, desc = "Yank path" })
  end,
})
