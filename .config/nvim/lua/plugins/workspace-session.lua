local gh = require("config.utils").gh

local M = {}

vim.g.barbar_auto_setup = false

vim.pack.add({
  { src = gh "romgrk/barbar.nvim", version = vim.version.range "1.*" },
  gh "tiagovla/scope.nvim",
})

require("barbar").setup({
  maximum_padding = 0,
  minimum_padding = 1,
  tabpages = true,
  animation = false,
  icons = {
    filetype = {
      enabled = true,
      custom_colors = true,
    },
  },
})

local scope_config = {
  hooks = {
    pre_tab_leave = function()
      vim.api.nvim_exec_autocmds("User", { pattern = "ScopeTabLeavePre" })
    end,

    post_tab_enter = function()
      vim.api.nvim_exec_autocmds("User", { pattern = "ScopeTabEnterPost" })
    end,
  },
}

function M.setup_scope()
  require("scope").setup(scope_config)
end

function M.suspend_scope()
  pcall(vim.api.nvim_clear_autocmds, { group = "ScopeAU" })
end

function M.resume_scope()
  M.setup_scope()
  pcall(vim.cmd.ScopeLoadState)
  pcall(require("barbar.state").get_updated_buffers, true)
  pcall(require("barbar.ui.render").update, true)
end

M.setup_scope()

vim.opt.sessionoptions:append "globals"

require("mini.sessions").setup({
  hooks = {
    pre = {
      write = function()
        vim.cmd "ScopeSaveState"
        vim.api.nvim_exec_autocmds("User", { pattern = "SessionSavePre" })
      end,
    },
  },
})

vim.api.nvim_create_autocmd("SessionLoadPre", {
  group = vim.api.nvim_create_augroup("ConfigWorkspaceSession", { clear = true }),
  desc = "Suspend scope.nvim while a Vim session rebuilds tabs",
  callback = M.suspend_scope,
})

vim.api.nvim_create_autocmd("SessionLoadPost", {
  group = "ConfigWorkspaceSession",
  desc = "Resume scope.nvim after a Vim session rebuilds tabs",
  callback = M.resume_scope,
})

local keys = {
  d = { "BufferClose", "Close" },
  D = { "BufferPickDelete", "Close(pick)" },
  c = { "BufferCloseAllButCurrent", "Close except current" },
  h = { "BufferCloseBuffersLeft", "Close left" },
  l = { "BufferCloseBuffersRight", "Close right" },
  n = { "BufferNext", "Next" },
  p = { "BufferPrevious", "Previous" },
  N = { "BufferMoveNext", "Next(move)" },
  P = { "BufferMovePrevious", "Previous(move)" },
  sd = { "BufferOrderByDirectory", "Sort: directory" },
  sn = { "BufferOrderByName", "Sort: name" },
  sl = { "BufferOrderByLanguage", "Sort: language" },
}

for key, value in pairs(keys) do
  vim.keymap.set("n", "<leader>b" .. key, "<cmd>" .. value[1] .. "<cr>", { desc = value[2] })
end

vim.keymap.set("n", "]b", "<cmd>BufferNext<cr>", { desc = "BufferNext" })
vim.keymap.set("n", "[b", "<cmd>BufferPrevious<cr>", { desc = "BufferPrevious" })

for i = 1, 9 do
  vim.keymap.set("n", "<leader>b" .. i, "<cmd>BufferGoto " .. i .. "<cr>", { desc = "Goto: " .. i })
end

vim.keymap.set("n", "<leader>ql", function()
  MiniSessions.read(MiniSessions.get_latest(), { force = true })
end, { desc = "Load last" })
vim.keymap.set("n", "<leader>qL", function()
  MiniSessions.select()
end, { desc = "List" })
vim.keymap.set("n", "<leader>qs", function()
  local ok, _ = pcall(MiniSessions.write)
  if not ok then
    vim.ui.input({ prompt = "Session Name = ", scope = "buffer" }, function(input)
      if vim.trim(input or "") == "" then
        return
      end
      MiniSessions.write(input)
    end)
  end
end, { desc = "Save" })
vim.keymap.set("n", "<leader>qd", function()
  MiniSessions.delete()
end, { desc = "Delete" })
vim.keymap.set("n", "<leader>qr", function()
  MiniSessions.restart()
end, { desc = "Restart" })

return M
