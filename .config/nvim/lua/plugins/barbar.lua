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

vim.api.nvim_create_autocmd("SessionLoadPre", {
  group = vim.api.nvim_create_augroup("ConfigBarbarScopeSession", { clear = true }),
  desc = "Suspend scope.nvim while a Vim session rebuilds tabs",
  callback = M.suspend_scope,
})

vim.api.nvim_create_autocmd("SessionLoadPost", {
  group = "ConfigBarbarScopeSession",
  desc = "Resume scope.nvim after a Vim session rebuilds tabs",
  callback = M.resume_scope,
})

return M
