---@type LazySpec
return {
  "rmagatti/auto-session",
  lazy = false,
  cmd = { "AutoSession", "SessionManager" },
  ---enables autocomplete for opts
  ---@module "auto-session"
  ---@type AutoSession.Config
  opts = {
    enabled = true,
    auto_save = true,
    suppressed_dirs = { "~/", "~/Projects", "~/Downloads", "/" },
    use_git_branch = true,
  },
  keys = {
    { "<leader>S", "", desc = "+[S]ession" },
    { "<leader>Ss", "<cmd>SessionSave<CR>", desc = "[S]ave" },
    { "<leader>Sl", "<cmd>AutoSession search<cr>", desc = "[L]ist" },
    { "<leader>Sd", "<cmd>AutoSession delete<cr>", desc = "[D]elte" },
  },
}
