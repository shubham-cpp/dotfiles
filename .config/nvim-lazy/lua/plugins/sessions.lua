---@type LazySpec
return {
  "rmagatti/auto-session",
  lazy = false,

  ---enables autocomplete for opts
  ---@module "auto-session"
  ---@type AutoSession.Config
  opts = {
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
