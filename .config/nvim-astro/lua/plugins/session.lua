---@type LazySpec
return {
  {
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
      { "<leader>s", "", desc = "+[S]ession" },
      { "<leader>ss", "<cmd>SessionSave<CR>", desc = "[S]ave" },
      { "<leader>sl", "<cmd>AutoSession search<cr>", desc = "[L]ist" },
      { "<leader>sd", "<cmd>AutoSession delete<cr>", desc = "[D]elte" },
    },
  },
}
