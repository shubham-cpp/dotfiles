---@type LazySpec
return {
  {
    "folke/persistence.nvim",
    keys = function()
      return {
        {
          "<leader>ql",
          function()
            require("persistence").load()
          end,
          desc = "Load Session",
        },
        {
          "<leader>qs",
          function()
            require("persistence").select()
          end,
          desc = "Select Session",
        },
        {
          "<leader>qL",
          function()
            require("persistence").load({ last = true })
          end,
          desc = "Restore Last Session",
        },
        {
          "<leader>qd",
          function()
            require("persistence").stop()
          end,
          desc = "Don't Save Current Session",
        },
      }
    end,
  },
  {
    "rmagatti/auto-session",
    enabled = false,
    lazy = false,
    cmd = { "AutoSession", "SessionManager" },
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
  },
}
