---@type LazySpec
return {
  "tanvirtin/vgit.nvim",
  version = "*",
  dependencies = { "nvim-lua/plenary.nvim", "nvim-tree/nvim-web-devicons" },
  cmd = "VGit",
  enabled = false,
  opts = {
    keymaps = {
      {
        mode = "n",
        key = "<leader>gk",
        handler = "hunk_up",
        desc = "Go up in the direction of the hunk",
      },
      {
        mode = "n",
        key = "<leader>gj",
        handler = "hunk_down",
        desc = "Go down in the direction of the hunk",
      },
      {
        mode = "n",
        key = "<leader>gs",
        handler = "buffer_hunk_stage",
        desc = "Hunk Stage",
      },
      {
        mode = "n",
        key = "<leader>gr",
        handler = "buffer_hunk_reset",
        desc = "Hunk Reset",
      },
      {
        mode = "n",
        key = "<leader>gR",
        handler = "buffer_reset",
        desc = "Buffer Reset",
      },
      {
        mode = "n",
        key = "<leader>gp",
        handler = "buffer_hunk_preview",
        desc = "Hunk Preview",
      },
      {
        mode = "n",
        key = "<leader>gl",
        handler = "buffer_blame_preview",
        desc = "Git Blame",
      },
      {
        mode = "n",
        key = "<leader>gd",
        handler = "buffer_diff_preview",
        desc = "Buffer Diff",
      },
      {
        mode = "n",
        key = "<leader>gD",
        handler = "project_diff_preview",
        desc = "Project Diff",
      },
    },
  },
}
