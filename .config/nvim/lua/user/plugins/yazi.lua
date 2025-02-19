---@type LazySpec
return {
  "mikavilpas/yazi.nvim",
  dependencies = { "snacks.nvim" },
  cmd = "Yazi",
  keys = {
    {
      "<leader>e",
      mode = { "n", "v" },
      "<cmd>Yazi<cr>",
      desc = "Open yazi at the current file",
    },
    {
      "<leader>E",
      "<cmd>Yazi cwd<cr>",
      desc = "Open the file manager in nvim's working directory",
    },
{
      "<c-/>",
      "<cmd>Yazi toggle<cr>",
      desc = "Resume the last yazi session",
    },
  },
  opts = {}
}
