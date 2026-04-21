---@type LazySpec
return {
  {
    "dlyongemallo/diffview.nvim",
    cmd = { "DiffviewOpen", "DiffviewFileHistory", "DiffviewClose" },
    opts = {
      view = {
        default = { layout = "diff2_horizontal" },
        merge_tool = { layout = "diff3_horizontal" },
        file_history = { layout = "diff2_horizontal" },
      },
      enhanced_diff_hl = true,
    },
    config = function(_, opts)
      local actions = require "diffview.actions"
      opts.keymaps = {
        file_panel = { { "n", "q", actions.close, { desc = "DiffviewClose" } } },
        file_history_panel = { { "n", "q", actions.close, { desc = "DiffviewClose" } } },
      }
      require("diffview").setup(opts)
    end,
    dependencies = {
      {
        "AstroNvim/astrocore",
        ---@type AstroCoreOpts
        opts = {
          mappings = {
            n = {
              ["<Leader>gd"] = { "<cmd>DiffviewOpen<cr>", desc = "Diff open" },
              ["<Leader>gH"] = { "<cmd>DiffviewFileHistory %<cr>", desc = "File history" },
              ["<Leader>gF"] = { "<cmd>DiffviewFileHistory<cr>", desc = "Repo history" },
            },
          },
        },
      },
    },
  },
}
