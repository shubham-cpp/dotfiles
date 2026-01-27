---@type LazySpec
return {
  "folke/todo-comments.nvim",
  opts = {},
  event = "User AstroFile",
  cmd = { "TodoTrouble", "TodoFzfLua", "TodoLocList", "TodoQuickFix" },
  specs = {
    {
      "AstroNvim/astrocore",
      opts = function(_, opts)
        local maps = opts.mappings
        maps.n["<Leader>ft"] = { "<cmd>TodoFzfLua<cr>", desc = "Find Todos" }
      end,
    },
  },
}
