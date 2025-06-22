---@type LazySpec
return {
  "folke/todo-comments.nvim",
    -- stylua: ignore
    keys = {
      { "<leader>ft", function() require("todo-comments.fzf").todo() end, desc = "Todo" },
      { "<leader>fT", function () require("todo-comments.fzf").todo({ keywords = { "TODO", "FIX", "FIXME" } }) end, desc = "Todo/Fix/Fixme" },
    },
}

