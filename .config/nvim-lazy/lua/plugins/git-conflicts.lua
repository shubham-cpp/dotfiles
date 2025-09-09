---@type LazySpec
return {
  {
    "akinsho/git-conflict.nvim",
    event = "BufReadPre",
    config = function()
      local gitconflict = require("git-conflict")
      ---@diagnostic disable-next-line: missing-fields
      gitconflict.setup({ default_mappings = false })

      vim.keymap.set({ "n", "v" }, "<leader>co", function()
        gitconflict.choose("ours")
      end, { desc = "Choose ours" })
      vim.keymap.set({ "n", "v" }, "<leader>ct", function()
        gitconflict.choose("theirs")
      end, { desc = "Choose theirs" })
      vim.keymap.set({ "n", "v" }, "<leader>cb", function()
        gitconflict.choose("both")
      end, { desc = "Choose both" })
      vim.keymap.set({ "n", "v" }, "<leader>cn", function()
        gitconflict.choose("none")
      end, { desc = "Choose none" })
      vim.keymap.set("n", "]x", function()
        gitconflict.find_next("ours")
      end, { desc = "Next conflict" })
      vim.keymap.set("n", "[x", function()
        gitconflict.find_prev("ours")
      end, { desc = "Previous conflict" })
    end,
  },
}
