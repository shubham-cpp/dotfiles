---@type LazySpec
return {
  "akinsho/toggleterm.nvim",
  cmd = { "ToggleTerm", "TermExec" },
  keys = { "<C-\\>", "<C-/>" },
  opts = {
    direction = "float",
    shell = "fish",
    open_mapping = [[<c-\>]],
    float_opts = { border = "rounded" },
    ---@param t Terminal
    on_create = function(t)
      vim.opt_local.foldcolumn = "0"
      vim.opt_local.signcolumn = "no"
    end,
    on_open = function(t)
      vim.keymap.set("t", "<C-]>", "<C-\\><C-n>", { buffer = bufnr, desc = "Goto normal mode" })
      vim.keymap.set("n", "A", "A<C-k>", { buffer = bufnr })
      vim.keymap.set("n", "D", "A<C-k><C-\\><C-n>", { buffer = bufnr })
      -- vim.keymap.set("n", "cc", "A<C-e><C-u>", { buffer = bufnr })
      -- vim.keymap.set("n", "dd", "A<C-e><C-u><C-\\><C-n>", { buffer = bufnr })

      vim.opt_local.foldexpr = ""
      vim.opt_local.foldmethod = "manual"
    end,
  },
  config = function(_, opts)
    local Terminal = require("toggleterm.terminal").Terminal
    local yazi = Terminal:new({
      cmd = "yazi",
      hidden = true,
      direction = "float",
      on_create = function(term)
        local bufnr = term.bufnr
        vim.keymap.set({ "t", "n" }, "<C-/>", function() term:toggle() end, { desc = "Toggle Yazi", buffer = bufnr })
      end,
    })
    require("toggleterm").setup(opts)

    vim.keymap.set("n", "<C-/>", function() yazi:toggle() end, { desc = "Toggle Yazi" })
  end,
}
