---@type LazySpec
return {
  {
    "cohama/lexima.vim",
    branch = "master",
    enabled = false, -- Doesn't work with jsx expand https://github.com/LazyVim/LazyVim/discussions/2020
    config = function()
      vim.cmd([[call lexima#add_rule({'at': '\%#\w', 'char': '(', 'input': '('})]])
    end,
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    enabled = true,
    opts = {
      enable_check_bracket_line = true,
      disable_in_visualblock = true,
      check_ts = true,
      fast_wrap = {},
    },
    config = function(_, opts)
      require("nvim-autopairs").setup(opts)

      local ok_cmp, cmp = pcall(require, "cmp")

      if not ok_cmp then
        return
      end
      local cmp_autopairs = require("nvim-autopairs.completion.cmp")

      cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())
    end,
  },
}
