---@type LazySpec
return {
  {
    "cohama/lexima.vim",
    branch = "master",
    enabled = true, -- Doesn't work with jsx expand https://github.com/LazyVim/LazyVim/discussions/2020
    config = function()
      vim.cmd([[call lexima#add_rule({'at': '\%#\w', 'char': '(', 'input': '('})]])
    end,
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    enabled = false,
    opts = {
      fast_wrap = {
        map = "<M-e>",
        chars = { "{", "[", "(", '"', "'" },
        pattern = ([[ [%'%"%)%>%]%)%}%,] ]]):gsub("%s+", ""),
        offset = 0,
        end_key = "$",
        keys = "qwertyuiopzxcvbnmasdfghjkl",
        check_comma = true,
        highlight = "PmenuSel",
        highlight_grey = "LineNr",
      },
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
