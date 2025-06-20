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

      local cmp_autopairs = require("nvim-autopairs.completion.cmp")
      local cmp = require("cmp")
      cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())
    end,
  },
}
