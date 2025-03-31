---@type LazySpec
return {
  "kevinhwang91/nvim-ufo",
  dependencies = "kevinhwang91/promise-async",
  event = "BufRead",
  keys = function()
    local ufo = require "ufo"
    return {
      { "zR", ufo.openAllFolds, desc = "Open Folds" },
      { "zM", ufo.closeAllFolds, desc = "Close Folds" },
    }
  end,
  opts = {},
  init = function()
    vim.opt.foldcolumn = "1" -- '0' is not bad
    vim.opt.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
    vim.opt.foldlevelstart = 99
    vim.opt.foldenable = true
    local filetype_exclude = { "help", "alpha", "dashboard", "neo-tree", "Trouble", "lazy", "mason" }
    vim.api.nvim_create_autocmd("FileType", {
      group = vim.api.nvim_create_augroup("local_detach_ufo", { clear = true }),
      desc = "Disable ufo for these filetypes",
      pattern = filetype_exclude,
      callback = function() require("ufo").detach() end,
    })
  end,
}
