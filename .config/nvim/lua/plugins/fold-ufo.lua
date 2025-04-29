---@type LazySpec
return {
  "kevinhwang91/nvim-ufo",
  dependencies = "kevinhwang91/promise-async",
  enabled = false,
  opts = {
    provider_selector = function() return { "lsp", "indent" } end,
  },
  keys = {
    { "zr", function() require("ufo").openFoldsExceptKinds() end },
    { "zR", function() require("ufo").openAllFolds() end },
    { "zm", function() require("ufo").closeFoldsWith() end },
    { "zM", function() require("ufo").closeAllFolds() end },
  },
}
