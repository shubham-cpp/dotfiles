---@type LazySpec
return {
  "windwp/nvim-autopairs",
  event = "InsertEnter",
  opts = {
    enable_check_bracket_line = true,
    disable_in_visualblock = true,
    check_ts = true,
    fast_wrap = {},
  },
  config = function(_, opts)
    require("nvim-autopairs").setup(opts)
    -- If you want insert `(` after select function or method item
    local ok, cmp = pcall(require, "cmp")
    if ok then
      local cmp_autopairs = require "nvim-autopairs.completion.cmp"
      cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())
    end
  end,
}
