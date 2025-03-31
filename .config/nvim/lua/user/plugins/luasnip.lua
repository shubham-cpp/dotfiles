---@type LazySpec
return {
  "L3MON4D3/LuaSnip",
  version = "v2.*",
  enabled = true,
  build = "make install_jsregexp",
  dependencies = {
    {
      "rafamadriz/friendly-snippets",
      config = function()
        require("luasnip.loaders.from_vscode").lazy_load()
        require("luasnip.loaders.from_vscode").lazy_load({ paths = { vim.fn.stdpath "config" .. "/snippets" } })

        local extends = {
          typescript = { "tsdoc" },
          javascript = { "jsdoc" },
          lua = { "luadoc" },
          python = { "pydoc" },
          rust = { "rustdoc" },
          cs = { "csharpdoc" },
          java = { "javadoc" },
          c = { "cdoc" },
          cpp = { "cppdoc" },
          php = { "phpdoc" },
          kotlin = { "kdoc" },
          ruby = { "rdoc" },
          sh = { "shelldoc" },
        }
        -- friendly-snippets - enable standardized comments snippets
        for ft, snips in pairs(extends) do
          require("luasnip").filetype_extend(ft, snips)
        end
      end,
    },
  },
  opts = {
    history = true,
    delete_check_events = "TextChanged",
  },
}
