local gen_loader = require("mini.snippets").gen_loader

local lang_patterns = function(lang)
  return {
    lang .. "/**/*.json",
    lang .. "/**/*.lua",
    "**/" .. lang .. ".json",
    "**/" .. lang .. ".lua",
  }
end

local js_snippets = lang_patterns "javascript"
local jsx_snippets = vim.list_extend(lang_patterns "javascriptreact", {
  "javascript/javascript.json",
  "html.json",
  "javascript/react.json",
  "javascript/react-es7.json",
  "javascript/react-native.json",
  "javascript/next.json",
})
local ts_snippets = lang_patterns "typescript"
local tsx_snippets = vim.list_extend(lang_patterns "typescriptreact", {
  "javascript/typescript.json",
  "html.json",
  "javascript/react-ts.json",
  "javascript/react-es7.json",
  "javascript/react-native-ts.json",
  "javascript/next-ts.json",
})

require("mini.snippets").setup({
  snippets = {
    gen_loader.from_file(vim.fn.stdpath "config" .. "/snippets/global.json"),
    gen_loader.from_lang({
      lang_patterns = {
        javascript = js_snippets,
        javascriptreact = jsx_snippets,
        jsx = jsx_snippets,
        typescript = ts_snippets,
        typescriptreact = tsx_snippets,
        tsx = tsx_snippets,
      },
    }),
  },
})
