---@type LazySpec
return {
  {
    "AstroNvim/astrolsp",
    optional = true,
    ---@type AstroLSPOpts
    opts = {
      config = {
        tailwindcss = {
          settings = {
            tailwindCSS = {
              classAttributes = { "class", "className", "classList", "ngClass" },
              lint = {
                cssConflict = "warning",
                invalidApply = "error",
                invalidConfigPath = "error",
                invalidScreen = "error",
                invalidTailwindDirective = "error",
                invalidVariant = "error",
                recommendedVariantOrder = "warning",
              },
              validate = true,
              experimental = {
                classRegex = {
                  "tw`([^`]*)",
                  'tw="([^"]*)',
                  'tw={"([^"}]*)',
                  "tw\\.\\w+`([^`]*)",
                  "tw\\(.*?\\)`([^`]*)",
                  { "clsx\\(([^)]*)\\)", "(?:'|\"|`)([^']*)(?:'|\"|`)" },
                  { "classnames\\(([^)]*)\\)", "'([^']*)'" },
                  { "cva\\(([^)]*)\\)", "[\"'`]([^\"'`]*).*?[\"'`]" },
                  { "cn\\(([^)]*)\\)", "(?:'|\"|`)([^']*)(?:'|\"|`)" },
                },
              },
            },
          },
          root_dir = function(fname)
            local root_pattern = require("lspconfig").util.root_pattern
            -- First, check for common Tailwind config files
            local root = root_pattern(
              "tailwind.config.mjs",
              "tailwind.config.cjs",
              "tailwind.config.js",
              "tailwind.config.ts",
              "postcss.config.js",
              "config/tailwind.config.js", -- for blade laravel
              "assets/tailwind.config.js", -- for elixir phoenix
              "assets/tailwind.config.mjs", -- for elixir phoenix
              "assets/tailwind.config.ts" -- for elixir phoenix
            )(fname)
            return root
          end,
          init_options = {
            userLanguages = {
              eelixir = "html-eex",
              eruby = "erb",
              blade = "html",
            },
          },
          single_file_support = false,
        },
      },
    },
  },
  {
    "mason.nvim",
    optional = true,
    opts = { ensure_installed = { "tailwindcss-language-server" } },
  },
}
