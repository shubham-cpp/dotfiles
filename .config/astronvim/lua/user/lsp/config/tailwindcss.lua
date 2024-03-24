return {
  -- cmd = { require("user.lsp.util").bun_path() .. "/tailwindcss-language-server", "--stdio" },
  root_dir = require("lspconfig").util.root_pattern(
    "tailwind.config.js",
    "tailwind.config.cjs",
    "tailwind.config.mjs",
    "tailwind.config.ts"
  ),
  single_file_support = false,
}
