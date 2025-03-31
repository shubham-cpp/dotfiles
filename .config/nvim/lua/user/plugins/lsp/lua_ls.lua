---@type LazySpec
return {
  {
    "mason.nvim",
    optional = true,
    opts = { ensure_installed = { "lua-language-server", "stylua" } },
  },
  {
    "astrolsp",
    optional = true,
    ---@type AstroLSPOpts
    opts = {
      -- Configure language servers for `lspconfig` (`:h lspconfig-setup`)
      config = {
        lua_ls = {
          settings = {
            Lua = {
              format = {
                enable = true,
              },
              diagnostics = {
                enable = true,
                globals = { "vim", "describe", "Snacks" },
                disable = { "lowercase-global" },
              },
              workspace = {
                checkThirdParty = false,
                library = { vim.env.VIMRUNTIME },
              },
              telemetry = { enable = false },
              codeLens = { enable = true },
              completion = { callSnippet = "Replace" },
              doc = { privateName = { "^_" } },
              hint = {
                enable = true,
                setType = false,
                paramType = true,
                paramName = "Disable",
                semicolon = "Disable",
                arrayIndex = "Disable",
              },
            },
          },
        },
      },
    },
  },
  {
    "nvim-treesitter",
    optional = true,
    opts = { ensure_installed = { "lua", "luadoc" } },
  },
}
