---@type LazySpec
return {
  {
    "astrolsp",
    optional = true,
    ---@type AstroLSPOpts
    opts = {
      config = {
        cssls = {
          settings = {
            css = {
              validate = true,
              completion = {
                triggerPropertyValueCompletion = true,
                completePropertyWithSemicolon = true,
              },
              lint = {
                compatibleVendorPrefixes = "warn",
                duplicateProperties = "warn",
                boxModel = "warn",
                unknownVendorSpecificProperties = "warn",
                float = "error",
              },
            },
            scss = {
              validate = true,
              completion = {
                triggerPropertyValueCompletion = true,
                completePropertyWithSemicolon = true,
              },
              lint = {
                compatibleVendorPrefixes = "warn",
                duplicateProperties = "warn",
                boxModel = "warn",
                unknownVendorSpecificProperties = "warn",
                float = "error",
              },
            },
          },
        },
        cssmodules_ls = {},
        css_variables = {},
        html = {},
        emmet_language_server = {
          init_options = {
            syntaxProfiles = {
              jsx = {
                self_closing_tag = true,
              },
            },
            showExpandedAbbreviation = "inMarkupAndStylesheetFilesOnly",
            showSuggestionsAsSnippets = true,
          },
          filetypes = {
            "eruby",
            "heex",
            "html",
            "css",
            "less",
            "sass",
            "scss",
            "pug",
            "javascript",
            "javascriptreact",
            "typescriptreact",
          },
        },
      },
    },
  },
  {
    "mason.nvim",
    optional = true,
    opts = {
      ensure_installed = {
        "html-lsp",
        "css-lsp",
        "css-variables-language-server",
        "cssmodules-language-server",
        "emmet-language-server",
        "prettierd",
      },
    },
  },
  {
    "nvim-treesitter",
    optional = true,
    opts = { ensure_installed = { "html", "css", "scss" } },
  },
}
