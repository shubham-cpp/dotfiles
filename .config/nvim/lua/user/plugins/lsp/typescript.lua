---@type LazySpec
return {
  {
    "astrolsp",
    optional = true,
    ---@type AstroLSPOpts
    opts = {
      autocmds = {
        eslint_fix_on_save = {
          cond = function(client) return client.name == "eslint" and vim.fn.exists ":EslintFixAll" > 0 end,
          {
            event = "BufWritePost",
            desc = "Fix all eslint errors",
            callback = function(args)
              if vim.F.if_nil(vim.b[args.buf].autoformat, vim.g.autoformat, true) then vim.cmd.EslintFixAll() end
            end,
          },
        },
        nvim_vtsls = {
          {
            event = "LspAttach",
            desc = "Load nvim-vtsls with vtsls",
            callback = function(args)
              if assert(vim.lsp.get_client_by_id(args.data.client_id)).name == "vtsls" then
                require("vtsls")._on_attach(args.data.client_id, args.buf)
                vim.api.nvim_del_augroup_by_name "nvim_vtsls"
              end
            end,
          },
        },
      },
      config = {
        vtsls = {
          settings = {
            typescript = {
              updateImportsOnFileMove = { enabled = "always" },
              preferences = {
                preferTypeOnlyAutoImports = true,
              },
              inlayHints = {
                parameterNames = { enabled = "all" },
                parameterTypes = { enabled = true },
                variableTypes = { enabled = true },
                propertyDeclarationTypes = { enabled = true },
                functionLikeReturnTypes = { enabled = true },
                enumMemberValues = { enabled = true },
              },
            },
            javascript = {
              updateImportsOnFileMove = { enabled = "always" },
              preferences = {
                preferTypeOnlyAutoImports = true,
              },
              inlayHints = {
                parameterNames = { enabled = "literals" },
                parameterTypes = { enabled = true },
                variableTypes = { enabled = true },
                propertyDeclarationTypes = { enabled = true },
                functionLikeReturnTypes = { enabled = true },
                enumMemberValues = { enabled = true },
              },
            },
            vtsls = {
              enableMoveToFileCodeAction = true,
              autoUseWorkspaceTsdk = true,
              -- tsserver = {
              --   globalPlugins = {
              --     {
              --       name = "@vue/typescript-plugin",
              --       location = require("mason-registry").get_package("vue-language-server"):get_install_path()
              --         .. "/node_modules/@vue/language-server",
              --       languages = { "vue" },
              --       configNamespace = "typescript",
              --       enableForWorkspaceTypeScriptVersions = true,
              --     },
              --     {
              --       name = "typescript-svelte-plugin",
              --       location = require("mason-registry").get_package("svelte-language-server"):get_install_path()
              --         .. "/node_modules/typescript-svelte-plugin",
              --       enableForWorkspaceTypeScriptVersions = true,
              --     },
              --   },
              -- },
            },
          },
          filetypes = {
            "javascript",
            "javascriptreact",
            "javascript.jsx",
            "typescript",
            "typescriptreact",
            "typescript.tsx",
            "vue",
          },
        },
      },
    },
  },
  {
    "yioneko/nvim-vtsls",
    lazy = true,
    config = function(_, opts) require("vtsls").config(opts) end,
  },
  {
    "dmmulroy/ts-error-translator.nvim",
    ft = { "javascript", "typescript", "javascriptreact", "typescriptreact" },
    opts = {},
  },
  {
    "mason.nvim",
    optional = true,
    opts = { ensure_installed = { "vtsls", "eslint-lsp" } },
  },
  {
    "nvim-treesitter",
    optional = true,
    opts = { ensure_installed = { "typescript", "javascript", "tsx", "styled", "jsdoc" } },
  },
}
