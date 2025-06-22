---@type LazySpec
return {
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = {
      ensure_installed = { "typescript", "javascript", "tsx", "jsdoc", }
    },
  },
  {
    "mason-org/mason-lspconfig.nvim",
    opts = { ensure_installed = { "vtsls" }, },
  },
  { 'yioneko/nvim-vtsls',cmd='VtsExec' },
  {
    'neovim/nvim-lspconfig',
    opts = function(_, opts)
      require('vtsls').config({
        refactor_auto_rename = true,
      })

      local fts = vim.deepcopy(require('vtsls').lspconfig.default_config.filetypes)
      require("lspconfig.configs").vtsls = require("vtsls").lspconfig
      table.insert(fts, "vue")

      local vue_location = require('mason-registry').get_package('vue-language-server'):get_install_path() .. "/node_modules/@vue/language-server"

      opts.servers.vtsls = {
        filetypes = fts,
        settings = {
            complete_function_calls = true,
            vtsls = {
tsserver={globalPlugins = {
        {
          name = "@vue/typescript-plugin",
          location = vue_location,
          languages = { "vue" },
          configNamespace = "typescript",
          enableForWorkspaceTypeScriptVersions = true,
        },
            },},
              enableMoveToFileCodeAction = true,
              autoUseWorkspaceTsdk = true,
            },
          typescript = {
              updateImportsOnFileMove = { enabled = "always" },
            inlayHints = {
              parameterNames = { enabled = "literals" },
              parameterTypes = { enabled = true },
              variableTypes = { enabled = true },
              propertyDeclarationTypes = { enabled = true },
              functionLikeReturnTypes = { enabled = true },
              enumMemberValues = { enabled = true },
            }
          },
          javascript = {
              updateImportsOnFileMove = { enabled = "always" },
            inlayHints = {
              parameterNames = { enabled = "literals" },
              parameterTypes = { enabled = true },
              variableTypes = { enabled = true },
              propertyDeclarationTypes = { enabled = true },
              functionLikeReturnTypes = { enabled = true },
              enumMemberValues = { enabled = true },
            }
          },
        },
        on_attach = function (_,buffer,map)
          map("gD",'<cmd>VtsExec goto_source_definition<cr>','Goto Source Def')
          map("gR",'<cmd>VtsExec file_references<cr>','File References')
          map("<leader>lv",'<cmd>VtsExec select_ts_version<cr>','Select TS Version')
          map("<leader>lR",'<cmd>VtsExec rename_file<cr>','Rename File')
          map("grR",'<cmd>VtsExec rename_file<cr>','Rename File')
          map("gro",'<cmd>VtsExec organize_imports<cr>','Organize Imports')
          map("<leader>lo",'<cmd>VtsExec organize_imports<cr>','Organize Imports')
          map("<leader>lF",'<cmd>VtsExec fix_all<cr>','Fix All')
        end
      }
    end
  }
}
