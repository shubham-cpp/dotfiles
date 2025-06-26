local function has(client, method)
  if not client then
    return false
  end

  if not method then
    return true
  end

  local name = method:find "/" and method or "textDocument/" .. method
  return client.supports_method(name)
end
---@type LazySpec
return {
  {
    "neovim/nvim-lspconfig",
    -- event = "VeryLazy",
    event = { "BufReadPost", "BufNewFile", "BufWritePre" },
    dependencies = { "saghen/blink.cmp", "hrsh7th/nvim-cmp", "mason-org/mason.nvim", "mason-org/mason-lspconfig.nvim" },
    opts = {
      servers = {
        lua_ls = {
          settings = {
            Lua = {
              workspace = {
                checkThirdParty = false,
              },
              codeLens = {
                enable = true,
              },
              completion = {
                callSnippet = "Replace",
              },
              doc = {
                privateName = { "^_" },
              },
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
    opts_extend = { "servers" },
    config = function(_, opts)
      local lspconfig = require "lspconfig"
      vim.diagnostic.config {
        underline = true,
        update_in_insert = false,
        severity_sort = true,
        virtual_text = {
          spacing = 4,
          source = "if_many",
          prefix = "●",
        },
        jump = { float = true },
        float = { style = "minimal", border = "rounded" },
        signs = {
          text = {
            [vim.diagnostic.severity.ERROR] = " ",
            [vim.diagnostic.severity.WARN] = " ",
            [vim.diagnostic.severity.HINT] = " ",
            [vim.diagnostic.severity.INFO] = " ",
          },
        },
      }

      for server, config in pairs(opts.servers) do
        -- config.capabilities = require("blink.cmp").get_lsp_capabilities(vim.tbl_deep_extend("force", {
        config.capabilities = require("cmp_nvim_lsp").default_capabilities(vim.tbl_deep_extend("force", {
          textDocument = {
            foldingRange = { dynamicRegistration = false, lineFoldingOnly = true },
          },
        }, config.capabilities or {}))
        lspconfig[server].setup(vim.tbl_extend("force", config, {
          on_attach = function(client, buffer)
            local ok, picker = pcall(require, "snacks.picker")
            ---LSP mappings function
            ---@param lhs string
            ---@param rhs string | function
            ---@param desc? string
            ---@param method? string
            ---@param mode? string | string[]
            local function map(lhs, rhs, desc, method, mode)
              if has(client, method) then
                vim.keymap.set(mode or "n", lhs, rhs, {
                  buffer = buffer,
                  desc = desc,
                })
              end
            end
            map("gd", vim.lsp.buf.definition, "Goto Definition", "definition")
            map("grt", vim.lsp.buf.type_definition, "Goto type_definition", "typeDefinition")
            map("gD", vim.lsp.buf.declaration, "Goto Declaration", "declaration")
            map("<leader>ld", vim.lsp.buf.definition, "Goto Definition", "definition")
            map("<leader>lD", vim.lsp.buf.declaration, "Goto Declaration", "declaration")
            map("<leader>la", vim.lsp.buf.code_action, "Code Action", "codeAction", { "n", "v" })
            map("<leader>lA", function()
              vim.lsp.buf.code_action { context = { only = { "source" }, diagnostics = {} } }
            end, "Source Action", "codeAction", { "n", "v" })
            map("<leader>lc", vim.lsp.codelens.run, "Run Codelens", "codeLens", { "n", "v" })
            map("<leader>lC", vim.lsp.codelens.refresh, "Refresh & Display Codelens", "codeLens")
            map("<leader>lr", vim.lsp.buf.rename, "Rename", "rename")
            map("<leader>li", vim.lsp.buf.implementation, "Implementation", "implementation")
            map("<leader>lw", vim.lsp.buf.document_symbol, "Symbols", "documentSymbol")
            map("<leader>lW", vim.lsp.buf.workspace_symbol, "Symbols(Workspace)", "workspace/symbol")
            map("gro", vim.lsp.buf.document_symbol, "Symbols", "documentSymbol")
            map("grO", vim.lsp.buf.workspace_symbol, "Symbols(Workspace)", "workspace/symbol")

            if ok then
              map("gd", picker.lsp_definitions, "Goto Definition", "definition")
              map("grt", picker.lsp_type_definitions, "Goto type_definition", "typeDefinition")
              map("gD", picker.lsp_declarations, "Goto Declaration", "declaration")
              map("<leader>ld", picker.lsp_definitions, "Goto Definition", "definition")
              map("<leader>lD", picker.lsp_declarations, "Goto Declaration", "declaration")
              map("<leader>li", picker.lsp_implementations, "Implementation", "implementation")
              map("gri", picker.lsp_implementations, "Implementation", "implementation")
              map("grr", function()
                picker.lsp_references { ignore_current_line = true, includeDeclaration = false }
              end, "References", "definition")

              -- map("<leader>la", picker.lsp_code_actions, "Code Action", "codeAction", { "n", "v" })
              -- map("gra", picker.lsp_code_actions, "Code Action", "codeAction", { "n", "v" })

              map("gro", picker.lsp_symbols, "Symbols", "documentSymbol")
              map("grO", picker.lsp_workspace_symbols, "Symbols(Workspace)", "workspace/symbol")
              map("<leader>lw", picker.lsp_symbols, "Symbols", "documentSymbol")
              map("<leader>lW", picker.lsp_workspace_symbols, "Symbols(Workspace)", "workspace/symbol")
            end

            if config["on_attach"] ~= nil then
              config.on_attach(client, buffer, map)
            end
          end,
        }))
      end
    end,
  },
  {
    "folke/lazydev.nvim",
    ft = "lua",
    opts = {
      library = {
        "lazy.nvim",
        -- It can also be a table with trigger words / mods
        -- Only load luvit types when the `vim.uv` word is found
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
  },
}
