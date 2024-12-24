---@type LazySpec
return {
  {
    "AstroNvim/astrolsp",
    ---@type AstroLSPOpts
    opts = {
      -- Configuration table of features provided by AstroLSP
      features = {
        codelens = true, -- enable/disable codelens refresh on start
        inlay_hints = true, -- enable/disable inlay hints on start
        semantic_tokens = true, -- enable/disable semantic token highlighting
      },
      -- customize lsp formatting options
      formatting = {
        -- control auto formatting on save
        format_on_save = {
          enabled = true, -- enable or disable format on save globally
          allow_filetypes = { -- enable format on save for specified filetypes only
            -- "go",
          },
          ignore_filetypes = { -- disable format on save for specified filetypes
            -- "python",
          },
        },
        disabled = { -- disable formatting capabilities for the listed language servers
          -- disable lua_ls formatting capability if you want to use StyLua to format your lua code
          -- "lua_ls",
        },
        timeout_ms = 1000, -- default format timeout
        -- filter = function(client) -- fully override the default formatting function
        --   return true
        -- end
      },
      -- enable servers that you already have installed without mason
      servers = {
        -- "pyright"
      },
      -- customize language server configuration options passed to `lspconfig`
      ---@diagnostic disable: missing-fields
      config = {
        clangd = { capabilities = { offsetEncoding = "utf-8" } },
      },
      -- customize how language servers are attached
      handlers = {
        -- a function without a key is simply the default handler, functions take two parameters, the server name and the configured options table for that server
        -- function(server, opts) require("lspconfig")[server].setup(opts) end

        -- the key is the server that is being setup with `lspconfig`
        -- rust_analyzer = false, -- setting a handler to false will disable the set up of that language server
        -- pyright = function(_, opts) require("lspconfig").pyright.setup(opts) end -- or a custom handler function can be passed
        emmet_ls = function(_, opts)
          opts.on_attach = function(client, bufnr)
            vim.keymap.set("i", "<C-t>", function()
              client.request(
                "textDocument/completion",
                vim.lsp.util.make_position_params(0, client.offset_encoding),
                function(_, result)
                  local textEdit = result.items[1].textEdit
                  local snip_string = textEdit.newText
                  textEdit.newText = ""
                  vim.lsp.util.apply_text_edits({ textEdit }, bufnr, client.offset_encoding)
                  local ok, luasnip = pcall(require, "luasnip")
                  if ok then
                    luasnip.lsp_expand(snip_string)
                  else
                    vim.snippet.expand(snip_string)
                  end
                end,
                bufnr
              )
            end, { buffer = bufnr, desc = "Emmet Expand", noremap = true })
          end
          require("lspconfig").emmet_ls.setup(opts)
        end,
        emmet_language_server = function(_, opts)
          opts.on_attach = function(client, bufnr)
            vim.keymap.set("i", "<C-t>", function()
              client.request(
                "textDocument/completion",
                vim.lsp.util.make_position_params(0, client.offset_encoding),
                function(_, result)
                  local textEdit = result.items[1].textEdit
                  local snip_string = textEdit.newText
                  textEdit.newText = ""
                  vim.lsp.util.apply_text_edits({ textEdit }, bufnr, client.offset_encoding)
                  local ok, luasnip = pcall(require, "luasnip")
                  if ok then
                    luasnip.lsp_expand(snip_string)
                  else
                    vim.snippet.expand(snip_string)
                  end
                end,
                bufnr
              )
            end, { buffer = bufnr, desc = "Emmet Expand", noremap = true })
          end
          require("lspconfig").emmet_ls.setup(opts)
        end,
      },
      -- Configure buffer local auto commands to add when attaching a language server
      autocmds = {
        -- first key is the `augroup` to add the auto commands to (:h augroup)
        lsp_codelens_refresh = {
          -- Optional condition to create/delete auto command group
          -- can either be a string of a client capability or a function of `fun(client, bufnr): boolean`
          -- condition will be resolved for each client on each execution and if it ever fails for all clients,
          -- the auto commands will be deleted for that buffer
          cond = "textDocument/codeLens",
          -- cond = function(client, bufnr) return client.name == "lua_ls" end,
          -- list of auto commands to set
          {
            -- events to trigger
            event = { "InsertLeave", "BufEnter" },
            -- the rest of the autocmd options (:h nvim_create_autocmd)
            desc = "Refresh codelens (buffer)",
            callback = function(args)
              if require("astrolsp").config.features.codelens then vim.lsp.codelens.refresh { bufnr = args.buf } end
            end,
          },
        },
      },
      commands = {
        ImportsOrganize = {
          function()
            vim.lsp.buf.code_action {
              context = { only = { "source.organizeImports" } },
              apply = true,
            }
          end,
          cond = "textDocument/codeAction",
          desc = "Organize Imports",
        },
        ImportsRemove = {
          function()
            vim.lsp.buf.code_action {
              context = { only = { "source.removeUnused" } },
              apply = true,
            }
          end,
          cond = "textDocument/codeAction",
          desc = "Remove Unused Imports",
        },
        FixAll = {
          function()
            vim.lsp.buf.code_action {
              context = { only = { "source.fixAll" } },
              apply = true,
            }
          end,
          cond = "textDocument/codeAction",
          desc = "Fix All fixable diagnostics",
        },
      },
      -- mappings to be set up on attaching of a language server
      mappings = {
        n = {
          -- a `cond` key can provided as the string of a server capability to be required to attach, or a function with `client` and `bufnr` parameters from the `on_attach` that returns a boolean
          gD = {
            function() vim.lsp.buf.declaration() end,
            desc = "Declaration of current symbol",
            cond = "textDocument/declaration",
          },
          ["<Leader>uY"] = {
            function() require("astrolsp.toggles").buffer_semantic_tokens() end,
            desc = "Toggle LSP semantic highlight (buffer)",
            cond = function(client)
              return client.supports_method "textDocument/semanticTokens/full" and vim.lsp.semantic_tokens ~= nil
            end,
          },
          gd = {
            function()
              local ok, builtin = pcall(require, "telescope.builtin")
              if not ok then
                vim.lsp.buf.references()
              else
                builtin.lsp_definitions()
              end
              print(ok)
            end,
            desc = "Goto references",
            cond = "textDocument/definition",
          },
          ["<F2>"] = {
            vim.lsp.buf.rename,
            desc = "Rename",
            cond = "textDocument/rename",
          },
          grr = {
            function()
              local ok, builtin = pcall(require, "telescope.builtin")
              if not ok then
                vim.lsp.buf.references()
              else
                builtin.lsp_references()
              end
            end,
            desc = "Goto references",
            cond = "textDocument/references",
          },
          ["grw"] = {
            function()
              local ok, builtin = pcall(require, "telescope.builtin")
              if not ok then
                vim.lsp.buf.document_symbol()
              else
                builtin.lsp_document_symbols()
              end
            end,
            desc = "Document symbol",
            cond = "textDocument/documentSymbol",
          },
          ["grW"] = {
            function()
              local ok, builtin = pcall(require, "telescope.builtin")
              if not ok then
                vim.lsp.buf.workspace_symbol()
              else
                builtin.lsp_workspace_symbols()
              end
            end,
            desc = "Workspace symbols",
            cond = "workspace/symbol",
          },
          ["<leader>lw"] = {
            function()
              local ok, builtin = pcall(require, "telescope.builtin")
              if not ok then
                vim.lsp.buf.document_symbol()
              else
                builtin.lsp_document_symbols()
              end
            end,
            desc = "Document symbol",
            cond = "textDocument/documentSymbol",
          },
          ["<leader>lW"] = {
            function()
              local ok, builtin = pcall(require, "telescope.builtin")
              if not ok then
                vim.lsp.buf.workspace_symbol()
              else
                builtin.lsp_workspace_symbols()
              end
            end,
            desc = "Workspace symbols",
            cond = "workspace/symbol",
          },
          ["<leader>="] = {
            vim.lsp.buf.format,
            desc = "Format buffer",
          },
          ["<leader>lo"] = {
            function()
              vim.lsp.buf.code_action {
                context = { only = { "source.organizeImports" } },
                apply = true,
              }
            end,
            desc = "Oraganize imports",
            cond = "textDocument/codeAction",
          },
          ["<leader>lF"] = {
            function()
              vim.lsp.buf.code_action {
                context = { only = { "source.fixAll" } },
                apply = true,
              }
            end,
            desc = "Fix All",
            cond = "textDocument/codeAction",
          },
        },
        i = {
          ["<C-k>"] = {
            vim.lsp.buf.signature_help,
            desc = "Signature help",
            cond = "textDocument/signatureHelp",
          },
        },
      },
      -- A custom `on_attach` function to be run after the default `on_attach` function
      -- takes two parameters `client` and `bufnr`  (`:h lspconfig-setup`)
      on_attach = function(client, bufnr)
        -- this would disable semanticTokensProvider for all clients
        -- client.server_capabilities.semanticTokensProvider = nil
      end,
    },
  },
  {
    "williamboman/mason-lspconfig.nvim",
    optional = true,
    opts = function(_, opts)
      -- opts.ensure_installed = vim.tbl_filter(
      --   function(server_name) return server_name ~= "emmet_ls" end,
      --   opts.ensure_installed or {}
      -- )
      opts.ensure_installed =
        require("astrocore").list_insert_unique(opts.ensure_installed, { "css_variables", "cssmodules_ls" })
    end,
  },
}
