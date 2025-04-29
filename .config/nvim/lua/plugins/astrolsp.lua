---@type LazySpec
return {
  {
    "AstroNvim/astrolsp",
    ---@type AstroLSPOpts
    opts = {
      capabilities = {
        textDocument = {
          foldingRange = {
            dynamicRegistration = false,
            lineFoldingOnly = true,
          },
        },
      },
      -- customize language server configuration options passed to `lspconfig`
      ---@diagnostic disable: missing-fields
      config = {
        clangd = { capabilities = { offsetEncoding = "utf-8" } },
        vtsls = {
          settings = {
            complete_function_calls = true,
            vtsls = {
              enableMoveToFileCodeAction = true,
              autoUseWorkspaceTsdk = true,
              experimental = {
                maxInlayHintLength = 30,
                --   completion = {
                --     enableServerSideFuzzyMatch = true,
                --   },
              },
            },
            typescript = {
              updateImportsOnFileMove = { enabled = "always" },
              suggest = { completeFunctionCalls = true },
              inlayHints = {
                enumMemberValues = { enabled = true },
                functionLikeReturnTypes = { enabled = true },
                parameterNames = { enabled = "literals" },
                parameterTypes = { enabled = true },
                propertyDeclarationTypes = { enabled = true },
                variableTypes = { enabled = false },
              },
            },
            javascript = {
              updateImportsOnFileMove = { enabled = "always" },
              suggest = { completeFunctionCalls = true },
              inlayHints = {
                enumMemberValues = { enabled = true },
                functionLikeReturnTypes = { enabled = true },
                parameterNames = { enabled = "literals" },
                parameterTypes = { enabled = true },
                propertyDeclarationTypes = { enabled = true },
                variableTypes = { enabled = false },
              },
            },
          },
        },
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
      -- mappings to be set up on attaching of a language server
      mappings = {
        n = {
          ["<Leader>le"] = {
            function() vim.cmd "EslintFixAll" end,
            desc = "Eslint Fix",
            cond = function(client) return client.name == "eslint" end,
          },
          ["<Leader>lo"] = {
            function() vim.cmd "VtsExec organize_imports" end,
            desc = "Organize Imports",
            cond = function(client) return client.name == "vtsls" end,
          },
          ["<Leader>lv"] = {
            function() vim.cmd "VtsExec select_ts_version" end,
            desc = "Change TS version",
            cond = function(client) return client.name == "vtsls" end,
          },
          gro = {
            function()
              local ok, picker = pcall(require, "snacks.picker")
              if ok then
                picker.lsp_symbols()
              else
                vim.lsp.buf.document_symbol()
              end
            end,
            desc = "Document symbols",
            cond = "textDocument/documentSymbol",
          },
          grO = {
            function()
              local ok, picker = pcall(require, "snacks.picker")
              if ok then
                picker.lsp_workspace_symbols()
              else
                vim.lsp.buf.workspace_symbol()
              end
            end,
            desc = "Workspace symbols",
            cond = "workspace/symbol",
          },
        },
      },
    },
  },
  {
    "AstroNvim/astrolsp",
    ---@param opts AstroLSPOpts
    opts = function(_, opts)
      if require("astrocore").is_available "snacks.nvim" then
        opts.mappings.n.grr = {
          function() require("snacks.picker").lsp_references() end,
          desc = "LSP References",
          cond = "textDocument/references",
        }
        opts.mappings.n.gri = {
          function() require("snacks.picker").lsp_implementations() end,
          desc = "LSP Implementations",
          cond = "textDocument/implementation",
        }
        opts.mappings.n.gO = {
          function() require("snacks.picker").lsp_symbols() end,
          desc = "LSP Document Symbols",
        }

        if opts.mappings.n.gd then opts.mappings.n.gd[1] = function() require("snacks.picker").lsp_definitions() end end
        if opts.mappings.n.gI then
          opts.mappings.n.gI[1] = function() require("snacks.picker").lsp_implementations() end
        end
        if opts.mappings.n.gy then
          opts.mappings.n.gy[1] = function() require("snacks.picker").lsp_type_definitions() end
        end
        if opts.mappings.n["<Leader>lG"] then
          opts.mappings.n["<Leader>lG"][1] = function() require("snacks.picker").lsp_workspace_symbols() end
        end
        if opts.mappings.n["<Leader>lR"] then
          opts.mappings.n["<Leader>lR"][1] = function() require("snacks.picker").lsp_references() end
        end
      elseif require("astrocore").is_available "telescope.nvim" then
        if opts.mappings.n.gd then
          opts.mappings.n.gd[1] = function() require("telescope.builtin").lsp_definitions { reuse_win = true } end
        end
        if opts.mappings.n.gI then
          opts.mappings.n.gI[1] = function() require("telescope.builtin").lsp_implementations { reuse_win = true } end
        end
        if opts.mappings.n.gy then
          opts.mappings.n.gy[1] = function() require("telescope.builtin").lsp_type_definitions { reuse_win = true } end
        end
        if opts.mappings.n["<Leader>lG"] then
          opts.mappings.n["<Leader>lG"][1] = function()
            vim.ui.input({ prompt = "Symbol Query: (leave empty for word under cursor)" }, function(query)
              if query then
                -- word under cursor if given query is empty
                if query == "" then query = vim.fn.expand "<cword>" end
                require("telescope.builtin").lsp_workspace_symbols {
                  query = query,
                  prompt_title = ("Find word (%s)"):format(query),
                }
              end
            end)
          end
        end
        if opts.mappings.n["<Leader>lR"] then
          opts.mappings.n["<Leader>lR"][1] = function() require("telescope.builtin").lsp_references() end
        end
      end
    end,
  },
}
