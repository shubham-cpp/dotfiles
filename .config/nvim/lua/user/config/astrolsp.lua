---@type AstroLSPConfig
local M = {
  -- Configuration table of features provided by AstroLSP
  features = {
    codelens = true, -- enable/disable codelens refresh on start
    inlay_hints = false, -- enable/disable inlay hints on start
    semantic_tokens = true, -- enable/disable semantic token highlighting
  },
  autocmds = {
    lsp_document_highlight = {
      cond = "textDocument/documentHighlight",
      {
        event = { "CursorHold", "CursorHoldI" },
        desc = "Document Highlighting",
        callback = function() vim.lsp.buf.document_highlight() end,
      },
      {
        event = { "CursorMoved", "CursorMovedI", "BufLeave" },
        desc = "Document Highlighting Clear",
        callback = function() vim.lsp.buf.clear_references() end,
      },
    },
  },
  commands = {
    ImportsOrganize = {
      function() vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } }, apply = true }) end,
      cond = "textDocument/codeAction",
      desc = "Organize Imports",
    },
    ImportsRemove = {
      function() vim.lsp.buf.code_action({ context = { only = { "source.removeUnused" } }, apply = true }) end,
      cond = "textDocument/codeAction",
      desc = "Remove Unused Imports",
    },
    FixAll = {
      function() vim.lsp.buf.code_action({ context = { only = { "source.fixAll" } }, apply = true }) end,
      cond = "textDocument/codeAction",
      desc = "Fix All fixable diagnostics",
    },
  },
  capabilities = {
    textDocument = { foldingRange = { dynamicRegistration = false } },
    workspace = { fileOperations = { didRename = true, willRename = true } },
  },
  mappings = {
    n = {
      gl = {
        vim.diagnostic.open_float,
        desc = "Hover diagnostics",
      },
      ["<leader>le"] = { vim.diagnostic.open_float, desc = "Hover diagnostics" },
      ["<leader>ld"] = {
        function()
          local ok_fzf, fzf = pcall(require, "fzf-lua")
          local ok_telescope, builtin = pcall(require, "telescope.builtin")
          local ok_snacks, picker = pcall(require, "snacks.picker")
          local ok_mini, mini_pick = pcall(require, "mini.pick")

          if ok_snacks then
            picker.diagnostics()
          elseif ok_fzf then
            fzf.diagnostics()
          elseif ok_telescope then
            builtin.diagnostics()
          elseif ok_mini then
            mini_pick.pickers.diagnostic()
          end
        end,
        desc = "Diagnostics",
      },
      gd = {
        function()
          local ok_fzf, fzf = pcall(require, "fzf-lua")
          local ok_telescope, builtin = pcall(require, "telescope.builtin")
          local ok_snacks, picker = pcall(require, "snacks.picker")
          local ok_mini, mini_pick = pcall(require, "mini.pick")

          if ok_snacks then
            picker.lsp_definitions()
          elseif ok_fzf then
            fzf.lsp_definitions({
              jump_to_single_result = true,
              winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
            })
          elseif ok_telescope then
            builtin.lsp_definitions()
          elseif ok_mini then
            mini_pick.pickers.lsp({ scope = "definition" })
          else
            vim.lsp.buf.definiton()
          end
        end,
        desc = "Goto definition",
        cond = "textDocument/definition",
      },
      ["<leader>lD"] = {
        function()
          local ok_fzf, fzf = pcall(require, "fzf-lua")
          local ok_telescope, builtin = pcall(require, "telescope.builtin")
          local ok_snacks, picker = pcall(require, "snacks.picker")
          local ok_mini, mini_pick = pcall(require, "mini.pick")

          if ok_snacks then
            picker.lsp_definitions()
          elseif ok_fzf then
            fzf.lsp_definitions({
              jump_to_single_result = true,
              winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
            })
          elseif ok_telescope then
            builtin.lsp_definitions()
          elseif ok_mini then
            mini_pick.pickers.lsp({ scope = "definition" })
          else
            vim.lsp.buf.definiton()
          end
        end,
        desc = "Goto definition",
        cond = "textDocument/definition",
      },
      gD = {
        function()
          local ok_fzf, fzf = pcall(require, "fzf-lua")
          local ok_telescope, builtin = pcall(require, "telescope.builtin")
          local ok_snacks, picker = pcall(require, "snacks.picker")
          local ok_mini, mini_pick = pcall(require, "mini.pick")

          if ok_snacks then
            picker.lsp_declarations()
          elseif ok_fzf then
            fzf.lsp_declarations({
              jump_to_single_result = true,
              winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
            })
          elseif ok_telescope then
            builtin.lsp_declarations()
          elseif ok_mini then
            mini_pick.pickers.lsp({ scope = "declaration" })
          else
            vim.lsp.buf.declaration()
          end
        end,
        desc = "Declaration of current symbol",
        cond = "textDocument/declaration",
      },
      ["<leader>lf"] = {
        function() vim.lsp.buf.code_action({ context = { only = { "source.fixAll" } }, apply = true }) end,
        desc = "Fix All",
        cond = "textDocument/codeAction",
      },
      ["<leader>lo"] = {
        function()
          vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } }, apply = true })
          vim.lsp.buf.code_action({ context = { only = { "source.removeUnused" } }, apply = true })
        end,
        desc = "Oraganize imports",
        cond = "textDocument/codeAction",
      },
      ["<leader>li"] = {
        function()
          local ok_fzf, fzf = pcall(require, "fzf-lua")
          local ok_telescope, builtin = pcall(require, "telescope.builtin")
          local ok_snacks, picker = pcall(require, "snacks.picker")
          local ok_mini, mini_pick = pcall(require, "mini.pick")

          if ok_snacks then
            picker.lsp_implementations()
          elseif ok_fzf then
            fzf.lsp_implementations({ winopts = { preview = { layout = "vertical", vertical = "up:60%" } } })
          elseif ok_telescope then
            builtin.lsp_implementations()
          elseif ok_mini then
            mini_pick.pickers.lsp({ scope = "implementation" })
          else
            vim.lsp.buf.implementation()
          end
        end,
        desc = "Goto implementation",
        cond = "textDocument/implementation",
      },
      ["<leader>lt"] = {
        function()
          local ok_fzf, fzf = pcall(require, "fzf-lua")
          local ok_telescope, builtin = pcall(require, "telescope.builtin")
          local ok_snacks, picker = pcall(require, "snacks.picker")
          local ok_mini, mini_pick = pcall(require, "mini.pick")

          if ok_snacks then
            picker.lsp_type_definitions()
          elseif ok_fzf then
            fzf.lsp_type_definitions({
              jump_to_single_result = true,
              winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
            })
          elseif ok_telescope then
            builtin.lsp_type_definitions()
          elseif ok_mini then
            mini_pick.pickers.lsp({ scope = "type_definition" })
          else
            vim.lsp.buf.type_definition()
          end
        end,
        desc = "Type definition",
        cond = "textDocument/typeDefinition",
      },
      ["<leader>F"] = {
        function() vim.cmd [[EslintFixAll]] end,
        cond = function(client) return client.name == "eslint" end,
      },
      grr = {
        function()
          local ok_fzf, fzf = pcall(require, "fzf-lua")
          local ok_telescope, builtin = pcall(require, "telescope.builtin")
          local ok_snacks, picker = pcall(require, "snacks.picker")
          local ok_mini, mini_pick = pcall(require, "mini.pick")

          if ok_snacks then
            picker.lsp_references()
          elseif ok_fzf then
            fzf.lsp_references({
              jump_to_single_result = true,
              winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
            })
          elseif ok_telescope then
            builtin.lsp_references()
          elseif ok_mini then
            mini_pick.pickers.lsp({ scope = "references" })
          else
            vim.lsp.buf.references()
          end
        end,
        desc = "Goto references",
        cond = "textDocument/references",
      },
      ["<leader>lR"] = {
        function()
          local ok_fzf, fzf = pcall(require, "fzf-lua")
          local ok_telescope, builtin = pcall(require, "telescope.builtin")
          local ok_snacks, picker = pcall(require, "snacks.picker")
          local ok_mini, mini_pick = pcall(require, "mini.pick")

          if ok_snacks then
            picker.lsp_references()
          elseif ok_fzf then
            fzf.lsp_references({
              jump_to_single_result = true,
              winopts = { preview = { layout = "vertical", vertical = "up:60%" } },
            })
          elseif ok_telescope then
            builtin.lsp_references()
          elseif ok_mini then
            mini_pick.pickers.lsp({ scope = "references" })
          else
            vim.lsp.buf.references()
          end
        end,
        desc = "Goto references",
        cond = "textDocument/references",
      },
      ["]e"] = {
        function() vim.diagnostic.goto_next({ severity = vim.diagnostic.severity.ERROR }) end,
        desc = "Goto Next Error",
      },
      ["[e"] = {
        function() vim.diagnostic.goto_prev({ severity = vim.diagnostic.severity.ERROR }) end,
        desc = "Goto Next Error",
      },
      ["]d"] = { vim.diagnostic.goto_next, desc = "Goto Next diagnostic" },
      ["[d"] = { vim.diagnostic.goto_prev, desc = "Goto Next diagnostic" },
      ["<leader>lw"] = {
        function()
          local ok_fzf, fzf = pcall(require, "fzf-lua")
          local ok_telescope, builtin = pcall(require, "telescope.builtin")
          local ok_snacks, picker = pcall(require, "snacks.picker")
          local ok_mini, mini_pick = pcall(require, "mini.pick")

          if ok_snacks then
            picker.lsp_symbols()
          elseif ok_fzf then
            fzf.lsp_document_symbols()
          elseif ok_telescope then
            builtin.lsp_document_symbols()
          elseif ok_mini then
            mini_pick.pickers.lsp({ scope = "document_symbol" })
          else
            vim.lsp.buf.document_symbol()
          end
        end,
        desc = "Document symbols",
        cond = "textDocument/documentSymbol",
      },
      ["<leader>lW"] = {
        function()
          local ok_fzf, fzf = pcall(require, "fzf-lua")
          local ok_telescope, builtin = pcall(require, "telescope.builtin")
          local ok_snacks, picker = pcall(require, "snacks.picker")
          local ok_mini, mini_pick = pcall(require, "mini.pick")

          if ok_snacks then
            picker.lsp_workspace_symbols()
          elseif ok_fzf then
            fzf.lsp_workspace_symbols()
          elseif ok_telescope then
            builtin.lsp_dynamic_workspace_symbols()
          elseif ok_mini then
            mini_pick.pickers.lsp({ scope = "workspace_symbol" })
          else
            vim.lsp.buf.workspace_symbol()
          end
        end,
        desc = "Workspace symbols",
        cond = "workspace/symbol",
      },
      gro = {
        function()
          local ok_fzf, fzf = pcall(require, "fzf-lua")
          local ok_telescope, builtin = pcall(require, "telescope.builtin")
          local ok_snacks, picker = pcall(require, "snacks.picker")
          local ok_mini, mini_pick = pcall(require, "mini.pick")

          if ok_snacks then
            picker.lsp_symbols()
          elseif ok_fzf then
            fzf.lsp_document_symbols()
          elseif ok_telescope then
            builtin.lsp_document_symbols()
          elseif ok_mini then
            mini_pick.pickers.lsp({ scope = "document_symbol" })
          else
            vim.lsp.buf.document_symbol()
          end
        end,
        desc = "Document symbols",
        cond = "textDocument/documentSymbol",
      },
      grO = {
        function()
          local ok_fzf, fzf = pcall(require, "fzf-lua")
          local ok_telescope, builtin = pcall(require, "telescope.builtin")
          local ok_snacks, picker = pcall(require, "snacks.picker")
          local ok_mini, mini_pick = pcall(require, "mini.pick")

          if ok_snacks then
            picker.lsp_workspace_symbols()
          elseif ok_fzf then
            fzf.lsp_workspace_symbols()
          elseif ok_telescope then
            builtin.lsp_dynamic_workspace_symbols()
          elseif ok_mini then
            mini_pick.pickers.lsp({ scope = "workspace_symbol" })
          else
            vim.lsp.buf.workspace_symbol()
          end
        end,
        desc = "Workspace symbols",
        cond = "workspace/symbol",
      },
      ["<leader>lr"] = { vim.lsp.buf.rename, desc = "Rename", cond = "textDocument/rename" },
      ["<leader>la"] = {
        function()
          local ok_fzf, fzf = pcall(require, "fzf-lua")
          if ok_fzf then
            fzf.lsp_code_actions()
          else
            vim.lsp.buf.code_action()
          end
        end,
        desc = "Code actions",
        cond = "textDocument/codeAction",
      },
      ["<leader>lI"] = {
        function() vim.cmd "LspInfo" end,
        desc = "LspInfo",
      },
      ["<leader>uY"] = {
        function() require("astrolsp.toggles").buffer_semantic_tokens() end,
        desc = "Toggle LSP semantic highlight (buffer)",
        cond = function(client, bufnr)
          return client.server_capabilities.semanticTokensProvider and vim.lsp.semantic_tokens
        end,
      },
      ["<leader>uI"] = {
        function() require("astrolsp.toggles").buffer_inlay_hints() end,
        desc = "Toggle LSP inlay highlight (buffer)",
        cond = function(client) return client.server_capabilities.inlayHintProvider and vim.lsp.inlay_hint end,
      },
    },
    x = {
      ["<leader>la"] = {
        function()
          local ok, fzf = pcall(require, "fzf-lua")
          if not ok then
            vim.lsp.buf.code_action()
          else
            fzf.lsp_code_actions()
          end
        end,
        desc = "Code actions",
        cond = "textDocument/codeAction",
      },
    },
  },
  handlers = {
    function(server, opts)
      opts.capabilities = require("user.config.util").get_lsp_capabilities(opts.capabilities)
      require("lspconfig")[server].setup(opts)
    end,
  },
}
return M
