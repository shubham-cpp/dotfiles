---@type LazySpec
return {
  {
    'AstroNvim/astrolsp',
    ---@module 'astrolsp'
    ---@type astrolsp.AstroLSPConfig
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
                compatibleVendorPrefixes = 'warn',
                duplicateProperties = 'warn',
                boxModel = 'warn',
                unknownVendorSpecificProperties = 'warn',
                float = 'error',
              },
            },
            scss = {
              validate = true,
              completion = {
                triggerPropertyValueCompletion = true,
                completePropertyWithSemicolon = true,
              },
              lint = {
                compatibleVendorPrefixes = 'warn',
                duplicateProperties = 'warn',
                boxModel = 'warn',
                unknownVendorSpecificProperties = 'warn',
                float = 'error',
              },
            },
          },
        },
        -- emmet_language_server = {
        --   filetypes = {
        --     'css',
        --     'less',
        --     'sass',
        --     'scss',
        --     'eruby',
        --     'html',
        --     'htmldjango',
        --     'pug',
        --     'heex',
        --     'blade',
        --     'vue',
        --     'svelte',
        --     'astro',
        --     'javascriptreact',
        --     'typescriptreact',
        --   },
        --   on_attach = function(client, bufnr)
        --     vim.keymap.set('i', '<C-t>', function()
        --       client.request(
        --         'textDocument/completion',
        --         vim.lsp.util.make_position_params(0, client.offset_encoding),
        --         function(_, result)
        --           local textEdit = result.items[1].textEdit
        --           local snip_string = textEdit.newText
        --           textEdit.newText = ''
        --           vim.lsp.util.apply_text_edits({ textEdit }, bufnr, client.offset_encoding)
        --           local ok, luasnip = pcall(require, 'luasnip')
        --           if ok then
        --             luasnip.lsp_expand(snip_string)
        --           else
        --             vim.snippet.expand(snip_string)
        --           end
        --         end,
        --         bufnr
        --       )
        --     end, { buffer = bufnr, desc = 'Emmet Expand', noremap = true })
        --   end,
        -- },
      },
      -- handlers = {
      --   emmet_language_server = false,
      -- },
    },
  },
  {
    'nvim-treesitter',
    opts = {
      ensure_installed = { 'html', 'css', 'scss' },
    },
  },
  {
    'mason.nvim',
    opts = {
      ensure_installed = {
        'html-lsp',
        'htmlhint',
        'stylelint',
        'css-lsp',
        'css-variables-language-server',
        'cssmodules-language-server',
        'prettierd',
        -- 'emmet-language-server',
        'emmet-ls',
      },
    },
  },
  {
    'nvim-lint',
    opts = {
      linters_by_ft = {
        html = { 'htmlhint' },
        css = { 'stylelint' },
        scss = { 'stylelint' },
        sass = { 'stylelint' },
        less = { 'stylelint' },
      },
    },
  },
}
