---@type LazySpec
return {
  {
    'williamboman/mason.nvim',
    opts = function(_, opts)
      opts.ensure_installed = require('my_config.util').unique_append_table(opts.ensure_installed, {
        'cspell',
        'html-lsp',
        'stylelint',
        'css-lsp',
        'css-variables-language-server',
        'cssmodules-language-server',
        'prettierd',
        'emmet-language-server',
      })
      return opts
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = require('my_config.util').unique_append_table(opts.ensure_installed, {
        'html',
        'css',
        'scss',
        'robot',
        'http',
        'regex',
      })
      return opts
    end,
  },
  {
    'AstroNvim/astrolsp',
    opts = function(_, opts)
      opts.config = vim.tbl_extend('force', opts.config or {}, {
        html = {},
        css_variables = {},
        cssmodules_ls = {},
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
        emmet_language_server = {
          filetypes = {
            'css',
            'less',
            'sass',
            'scss',
            'eruby',
            'html',
            'htmldjango',
            'pug',
            'heex',
            'blade',
            'vue',
            'svelte',
            'astro',
            'javascriptreact',
            'typescriptreact',
          },
          on_attach = function(client, bufnr)
            vim.keymap.set('i', '<C-t>', function()
              client.request(
                'textDocument/completion',
                vim.lsp.util.make_position_params(0, client.offset_encoding),
                function(_, result)
                  local textEdit = result.items[1].textEdit
                  local snip_string = textEdit.newText
                  textEdit.newText = ''
                  vim.lsp.util.apply_text_edits({ textEdit }, bufnr, client.offset_encoding)
                  local ok, luasnip = pcall(require, 'luasnip')
                  if ok then
                    luasnip.lsp_expand(snip_string)
                  else
                    vim.snippet.expand(snip_string)
                  end
                end,
                bufnr
              )
            end, { buffer = bufnr, desc = 'Emmet Expand', noremap = true })
          end,
        },
      })
      return opts
    end,
  },
}
