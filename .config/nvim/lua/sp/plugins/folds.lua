return {
  'kevinhwang91/nvim-ufo',
  dependencies = { 'kevinhwang91/promise-async' },
  event = { 'BufRead', 'BufNewFile' },
  keys = {
    {
      'zR',
      function()
        require('ufo').openAllFolds()
      end,
      desc = 'Ufo Open Folds',
    },
    {
      'zM',
      function()
        require('ufo').closeAllFolds()
      end,
      desc = 'Ufo Close Fold',
    },
  },
  opts = {
    preview = {
      mappings = {
        scrollB = '<C-b>',
        scrollF = '<C-f>',
        scrollU = '<C-u>',
        scrollD = '<C-d>',
      },
    },
    provider_selector = function(_, filetype, buftype)
      local function handleFallbackException(bufnr, err, providerName)
        if type(err) == 'string' and err:match 'UfoFallbackException' then
          return require('ufo').getFolds(bufnr, providerName)
        else
          return require('promise').reject(err)
        end
      end

      return (filetype == '' or buftype == 'nofile') and 'indent' -- only use indent until a file is opened
        or function(bufnr)
          return require('ufo')
            .getFolds(bufnr, 'lsp')
            :catch(function(err)
              return handleFallbackException(bufnr, err, 'treesitter')
            end)
            :catch(function(err)
              return handleFallbackException(bufnr, err, 'indent')
            end)
        end
    end,
  },
}
