---@type LazySpec
return {
  'chrisgrieser/nvim-scissors',
  opts = {
    snippetDir = vim.fn.stdpath 'config' .. '/snippets/', -- this is already the default, but I want to be explicit
    ---@type "yq"|"jq"|"none"|string[]
    jsonFormatter = vim.fn.executable 'jq' == 1 and 'jq' or 'none',
  },
  keys = {
    { '<leader>s', '', desc = '+[S]nippets' },
    {
      '<leader>se',
      function()
        require('scissors').editSnippet()
      end,
      desc = 'Snippet: [E]dit',
    },
    {
      '<leader>sn',
      function()
        require('scissors').addNewSnippet()
      end,
      mode = { 'n', 'x' },
      { desc = 'Snippet: [N]ew' },
    },
    { '<leader>sd', '<cmd>AutoSession delete<cr>', desc = '[D]elte' },
  },
}
