---@type LazySpec
return {
  'mikavilpas/yazi.nvim',
  dependencies = {
    'nvim-lua/plenary.nvim',
  },
  -- event = 'VeryLazy',
  keys = {
    {
      '<leader>-',
      function()
        require('yazi').yazi()
      end,
      desc = 'Open the file manager',
    },
    {
      -- Open in the current working directory
      '<leader>\\',
      function()
        require('yazi').yazi(nil, vim.fn.getcwd())
      end,
      desc = "Open the file manager in nvim's working directory",
    },
  },
  ---@type YaziConfig
  opts = {
    open_for_directories = false,
    integrations = {
      --- What should be done when the user wants to grep in a directory
      ---@param directory string
      grep_in_directory = function(directory)
        require('fzf-lua').live_grep_native({
          prompt = 'Search in ' .. directory,
          cwd = directory,
        })
      end,
    },
  },
}
