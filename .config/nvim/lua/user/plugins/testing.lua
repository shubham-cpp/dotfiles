---@type LazySpec
return {
  'nvim-neotest/neotest',
  dependencies = {
    'nvim-neotest/nvim-nio',
    'nvim-lua/plenary.nvim',
    'antoinemadec/FixCursorHold.nvim',
    'nvim-treesitter/nvim-treesitter',
    'zidhuss/neotest-minitest',
    'marilari88/neotest-vitest',
    'nvim-neotest/neotest-jest',
    { 'fredrikaverpil/neotest-golang', version = '*' }, -- Installation
    'nvim-neotest/neotest-plenary',
  },
  cmd = { 'Neotest' },
  opts = function()
    return {
      adapters = {
        require 'neotest-plenary',
        require 'neotest-golang',
        require 'neotest-vitest'({
          -- Filter directories when searching for test files. Useful in large projects (see Filter directories notes).
          filter_dir = function(name, rel_path, root)
            return name ~= 'node_modules'
          end,
        }),
        require 'neotest-jest'({
          -- Filter directories when searching for test files. Useful in large projects (see Filter directories notes).
          filter_dir = function(name, rel_path, root)
            return name ~= 'node_modules'
          end,
        }),
      },
    }
  end,
  keys = {
    {
      '<leader>ts',
      '<cmd>Neotest summary<cr>',
      desc = 'Test Summary',
    },
    {
      '<leader>tt',
      function()
        require('neotest').run.run(vim.fn.expand '%')
      end,
      desc = 'Run Test(file)',
    },
    {
      '<leader>twj',
      function()
        require('neotest').run.run({ vim.fn.expand '%', jestCommand = 'npx jest --watch ' })
      end,
      desc = 'Run Test(jest - watch)',
    },
    {
      '<leader>twJ',
      function()
        require('neotest').run.run({ jestCommand = 'npx jest --watch' })
      end,
      desc = 'Run Test(jest - watch - project)',
    },
    {
      '<leader>twv',
      function()
        require('neotest').run.run({ vim.fn.expand '%', vitestCommand = 'npx vitest --watch' })
      end,
      desc = 'Run Test(vitest - watch)',
    },
    {
      '<leader>twV',
      function()
        require('neotest').run.run({ vitestCommand = 'npx vitest --watch' })
      end,
      desc = 'Run Test(vitest - watch - project)',
    },
  },
}
