local configs = require 'nvim-treesitter.configs'
---@diagnostic disable-next-line: missing-fields
configs.setup({
  highlight = {
    enable = true,
    -- additional_vim_regex_highlighting = { 'markdown', 'xml' },
    disable = function(_, bufnr)
      local line_count = vim.api.nvim_buf_line_count(bufnr)
      if line_count > 2500 then
        return true
      end
    end,
  },
  indent = { enable = false },
  matchup = { enable = true },
  ensure_installed = {
    'bash',
    'dockerfile',
    'fish',
    'query',
    'regex',
    'rasi',
    'sxhkdrc',
    'vim',
    'vimdoc',
    'diff',
    'git_config',
    'git_rebase',
    'gitattributes',
    'gitcommit',
    'gitignore',
    'hyprlang',
    'ini',
    'sql',
    'ssh_config',
    'tmux',
    'toml',
    'xml',
    'zathurarc',
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = '<C-space>',
      node_incremental = '<C-space>',
      scope_incremental = false,
      node_decremental = '<bs>',
    },
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true,
      keymaps = {
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        ['ac'] = '@class.outer',
        ['ic'] = { query = '@class.inner', desc = 'Select inner part of a class region' },
        ['al'] = { query = '@scope', query_group = 'locals', desc = 'Select language scope' },
      },
      selection_modes = {
        ['@parameter.outer'] = 'v', -- charwise
        ['@function.outer'] = 'V', -- linewise
        ['@class.outer'] = '<c-v>', -- blockwise
      },
      include_surrounding_whitespace = true,
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        [']m'] = '@function.outer',
        [']]'] = { query = '@class.outer', desc = 'Next class start' },
        [']l'] = '@loop.*',
        -- ["]o"] = { query = { "@loop.inner", "@loop.outer" } }
        [']s'] = { query = '@scope', query_group = 'locals', desc = 'Next scope' },
        [']z'] = { query = '@fold', query_group = 'folds', desc = 'Next fold' },
      },
      goto_next_end = {
        [']M'] = '@function.outer',
        [']['] = '@class.outer',
      },
      goto_previous_start = {
        ['[m'] = '@function.outer',
        ['[['] = '@class.outer',
      },
      goto_previous_end = {
        ['[M'] = '@function.outer',
        ['[]'] = '@class.outer',
      },
      goto_next = {
        [']i'] = '@conditional.outer',
      },
      goto_previous = {
        ['[i'] = '@conditional.outer',
      },
    },
    swap = {
      enable = true,
      swap_next = {
        ['>K'] = { query = '@block.outer', desc = 'Swap next block' },
        ['>F'] = { query = '@function.outer', desc = 'Swap next function' },
        ['>A'] = { query = '@parameter.inner', desc = 'Swap next argument' },
      },
      swap_previous = {
        ['<K'] = { query = '@block.outer', desc = 'Swap previous block' },
        ['<F'] = { query = '@function.outer', desc = 'Swap previous function' },
        ['<A'] = { query = '@parameter.inner', desc = 'Swap previous argument' },
      },
    },
  },
})

require('nvim-ts-autotag').setup({
  -- Defaults
  opts = {
    enable_close = true, -- Auto close tags
    enable_rename = true, -- Auto rename pairs of tags
    enable_close_on_slash = false, -- Auto close on trailing </
  },
  aliases = {
    heex = 'html',
  },
})

vim.keymap.set('n', '<LocalLeader>c', function()
  require('treesitter-context').go_to_context(vim.v.count1)
end, { silent = true, desc = 'Goto Context' })
vim.keymap.set('n', '<leader><up>', function()
  require('treesitter-context').go_to_context(vim.v.count1)
end, { silent = true, desc = 'Goto Context' })

-- for _, v in ipairs(vim.fn.readdir(vim.g.base46_cache)) do
--   dofile(vim.g.base46_cache .. v)
-- end
