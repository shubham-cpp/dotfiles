local mappings = {
  add = 'ys',
  delete = 'ds',
  replace = 'cs',
  find = 'gzf',
  find_left = 'gzF',
  highlight = 'gzh',
  updates = 'gzn',
}

---@type LazySpec
return {
  'echasnovski/mini.surround',
  enabled = false,
  keys = {
    { 'gz', '', desc = '+surround' },
    { mappings.add, mode = { 'n', 'v' }, desc = 'Surround add' },
    { mappings.delete, mode = { 'n' }, desc = 'Surround delete' },
    { mappings.replace, mode = { 'n' }, desc = 'Surround replace' },
    { mappings.find, mode = { 'n' }, desc = 'Surround find' },
    { mappings.highlight, mode = { 'n' }, desc = 'Surround highlight' },
  },
  opts = {
    -- search_method = 'cover_or_next',
    n_lines = 500,
    mappings = {
      add = mappings.add, -- Add surrounding in Normal and Visual modes
      delete = mappings.delete, -- Delete surrounding
      replace = mappings.replace, -- Replace surrounding
      find = mappings.find, -- Find surrounding (to the right)
      find_left = mappings.find_left, -- Find surrounding (to the left)
      highlight = mappings.highlight, -- Highlight surrounding
      update_n_lines = mappings.updates, -- Update `n_lines`
    },
  },
  config = function(_, opts)
    require('mini.surround').setup(opts)
  end,
}
