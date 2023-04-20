return {
  'nvim-telescope/telescope.nvim',
  version = '*',
  opts = function(_, opts)
    -- run the core AstroNvim configuration function with the options table
    -- require 'plugins.configs.telescope'(plugin, opts)
    local actions = require 'telescope.actions'
    local action_layout = require 'telescope.actions.layout'
    opts.defaults.mappings.i['<A-CR>'] = actions.select_tab
    opts.defaults.mappings.n['<A-CR>'] = actions.select_tab
    opts.defaults.mappings.i['<A-p>'] = action_layout.toggle_preview
    opts.defaults.mappings.n['<A-p>'] = action_layout.toggle_preview
    -- opts.defaults.file_ignore_patterns = vim.tbl_extend('force', opts.defaults.file_ignore_patterns or {}, {
    --   '.backup',
    --   '.swap',
    --   '.langservers',
    --   '.undo',
    --   '.git/',
    --   'node_modules/%',
    --   'vendor',
    --   '.cache',
    --   -- '.vscode%',
    --   'classes',
    --   '.venv',
    --   '%.png',
    --   '%.jpeg',
    --   '%.jpg',
    --   '%.mkv',
    --   '%.mp3',
    --   '%.mp4',
    --   '%.out',
    --   '%.class',
    --   '__pycache__',
    --   '%.o',
    --   'patches',
    --   'packer_compiled.lua',
    -- })
    return opts
  end,
}
