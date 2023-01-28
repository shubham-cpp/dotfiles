local actions = require 'telescope.actions'
local action_layout = require 'telescope.actions.layout'

local config = {
  defaults = {
    mappings = {
      i = {
        -- ['<esc>'] = actions.close,
        ['<A-CR>'] = actions.select_tab,
        ['<M-p>'] = action_layout.toggle_preview,
      },
      n = { ['<A-CR>'] = actions.select_tab, ['<M-p>'] = action_layout.toggle_preview },
    },
    file_ignore_patterns = {
      '.backup',
      '.swap',
      '.langservers',
      '.undo',
      '.git/',
      'node_modules',
      'vendor',
      '.cache',
      -- '.vscode%',
      'classes',
      '.venv',
      '%.png',
      '%.jpeg',
      '%.jpg',
      '%.mkv',
      '%.mp3',
      '%.mp4',
      '%.out',
      '%.class',
      '__pycache__',
      '%.o',
      'patches',
      'packer_compiled.lua',
    },
  },
}

return config
