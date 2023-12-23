local function getVisualSelection()
  vim.cmd 'noau normal! "vy"'
  local text = vim.fn.getreg 'v'
  vim.fn.setreg('v', {})

  text = string.gsub(text, '\n', '')
  if #text > 0 then
    return text
  else
    return ''
  end
end
return {
  'nvim-telescope/telescope.nvim',
  version = '*',
  keys = {
    { '<leader>fr', '<cmd>Telescope resume<cr>', desc = '[R]esume' },
    { '<leader>fR', '<cmd>Telescope registers<cr>', desc = '[R]egisters' },
    { '<leader>fz', '<cmd>Telescope spell_suggest<cr>', desc = '[S]pellings' },
    {
      '<leader>fs',
      function()
        local text = getVisualSelection()
        require('telescope.builtin').live_grep({ default_text = text })
      end,
      mode = 'v',
      desc = '[S]earch [S]election',
    },
    {
      '<leader>fS',
      '<cmd>Telescope current_buffer_fuzzy_find<cr>',
      desc = 'Search(Current Buffer)',
    },
    {
      '<leader>fS',
      function()
        local text = getVisualSelection()
        require('telescope.builtin').current_buffer_fuzzy_find({ default_text = text })
      end,
      mode = 'v',
      desc = '[S]earch [S]election(Buffer)',
    },
  },
  opts = function(_, opts)
    -- run the core AstroNvim configuration function with the options table
    -- require 'plugins.configs.telescope'(plugin, opts)
    local actions = require 'telescope.actions'
    local action_layout = require 'telescope.actions.layout'
    local themes = require 'telescope.themes'
    local dropdown = {
      layout_strategy = 'vertical',
      layout_config = { width = 0.6, preview_cutoff = 1, prompt_position = 'top' },
    }
    opts.defaults.mappings.i['<A-CR>'] = actions.select_tab
    opts.defaults.mappings.n['<A-CR>'] = actions.select_tab
    opts.defaults.mappings.i['<A-p>'] = action_layout.toggle_preview
    opts.defaults.mappings.n['<A-p>'] = action_layout.toggle_preview
    opts.pickers = {
      lsp_references = dropdown,
      lsp_definitions = dropdown,
      git_branches = dropdown,
      git_commits = dropdown,
      git_bcommits = dropdown,
      lsp_document_symbols = dropdown,
      lsp_workspace_symbols = dropdown,
    }
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
