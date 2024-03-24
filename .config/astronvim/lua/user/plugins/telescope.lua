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

local function filename_first(_, path)
  local tail = vim.fs.basename(path)
  local parent = vim.fs.dirname(path)
  if parent == '.' then
    return tail
  end
  return string.format('%s\t\t%s', tail, parent)
end

local au_telescope = vim.api.nvim_create_augroup('au_telescope', { clear = true })

return {
  'nvim-telescope/telescope.nvim',
  keys = {
    {
      '<C-p>',
      function()
        require('telescope.builtin').find_files({
          path_display = filename_first,
          file_ignore_patterns = { 'node_modules' },
        })
      end,
      desc = 'Find Files',
    },
    {
      '<leader>ff',
      function()
        require('telescope.builtin').find_files({
          path_display = filename_first,
          file_ignore_patterns = { 'node_modules' },
        })
      end,
      desc = 'Find Files',
    },
    { '<leader>fw', '<cmd>Telescope grep_string<cr>', desc = '[G]rep current word' },
    { '<leader>fr', '<cmd>Telescope resume<cr>', desc = '[R]esume' },
    { '<leader>fR', '<cmd>Telescope registers<cr>', desc = '[R]egisters' },
    { '<leader>fz', '<cmd>Telescope spell_suggest<cr>', desc = '[S]pellings' },
    { '<leader>fs', '<cmd>Telescope live_grep<cr>', desc = 'Search Project' },
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
    {
      '<leader>fn',
      require('user.lsp.util').cmd 'Telescope find_files cwd=~/.config/astronvim/lua/user',
      desc = 'Astronvim User Config',
    },
    {
      '<leader>fN',
      require('user.lsp.util').cmd 'Telescope find_files cwd=~/.config/nvim',
      desc = 'Astronvim Config',
    },
    {
      '<leader>fd',
      require('user.lsp.util').cmd 'Telescope find_files cwd=~/Documents/dotfiles/.config hidden=true',
      desc = 'Dotfiles',
    },
  },
  opts = function(_, opts)
    local actions = require 'telescope.actions'
    local action_layout = require 'telescope.actions.layout'
    vim.api.nvim_create_autocmd('FileType', {
      pattern = 'TelescopeResults',
      group = au_telescope,
      callback = function(ctx)
        vim.api.nvim_buf_call(ctx.buf, function()
          vim.fn.matchadd('TelescopeParent', '\t\t.*$')
          vim.api.nvim_set_hl(0, 'TelescopeParent', { link = 'Comment' })
        end)
      end,
    })
    local dropdown = {
      layout_strategy = 'vertical',
      layout_config = { width = 0.6, preview_cutoff = 1, prompt_position = 'top' },
    }
    -- opts.defaults.mappings.i['<A-CR>'] = actions.select_tab
    -- opts.defaults.mappings.n['<A-CR>'] = actions.select_tab
    opts.defaults.mappings.n['<A-p>'] = action_layout.toggle_preview
    opts.defaults.mappings.i['<A-p>'] = action_layout.toggle_preview
    opts.defaults.mappings.i['<C-c>'] = actions.close
    opts.pickers = {
      buffers = {
        ignore_current_buffer = true,
        sort_lastused = true,
      },
      lsp_references = dropdown,
      lsp_definitions = dropdown,
      git_branches = dropdown,
      git_commits = dropdown,
      git_bcommits = dropdown,
      lsp_document_symbols = dropdown,
      lsp_workspace_symbols = dropdown,
    }
    return opts
  end,
}
