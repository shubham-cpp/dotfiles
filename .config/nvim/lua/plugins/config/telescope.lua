local M = {}
local au_telescope = vim.api.nvim_create_augroup('au_telescope', { clear = true })

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

local function telescope_create_file()
  require('telescope.builtin').find_files({
    prompt_title = 'Create File',
    find_command = { 'fd', '--type', 'd', '.', vim.fn.getcwd() },
    attach_mappings = function(_, map)
      local state = require 'telescope.actions.state'
      local actions = require 'telescope.actions'
      map('i', '<CR>', function(prompt_bufnr)
        local content = state.get_selected_entry()
        actions.close(prompt_bufnr)
        -- vim.print('content : ' .. content.cwd .. '/' .. content.value)
        local dir = content.value
        local name = vim.fn.input 'File Name: '
        vim.cmd('e ' .. dir .. name)
        vim.cmd 'w ++p'
      end)
      return true
    end,
  })
end

local function project_files()
  local opts = {
    path_display = filename_first,
    previewer = false,
  } -- define here if you want to define something

  if vim.b.gitsigns_head then
    opts.show_untracked = true
    require('telescope.builtin').git_files(opts)
  else
    require('telescope.builtin').find_files(opts)
  end
end

M.keys = function()
  local builtin = require 'telescope.builtin'
  return {
    {
      '<C-p>',
      project_files,
      desc = 'Git/Files',
    },
    {
      '<leader>ff',
      function()
        builtin.find_files({ path_display = filename_first })
      end,
      desc = 'Files',
    },
    {
      '<leader>fF',
      function()
        builtin.git_files({ show_untracked = true, use_file_path = true, path_display = filename_first })
      end,
      desc = 'Git Files(use_file_path)',
    },
    { '<leader>fp', telescope_create_file, desc = 'CreateFile' },
    { '<leader>fc', telescope_create_file, desc = 'CreateFile' },
    { '<leader>fk', builtin.keymaps, desc = 'keymaps' },
    { '<leader>fS', builtin.current_buffer_fuzzy_find, desc = 'Search(Buffer)' },
    -- { '<leader>fs', builtin.live_grep, desc = 'Search(Project)' },
    { '<leader>fs', '<cmd>Telescope egrepify<cr>', desc = 'Search(Project)' },
    { '<leader>fw', builtin.grep_string, desc = 'Search Current Word' },
    { '<leader>fb', builtin.buffers, desc = 'Buffers' },
    { '<leader>fh', builtin.help_tags, desc = 'Help' },
    { '<leader>fR', builtin.oldfiles, desc = 'Old Files' },
    { '<leader>fr', builtin.resume, desc = 'Resume' },
    {
      '<leader>fs',
      function()
        local text = getVisualSelection()
        builtin.live_grep({ default_text = text })
      end,
      mode = 'v',
      desc = '[S]earch [S]election',
    },
    {
      '<leader>fS',
      function()
        local text = getVisualSelection()
        builtin.current_buffer_fuzzy_find({ default_text = text })
      end,
      mode = 'v',
      desc = '[S]earch [S]election(Buffer)',
    },
    {
      '<leader>fn',
      function()
        builtin.find_files({
          cwd = vim.fn.stdpath 'config',
          path_display = filename_first,
        })
      end,
      desc = 'Neovim Config',
    },
    {
      '<leader>fd',
      function()
        builtin.git_files({
          cwd = vim.fn.expand '$HOME/Documents/dotfiles',
          path_display = filename_first,
          show_untracked = true,
        })
      end,
      desc = 'Dotfiles',
    },
    { '<leader>gb', builtin.git_branches, desc = 'Git Branches' },
    { '<leader>fg', builtin.git_status, desc = 'Git Status' },
    { '<leader>gS', builtin.git_stash, desc = 'Git Stash' },
    { '<leader>gc', builtin.git_commits, desc = 'Git Commits' },
    { '<leader>gC', builtin.git_bcommits, desc = 'Git Buffer Commits' },
  }
end

M.config = function()
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
  local actions = require 'telescope.actions'
  local action_layout = require 'telescope.actions.layout'
  local builtin = require 'telescope.builtin'

  local dropdown = {
    layout_strategy = 'vertical',
    layout_config = { width = 0.6, preview_cutoff = 1, prompt_position = 'top' },
  }
  require('telescope').setup({
    defaults = {
      -- path_display = { "truncate" },
      sorting_strategy = 'ascending',
      set_env = { ['COLORTERM'] = 'truecolor' },
      layout_config = {
        horizontal = { prompt_position = 'top', preview_width = 0.40 },
        vertical = { mirror = false },
        width = 0.87,
        height = 0.80,
        preview_cutoff = 120,
      },
      mappings = {
        i = {
          ['<C-j>'] = actions.move_selection_next,
          ['<C-k>'] = actions.move_selection_previous,
          ['<C-n>'] = actions.cycle_history_next,
          ['<C-p>'] = actions.cycle_history_prev,
          ['<C-c>'] = actions.close,
          ['<A-p>'] = action_layout.toggle_preview,

          ['<Tab>'] = actions.toggle_selection + actions.move_selection_next,
          ['<S-Tab>'] = actions.toggle_selection + actions.move_selection_previous,
          ['<C-q>'] = actions.smart_send_to_qflist + actions.open_qflist,
        },
        n = {
          q = actions.close,
          ['<A-p>'] = action_layout.toggle_preview,
          ['<Space>'] = actions.toggle_selection,
        },
      },
      vimgrep_arguments = {
        'rg',
        '--color=never',
        '--no-heading',
        '--with-filename',
        '--line-number',
        '--column',
        '--smart-case',
        '--hidden',
        '--trim', -- add this value
        '--glob=!.git/',
        '--glob=!node_modules/',
        '--glob=!.venv/',
        '--glob=!venv/',
      },
    },

    pickers = {
      buffers = {
        ignore_current_buffer = true,
        sort_lastused = true,
        mappings = {
          i = { ['<c-d>'] = 'delete_buffer' },
          n = { d = 'delete_buffer' },
        },
        prompt_title = 'Buffers',
        results_title = 'Command History',
        prompt_prefix = ' ',
        sorting_strategy = 'ascending',
        layout_strategy = 'bottom_pane',
        layout_config = {
          prompt_position = 'top',
        },
        border = true,
        borderchars = {
          prompt = { ' ', ' ', '─', ' ', ' ', ' ', '─', '─' },
          results = { '─', ' ', ' ', ' ', '─', '─', ' ', ' ' },
          preview = { '─', ' ', '─', '│', '┬', '─', '─', '╰' },
        },
      },
      help_tags = {
        prompt_title = 'Help',
        results_title = 'Help Tags',
        prompt_prefix = ' ',
        sorting_strategy = 'ascending',
        layout_strategy = 'bottom_pane',
        layout_config = {
          prompt_position = 'top',
          height = 25,
        },
        border = true,
        borderchars = {
          prompt = { ' ', ' ', '─', ' ', ' ', ' ', '─', '─' },
          results = { '─', ' ', ' ', ' ', '─', '─', ' ', ' ' },
          preview = { '─', ' ', '─', '│', '┬', '─', '─', '╰' },
        },
      },
      keymaps = {
        prompt_title = 'keymaps',
        results_title = 'keymaps',
        prompt_prefix = ' ',
        sorting_strategy = 'ascending',
        layout_strategy = 'bottom_pane',
        layout_config = {
          prompt_position = 'top',
          height = 25,
        },
        border = true,
        borderchars = {
          prompt = { ' ', ' ', '─', ' ', ' ', ' ', '─', '─' },
          results = { '─', ' ', ' ', ' ', '─', '─', ' ', ' ' },
          preview = { '─', ' ', '─', '│', '┬', '─', '─', '╰' },
        },
      },
      command_history = {
        prompt_title = '',
        results_title = 'Command History',

        --           
        --  卑 喝   
        prompt_prefix = ' ',
        -- prompt_prefix = " ",
        -- prompt_prefix = " ",
        -- prompt_prefix = " ",

        sorting_strategy = 'ascending',
        layout_strategy = 'bottom_pane',
        layout_config = {
          prompt_position = 'top',
          height = 25,
        },
        border = true,
        borderchars = {
          prompt = { ' ', ' ', '─', ' ', ' ', ' ', '─', '─' },
          results = { '─', ' ', ' ', ' ', '─', '─', ' ', ' ' },
          preview = { '─', ' ', '─', '│', '┬', '─', '─', '╰' },
        },
        mappings = {
          i = { ['<CR>'] = actions.edit_command_line },
          n = { ['<CR>'] = actions.edit_command_line },
        },
      },
      lsp_references = dropdown,
      lsp_definitions = dropdown,
      git_branches = dropdown,
      git_commits = dropdown,
      git_bcommits = dropdown,
      lsp_document_symbols = dropdown,
      lsp_workspace_symbols = dropdown,
    },
    extensions = {
      ['zf-native'] = {
        -- options for sorting file-like items
        file = {
          -- override default telescope file sorter
          enable = true,
          -- highlight matching text in results
          highlight_results = true,
          -- enable zf filename match priority
          match_filename = true,
          -- optional function to define a sort order when the query is empty
          initial_sort = nil,
        },

        -- options for sorting all other items
        generic = {
          -- override default telescope generic item sorter
          enable = true,
          -- highlight matching text in results
          highlight_results = true,
          -- disable zf filename match priority
          match_filename = false,
          -- optional function to define a sort order when the query is empty
          initial_sort = nil,
        },
      },
      ['ui-select'] = require('telescope.themes').get_dropdown({}),
    },
  })
  require('telescope').load_extension 'zf-native'
  require('telescope').load_extension 'ui-select'
  require('telescope').load_extension 'egrepify'
  vim.lsp.handlers['textDocument/definition'] = builtin.lsp_definitions
  vim.lsp.handlers['textDocument/typeDefinition'] = builtin.lsp_type_definitions
  vim.lsp.handlers['textDocument/implementation'] = builtin.lsp_implementations
  vim.lsp.handlers['textDocument/references'] = builtin.lsp_references
  vim.lsp.handlers['textDocument/documentSymbol'] = builtin.lsp_document_symbols
  vim.lsp.handlers['workspace/symbol'] = builtin.lsp_workspace_symbols
  vim.lsp.handlers['callHierarchy/incomingCalls'] = builtin.lsp_incoming_calls
  vim.lsp.handlers['callHierarchy/outgoingCalls'] = builtin.lsp_outgoing_calls
end
-- M.config()

return M
