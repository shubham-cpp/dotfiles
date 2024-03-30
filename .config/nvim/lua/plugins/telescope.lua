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
  cmd = 'Telescope',
  dependencies = {
    'nvim-lua/plenary.nvim',
    { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make', enabled = false },
    'natecraddock/telescope-zf-native.nvim',
    'nvim-telescope/telescope-ui-select.nvim',
  },
  keys = function()
    local builtin = require 'telescope.builtin'
    return {
      {
        '<C-p>',
        function()
          builtin.find_files({ path_display = filename_first, previewer = false })
        end,
        desc = 'Files',
      },
      {
        '<leader>ff',
        function()
          builtin.find_files({ path_display = filename_first })
        end,
        desc = 'Files',
      },
      { '<leader>fk', builtin.keymaps, desc = 'keymaps' },
      { '<leader>fS', builtin.current_buffer_fuzzy_find, desc = 'Search(Buffer)' },
      { '<leader>fs', builtin.live_grep, desc = 'Search(Project)' },
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
          builtin.find_files({
            cwd = vim.fn.expand '$HOME/Documents/dotfiles',
            path_display = filename_first,
            hidden = true,
          })
        end,
        desc = 'Dotfiles',
      },
      { '<leader>gb', builtin.git_branches, desc = 'Git Branches' },
      { '<leader>fg', builtin.git_status, desc = 'Git Status' },
      { '<leader>gS', builtin.git_stash, desc = 'Git Stash' },
      { '<leader>gc', builtin.git_commits, desc = 'Git Commits' },
      { '<leader>gC', builtin.git_bcommits, desc = 'Git Buffer Commits' },
      -- { "<leader>fs", builtin.lsp_document_symbols },
      -- { "<leader>fw", builtin.lsp_workspace_symbols },
      -- { "<leader>fc", builtin.lsp_code_actions },
      -- { "<leader>fl", builtin.lsp_references },
      -- { "<leader>fd", builtin.lsp_definitions },
      -- { "<leader>fm", builtin.lsp_implementations },
      -- { "<leader>fo", builtin.lsp_document_diagnostics },
      -- { "<leader>fq", builtin.lsp_workspace_diagnostics },
      -- { "<leader>fp", builtin.lsp_dynamic_workspace_symbols },
      -- { "<leader>ft", builtin.lsp_type_definitions },
      -- { "<leader>fy", builtin.lsp_document_diagnostics },
      -- { "<leader>fi", builtin.lsp_implementations },
      -- { "<leader>fv", builtin.lsp_references },
      -- { "<leader>fx", builtin.lsp_code_actions },
      -- { "<leader>fm", builtin.lsp_document_symbols },
    }
  end,
  config = function()
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

    local dropdown = {
      layout_strategy = 'vertical',
      layout_config = { width = 0.6, preview_cutoff = 1, prompt_position = 'top' },
    }
    require('telescope').setup({
      defaults = {
        -- path_display = { "truncate" },
        sorting_strategy = 'ascending',
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
          },
          n = { q = actions.close, ['<A-p>'] = action_layout.toggle_preview },
        },
      },

      pickers = {
        buffers = { ignore_current_buffer = true, sort_lastused = true },
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
    -- require("telescope").load_extension("fzf")
    require('telescope').load_extension 'ui-select'
  end,
}
