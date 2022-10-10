local M = {}
local map = require('sp.helper').map
local telescope = require 'telescope'
local previewers = require 'telescope.previewers'
local themes = require 'telescope.themes'
local builtin = require 'telescope.builtin'
local actions = require 'telescope.actions'
local action_layout = require 'telescope.actions.layout'

local center_list = themes.get_dropdown({
  winblend = 10,
  layout_config = { width = 0.5, height = 0.8 },
  previewer = false,
})

function M.fd_nvim()
  local opts = themes.get_dropdown({})
  opts.previewer = false
  opts.prompt_prefix = '  '
  opts.cwd = vim.fn.stdpath 'config'
  builtin.fd(opts)
end

function M.fd_dotfiles()
  local opts = themes.get_dropdown({})
  local dotfiles = io.popen "readlink -f ~/.config/awesome/ | cut -d'.' -f1"
  opts.previewer = false
  opts.hidden = true
  opts.prompt_prefix = '  '
  if dotfiles then
    opts.cwd = dotfiles:read()
    dotfiles:close()
  else
    opts.cwd = os.getenv 'HOME' .. '/Documents/dotfiles'
  end
  builtin.find_files(opts)
end

--- Custom find file picked up from telescope docs
---@param hidden boolean Determine whether to show hidden files or not
function M.find_files(hidden)
  local dropdown_theme = themes.get_dropdown({
    borderchars = {
      { '─', '│', '─', '│', '┌', '┐', '┘', '└' },
      prompt = { '─', '│', ' ', '│', '┌', '┐', '│', '│' },
      results = { '─', '│', '─', '│', '├', '┤', '┘', '└' },
      preview = { '─', '│', '─', '│', '┌', '┐', '┘', '└' },
    },
    width = 0.8,
    previewer = false,
    prompt_title = false,
  })
  dropdown_theme.hidden = hidden
  builtin.fd(dropdown_theme)
end

function M.grep_current()
  local opts = vim.deepcopy(center_list)
  opts.prompt_prefix = 'Goto 🔍'
  builtin.current_buffer_fuzzy_find(opts)
end

telescope.setup({
  defaults = {
    vimgrep_arguments = {
      'rg',
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case',
      '--trim',
    },
    mappings = {
      i = {
        ['<esc>'] = actions.close,
        ['<A-CR>'] = actions.select_tab,
        ['<M-p>'] = action_layout.toggle_preview,
      },
      n = { ['<A-CR>'] = actions.select_tab, ['<M-p>'] = action_layout.toggle_preview },
    },
    prompt_prefix = '🔍',
    initial_mode = 'insert',
    file_previewer = function(...)
      return previewers.cat.new(...)
    end,
    grep_previewer = function(...)
      return previewers.vimgrep.new(...)
    end,
    file_ignore_patterns = {
      '.backup',
      '.swap',
      '.langservers',
      '.undo',
      -- '.git',
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
})
telescope.load_extension 'zf-native'
vim.cmd [[ command! -bang Nvim :lua require'sp.telescope'.fd_nvim()<CR> ]]
vim.cmd [[ command! -bang DotFiles :lua require'sp.telescope'.fd_dotfiles()<CR> ]]
vim.cmd [[ command! -bang MGrep :lua require'sp.telescope'.grep_current()<CR> ]]

local ok, _ = pcall(require, 'fzf-lua')
if not ok then
  map('n', '<C-p>', M.find_files)
  map('n', ',c', M.fd_nvim)
  map('n', ',d', M.fd_dotfiles)
  map('n', '\\\\', builtin.live_grep)
  map('n', '<leader>gla', builtin.git_commits)
  map('n', '<leader>glc', builtin.git_bcommits)
  map('n', '<leader>gs', builtin.git_status)
  map('n', '<leader>gb', builtin.git_branches)
end

map('n', '<leader>ff', function()
  M.find_files(true)
end)
map('n', '<leader>fn', M.fd_nvim)
map('n', '<leader>fd', M.fd_dotfiles)
map('n', '<leader>fs', M.grep_current)
map('n', '<leader>fS', builtin.live_grep)
map('n', '<leader>fh', builtin.help_tags)
map('n', '<leader>fH', builtin.oldfiles)
map('n', '<leader>fc', builtin.colorscheme)
map('n', '<leader>fz', builtin.spell_suggest)
map('n', '<leader>fg', builtin.git_status)
map('n', '<leader>fb', builtin.buffers)

return M
