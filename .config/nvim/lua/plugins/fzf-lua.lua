return {
  'ibhagwan/fzf-lua',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  cmd = 'FzfLua',
  enabled = true,
  config = function()
    require 'plugins.config.fzf'
  end,
  keys = {
    '<C-p>',
    '<leader>ff',
    '<leader>fr',
    '<leader>fs',
    '<leader>fc',
    '<leader>fS',

    '<leader>fw',
    '<leader>fo',
    '<leader>fb',
    '<leader>fz',
    '<leader>fk',
    '<leader>fh',
    '<leader>fD',
    '<leader>fd',

    '<leader>fn',

    '<leader>fg',
    '<leader>gt',
    '<leader>gS',
    '<leader>gb',
    '<leader>gc',
    '<leader>gC',
  },
  -- keys = vim.tbl_keys(require('plugins.config.fzf').keys),
  -- keys = function()
  --   local fzf = require 'fzf-lua'
  --   return {
  --     { '<C-p>', project_files, desc = 'Git [F]iles' },
  --     { '<leader>ff', fzf.files, desc = '[F]iles' },
  --     { '<leader>fr', fzf.resume, desc = '[R]esume' },
  --     { '<leader>fs', fzf.live_grep_native, desc = '[S]earch(Project)' },
  --     { '<leader>fc', fzf_create_file, desc = 'Create File' },
  --     {
  --       '<leader>fS',
  --       function()
  --         fzf.grep_curbuf({ winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } } })
  --       end,
  --       desc = '[S]earch Current Buffer',
  --     },
  --     { '<leader>fw', fzf.grep_cWORD, desc = '[W]ord under cursor' },
  --     { '<leader>fw', fzf.grep_visual, mode = 'v', desc = 'Selection' },
  --     {
  --       '<leader>fo',
  --       function()
  --         fzf.oldfiles({ path_shorten = true })
  --       end,
  --       desc = '[O]ld Files',
  --     },
  --     { '<leader>fb', fzf.buffers, desc = '[B]uffers' },
  --     { '<leader>fz', fzf.spell_suggest, desc = '[S]pelling' },
  --     { '<leader>fk', fzf.keymaps, desc = '[K]eymaps' },
  --     { '<leader>fh', fzf.help_tags, desc = '[H]elp Tags' },
  --     { '<leader>fD', fzf.diagnostics, desc = '[D]iagnostics' },
  --     {
  --       '<leader>fd',
  --       function()
  --         fzf.files({ cwd = vim.fn.expand '~/Documents/dotfiles' })
  --       end,
  --       desc = '[D]otfiles',
  --     },
  --     {
  --       '<leader>fn',
  --       function()
  --         fzf.files({ cwd = vim.fn.stdpath 'config' })
  --       end,
  --       desc = '[N]eovim Config',
  --     },
  --     { '<leader>fg', fzf.git_status, desc = '[S]tatus' },
  --     { '<leader>gt', fzf.git_status, desc = '[S]tatus' },
  --     { '<leader>gS', fzf.git_stash, desc = 'Stash' },
  --     { '<leader>gb', fzf.git_branches, desc = '[B]ranches' },
  --     { '<leader>gc', fzf.git_commits, desc = '[C]ommits' },
  --     { '<leader>gC', fzf.git_bcommits, desc = '[B]ranch Commits' },
  --   }
  -- end,
}
