local function fzf_create_file()
  local fzf = require 'fzf-lua'
  local path = require 'fzf-lua.path'
  local uv = vim.uv or vim.loop
  local cmd = 'fd -t d . ' .. uv.cwd()
  local function get_full_path(selected)
    if #selected < 1 then
      return
    end
    local entry = path.entry_to_file(selected[1], { cwd = uv.cwd() })
    if entry.path == '<none>' then
      return
    end

    local fullpath = entry.path or entry.uri and entry.uri:match '^%a+://(.*)'
    if not path.is_absolute(fullpath) then
      fullpath = path.join({ uv.cwd(), fullpath })
    end
    return fullpath
  end
  fzf.fzf_exec(cmd, {
    defaults = {},
    prompt = 'Create> ',
    cwd = uv.cwd(),
    cwd_prompt_shorten_len = 32,
    cwd_prompt_shorten_val = 1,
    fzf_opts = {
      ['--tiebreak'] = 'end',
      ['--preview-window'] = 'nohidden,50%',
      ['--preview'] = {
        type = 'cmd',
        fn = function(selected)
          local fullpath = get_full_path(selected)
          local ls_cmd = 'command ls --color -hsv1F --group-directories-first'
          local eza_cmd =
            'eza -al --color=always --icons=always --group-directories-first --no-user --no-permissions --no-time'
          return string.format('%s %s', vim.fn.executable 'eza' == 1 and eza_cmd or ls_cmd, fullpath)
        end,
      },
    },
    fn_transform = function(x)
      return fzf.make_entry.file(x, { file_icons = true, color_icons = true, cwd = uv.cwd() })
    end,
    actions = {
      ['default'] = function(selected)
        local fullpath = get_full_path(selected)
        vim.ui.input({ prompt = 'File Name: ' }, function(name)
          if name == nil then
            return
          end
          vim.cmd('e ' .. fullpath .. name)
          vim.cmd 'w ++p'
        end)
      end,
    },
  })
end

return {
  'ibhagwan/fzf-lua',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  cmd = 'FzfLua',
  enabled = false,
  keys = function()
    local fzf = require 'fzf-lua'
    return {
      { '<C-p>', fzf.files, desc = '[F]iles' },
      { '<leader>ff', fzf.files, desc = '[F]iles' },
      { '<leader>fr', fzf.resume, desc = '[R]esume' },
      { '<leader>fs', fzf.live_grep_native, desc = '[S]earch(Project)' },
      { '<leader>fp', fzf_create_file, desc = 'Create File' },
      { '<leader>fc', fzf_create_file, desc = 'Create File' },
      {
        '<leader>fS',
        function()
          fzf.grep_curbuf({
            winopts = { preview = { layout = 'vertical', vertical = 'up:60%' } },
          })
        end,
        desc = '[S]earch Current Buffer',
      },
      { '<leader>fw', fzf.grep_cWORD, desc = '[W]ord under cursor' },
      { '<leader>fw', fzf.grep_visual, mode = 'v', desc = 'Selection' },
      {
        '<leader>fo',
        function()
          fzf.oldfiles({ path_shorten = true })
        end,
        desc = '[O]ld Files',
      },
      { '<leader>fb', fzf.buffers, desc = '[B]uffers' },
      { '<leader>fz', fzf.spell_suggest, desc = '[S]pelling' },
      { '<leader>fk', fzf.keymaps, desc = '[K]eymaps' },
      { '<leader>fh', fzf.help_tags, desc = '[H]elp Tags' },
      { '<leader>fD', fzf.diagnostics, desc = '[D]iagnostics' },
      {
        '<leader>fd',
        function()
          fzf.files({ cwd = vim.fn.expand '~/Documents/dotfiles' })
        end,
        desc = '[D]otfiles',
      },
      {
        '<leader>fn',
        function()
          fzf.files({ cwd = vim.fn.stdpath 'config' })
        end,
        desc = '[N]eovim Config',
      },
      { '<leader>fg', fzf.git_status, desc = '[S]tatus' },
      { '<leader>gt', fzf.git_status, desc = '[S]tatus' },
      { '<leader>gS', fzf.git_stash, desc = 'Stash' },
      { '<leader>gb', fzf.git_branches, desc = '[B]ranches' },
      { '<leader>gc', fzf.git_commits, desc = '[C]ommits' },
      { '<leader>gC', fzf.git_bcommits, desc = '[B]ranch Commits' },
    }
  end,
  config = function()
    require 'plugins.config.fzf'
  end,
}
