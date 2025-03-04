---@param picker snacks.Picker
local function explorer_add_in_parent(picker)
  local Tree = require 'snacks.explorer.tree'
  local uv = vim.uv or vim.loop
  local default_dir = ''
  local parent = picker:current().parent
  if parent.dir == false then
    default_dir = picker:cwd()
  else
    default_dir = parent.file
  end
  Snacks.input({
    prompt = 'Add a new file or directory (directories end with a "/")',
    default = default_dir .. '/',
  }, function(value)
    if not value or value:find '^%s$' then
      return
    end
    local path = vim.fs.normalize(value)
    local is_file = value:sub(-1) ~= '/'
    local dir = is_file and vim.fs.dirname(path) or path
    if is_file and uv.fs_stat(path) then
      Snacks.notify.warn('File already exists:\n- `' .. path .. '`')
      return
    end
    vim.fn.mkdir(dir, 'p')
    if is_file then
      io.open(path, 'w'):close()
    end
    Tree:open(dir)
    Tree:refresh(dir)
    picker.update(picker, { target = path })
  end)
end

---@param picker snacks.Picker
local function copy_path_full(picker)
  local selected = picker:selected({ fallback = true })[1]
  if not selected or selected == nil then
    return
  end
  vim.schedule(function()
    local full_path = vim.fn.fnamemodify(selected.file, ':p')
    vim.fn.setreg(vim.v.register, full_path)
    vim.notify(full_path, vim.log.levels.INFO, { title = 'File Path Copied' })
  end)
end

---@param picker snacks.Picker
local function copy_path_relative(picker)
  ---@type string[]
  local paths = vim.tbl_map(Snacks.picker.util.path, picker:selected())
  if #paths == 0 then
    vim.notify(
      'No files selected to move. Renaming instead.',
      vim.log.levels.WARN,
      { title = 'Invalid use of `copy_path_relative` function' }
    )
    return
  end
  if #paths ~= 2 then
    vim.notify(
      'Exactly two files need to be selected.',
      vim.log.levels.WARN,
      { title = 'Invalid use of `copy_path_relative` function' }
    )
    return
  end

  local from_file = paths[2]
  local to_file = paths[1]
  local script = vim.fn.expand '~/.local/bin/myscripts/cal_relative_path.py'
  local cmd = { script, from_file, to_file }

  local on_done = function(obj)
    if obj.stderr ~= '' or obj.stdout == '' then
      Snacks.notify.warn('Some error while calculating relative paths ' .. obj.stderr)
      return
    end
    vim.fn.setreg(vim.v.register, obj.stdout)
    vim.notify(obj.stdout, vim.log.levels.INFO, { title = 'File Path Copied' })
  end

  local obj = vim.system(cmd, { text = true }):wait()
  on_done(obj)
end

---@type LazySpec
return {
  {
    'folke/snacks.nvim',
    lazy = false,
    ---@type snacks.Config
    opts = {
      bigfile = { enabled = true },
      input = { enabled = true },
      notifier = { enabled = true },
      picker = {
        enabled = true,
        ui_select = true,
        layout = { preset = 'dropdown' },
        matcher = { frecency = true },
        formatters = { file = { filename_first = true } },
        sources = {
          explorer = {
            layout = { cycle = false },
            actions = {
              explorer_add_in_parent = explorer_add_in_parent,
              copy_path_full = copy_path_full,
              copy_path_relative = copy_path_relative,
            },
            win = {
              list = {
                keys = {
                  ['/'] = false,
                  ['f'] = { 'toggle_focus', mode = { 'n' } },
                  ['A'] = { 'explorer_add_in_parent' },
                  ['Y'] = { 'copy_path_full' },
                  ['gy'] = { 'copy_path_relative' },
                  ['gf'] = { 'picker_files', desc = 'Open File Picker' },
                  ['<leader>f'] = { 'picker_files', desc = 'Open File Picker' },
                },
              },
            },
          },
        },
        -- jump = { reuse_win = true },
        win = {
          -- input window
          input = {
            keys = {
              ['<c-x>'] = { 'edit_split', mode = { 'i', 'n' } },
              ['<c-t>'] = { 'edit_tab', mode = { 'i', 'n' } },
              ['<c-c>'] = { 'copy', mode = { 'i', 'n' } },
            },
          },
          list = { keys = { ['<c-x>'] = 'edit_split' } },
        },
      },
      indent = { enabled = true, char = '│', only_current = true },
      scope = { enabled = true },
      explorer = { enabled = true },
      quickfile = { enabled = true },
      scroll = { enabled = true },
      statuscolumn = { enabled = true },
    },
    keys = {
      {
        '<leader>Z',
        function()
          Snacks.zen()
        end,
        desc = 'Toggle Zen Mode',
      },
      {
        '<leader>un',
        function()
          Snacks.notifier.show_history()
        end,
        desc = 'Notification History',
      },
      {
        '<C-w>m',
        function()
          Snacks.zen.zoom()
        end,
        desc = 'Toggle Zoom',
      },
      {
        '<leader>gg',
        function()
          Snacks.lazygit()
        end,
        desc = 'Lazygit',
      },
      {
        '<leader>gs',
        function()
          Snacks.picker.git_status()
        end,
        desc = 'Git Status',
      },
      {
        '<leader>gl',
        function()
          Snacks.git.blame_line()
        end,
        desc = 'Git Blame Line',
      },
      {
        '<leader>gb',
        function()
          Snacks.picker.git_branches()
        end,
        desc = 'Git Branches',
      },
      {
        '<leader>gc',
        function()
          Snacks.picker.git_log()
        end,
        desc = 'Git commits(project)',
      },
      {
        '<leader>gC',
        function()
          Snacks.picker.git_log_line()
        end,
        desc = 'Git commits(current file)',
      },
      {
        '<leader>bc',
        function()
          Snacks.bufdelete.other()
        end,
        desc = 'Close All(Except current buffer)',
      },
      {
        '<leader>bd',
        function()
          Snacks.bufdelete.delete()
        end,
        desc = 'Close current(Buffer)',
      },
      {
        '<leader>bC',
        function()
          Snacks.bufdelete.all()
        end,
        desc = 'Close All(Buffer)',
      },
      {
        '<c-\\>',
        function()
          Snacks.terminal(nil, { win = { style = 'float', border = 'rounded' } })
        end,
        desc = 'Toggle Terminal',
      },
      { '<leader>f', '', desc = '+pick' },
      {
        '<C-p>',
        function()
          local branch = vim.b.gitsigns_head or nil
          if branch ~= nil and branch ~= '' then
            Snacks.picker.git_files({ layout = { preset = 'vscode' }, untracked = true })
          else
            Snacks.picker.files({ layout = { preset = 'vscode' } })
          end
        end,
        desc = 'Find Files',
      },
      {
        '<leader>ff',
        function()
          Snacks.picker.files({ layout = { preset = 'vscode' } })
        end,
        desc = 'Find Files',
      },
      {
        '<leader>fn',
        function()
          Snacks.picker.files({ cwd = vim.fn.stdpath 'config', layout = { preset = 'vscode' } })
        end,
        desc = 'Find Config File',
      },
      {
        '<leader>fN',
        function()
          Snacks.picker.files({ cwd = vim.fn.stdpath 'data' .. '/lazy', layout = { preset = 'vscode' } })
        end,
        desc = 'Neovim Data dir',
      },
      {
        '<leader>fd',
        function()
          Snacks.picker.files({ cwd = vim.fn.expand '~/Documents/dotfiles/.config', layout = { preset = 'vscode' } })
        end,
        desc = 'Find Dotfiles',
      },
      {
        '<leader>fb',
        function()
          Snacks.picker.buffers()
        end,
        desc = 'Buffers',
      },
      {
        '<leader>fg',
        function()
          Snacks.picker.git_files({ layout = { preset = 'vscode' }, untracked = true })
        end,
        desc = 'Find Git Files',
      },
      {
        '<leader>/',
        function()
          Snacks.picker.lines()
        end,
        desc = 'Buffer Lines',
      },
      {
        '<leader>fs',
        function()
          Snacks.picker.grep()
        end,
        desc = 'Grep',
      },
      {
        '<leader>fS',
        function()
          Snacks.picker.grep_buffers()
        end,
        desc = 'Grep Open Buffers',
      },
      {
        '<leader>fw',
        function()
          Snacks.picker.grep_word()
        end,
        desc = 'Visual selection or word',
        mode = { 'n', 'x' },
      },
      {
        '<leader>fh',
        function()
          Snacks.picker.help()
        end,
        desc = 'Help Pages',
      },
      {
        '<leader>fk',
        function()
          Snacks.picker.keymaps()
        end,
        desc = 'Keymaps',
      },
      {
        '<leader>fq',
        function()
          Snacks.picker.qflist()
        end,
        desc = 'Quickfix List',
      },
      {
        '<leader>fm',
        function()
          Snacks.picker.man()
        end,
        desc = 'Man Pages',
      },
      {
        '<leader>fr',
        function()
          Snacks.picker.resume()
        end,
        desc = 'Resume',
      },
      {
        '<leader>fo',
        function()
          Snacks.picker.smart({ layout = { preset = 'vertical' } })
        end,
        desc = 'Find buffers/recent/files',
      },
      {
        '<leader>fO',
        function()
          Snacks.picker.recent({ layout = { preset = 'vertical' } })
        end,
        desc = 'Old Files(recent)',
      },
      {
        '<leader>fH',
        function()
          Snacks.picker.highlights()
        end,
        desc = 'Highlights',
      },
      {
        '<leader>fz',
        function()
          Snacks.picker.zoxide()
        end,
        desc = 'Zoxided',
      },
      {
        '<leader>ld',
        function()
          Snacks.picker.diagnostics()
        end,
        desc = 'Diagnostics',
      },
      {
        '<leader>-',
        function()
          Snacks.explorer()
        end,
        desc = '+explorer',
      },
    },
    init = function()
      vim.api.nvim_create_user_command('NotificationHistory', function()
        if not _G.Snacks then
          return
        end
        Snacks.notifier.show_history()
      end, { desc = 'Show Notification History' })
      vim.keymap.set('t', '<C-\\>', '<cmd>close<cr>', { desc = 'Hide Terminal' })
    end,
  },
}
