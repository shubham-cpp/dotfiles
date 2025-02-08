---@param picker snacks.Picker
local function copy_path_full(picker)
  local selected = picker:selected({ fallback = true })[1]
  if not selected or selected == nil then
    return
  end
  vim.schedule(function()
    local full_path = vim.fn.fnamemodify(selected.file, ':p')
    vim.fn.setreg('+', full_path)
    vim.notify(full_path, vim.log.levels.INFO, { title = 'File Path Copied' })
  end)
end
---FIXME: not working currently
---@param picker snacks.Picker
local function copy_path_relative(picker)
  local selected = picker:selected({ fallback = true })[1]
  if not selected or selected == nil then
    return
  end
  vim.schedule(function()
    local full_path = vim.fs.normalize(selected.file)
    vim.fn.setreg('+', full_path)
    vim.notify(full_path, vim.log.levels.INFO, { title = 'File Path Copied' })
  end)
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
            layout = { layout = { position = 'right' }, cycle = false },
            actions = {
              copy_path_full = copy_path_full,
              copy_path_relative = copy_path_relative,
            },
            win = {
              list = {
                keys = {
                  ['Y'] = { 'copy_path_full', mode = { 'n' } },
                  ['gy'] = { 'copy_path_relative', mode = { 'n' } },
                  -- ['g.'] = { 'toggle_hidden', mode = { 'n' } },
                  -- ['gI'] = { 'toggle_hidden', mode = { 'n' } },
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
      indent = {
        enabled = true,
        char = '│',
        only_current = true,
      },
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
        '<C-/>',
        function()
          Snacks.terminal({ 'yazi' }, { cwd = vim.uv.cwd() })
        end,
        desc = 'Toggle Yazi',
        mode = { 'n', 't' },
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
          Snacks.picker.files({ layout = { preset = 'vscode' } })
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
          Snacks.picker.git_files({ layout = { preset = 'vscode' } })
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
        '<C-y>',
        function()
          Snacks.explorer()
        end,
        desc = 'explorer',
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
