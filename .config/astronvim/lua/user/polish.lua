local map = function(options)
  options.opts = { buffer = true, silent = true, noremap = true }
  options.mode = options.mode or 'n'
  vim.keymap.set(options.mode, options.lhs, options.rhs, options.opts)
end

vim.cmd [[ autocmd TermEnter term://*toggleterm#* tnoremap <silent><c-\> <Cmd>exe v:count1 . "ToggleTerm"<CR> ]]

--- Highlight_Yank {{{
local highlight_yank = vim.api.nvim_create_augroup('Highlight_Yank', { clear = true })

local highlight_yank_commands = {
  TextYankPost = {
    callback = function()
      vim.highlight.on_yank({ timeout = 100 })
    end,
    desc = 'Show highlight when copying,deleting',
  },
  TermOpen = {
    command = 'startinsert',
    pattern = 'term://*',
    desc = 'Start with insert mode in terminal',
  },
}

for event, opts in pairs(highlight_yank_commands) do
  if opts.callback == nil and opts.command == nil then
    print "You didn't specify and command or callback for autocommand"
    return 1
  end
  opts.group = highlight_yank
  opts.pattern = opts.pattern or '*'
  vim.api.nvim_create_autocmd(event, opts)
end

--- }}}
--- File_Reloads {{{
local File_Reloads = vim.api.nvim_create_augroup('File_Reloads', { clear = true })

local file_reloads_commands = {
  {
    event = 'FileType',
    opts = {
      callback = function()
        vim.opt_local.foldmethod = 'marker'
        map({ lhs = '<F5>', rhs = '<cmd>so %<cr>', opts = { noremap = true, buffer = true } })
      end,
      pattern = { 'vim' },
      desc = 'Setup foldmethod and use F5 to source current buffer',
    },
  },
  {
    event = 'BufWritePost',
    opts = {
      command = '!xrdb %',
      pattern = { 'Xresources', 'Xdefaults' },
      desc = 'Run xrdb after writing to Xresources',
    },
  },
  -- {
  --   event = 'BufReadPost',
  --   opts = {
  --     callback = function()
  --       vim.opt_local.foldmethod = 'expr'
  --       vim.opt_local.foldexpr = 'nvim_treesitter#foldexpr()'
  --     end,
  --     pattern = {
  --       '*.js',
  --       '*.jsx',
  --       '*.ts',
  --       '*.tsx',
  --       '*.vue',
  --       '*.html',
  --       '*.css',
  --       '*.scss',
  --       '*.c',
  --       '*.cpp',
  --       '*.rs',
  --       '*.go',
  --     },
  --     desc = 'Use nvim-treesitter folding',
  --   },
  -- },
  -- {
  --   event = { 'BufEnter', 'FocusGained' },
  --   opts = { command = 'checktime', desc = 'Check for changes to current file from other programs' },
  -- },
  -- {
  --   event = 'BufEnter',
  --   opts = {
  --     command = 'setlocal ft=fish',
  --     pattern = '*.fish',
  --     desc = 'Properly set filetype to fish',
  --   },
  -- },
  {
    event = 'BufWritePost',
    opts = {
      command = "call system('pkill -USR1 -x sxhkd && notify-send -t 1000 BSPWM SXHKD Restarted')",
      pattern = 'sxhkdrc',
      desc = 'Restart sxhkd after saving sxhkdrc',
    },
  },
  {
    event = 'FileType',
    opts = {
      command = 'setlocal sw=2 ts=2',
      pattern = { 'css', 'javascript', 'javascriptreact', 'typescript', 'typescriptreact', 'html', 'json' },
      desc = 'Clean whitespaces',
    },
  },
  {
    event = 'FileType',
    opts = {
      command = 'nnoremap <buffer> q :q<cr>',
      pattern = { 'help', 'man' },
      desc = 'Use q to close help/man window',
    },
  },
  {
    event = 'BufWritePre',
    opts = {
      command = [[ let &bex = '@' . strftime("%F.%H:%M") ]],
      desc = 'Meaningful backup name, ex: filename@2015-04-05.14:59',
    },
  },
  {
    event = 'TermOpen',
    opts = {
      callback = function()
        map({ mode = 't', lhs = '<C-]>', rhs = '<C-\\><C-n>' })
        map({ lhs = 'A', rhs = 'A<C-k>' })
        map({ lhs = 'D', rhs = 'A<C-k><C-\\><C-n>' })
        map({ lhs = 'cc', rhs = 'A<C-e><C-u>' })
        map({ lhs = 'cc', rhs = 'A<C-e><C-u>' })
        map({ lhs = 'dd', rhs = 'A<C-e><C-u><C-><C-n>' })
        vim.opt_local.signcolumn = 'no'
        vim.opt_local.relativenumber = false
        vim.opt_local.number = false
      end,
    },
  },
  {
    event = 'FileType',
    opts = {
      pattern = { 'markdown' },
      callback = function()
        map({ lhs = 'M-p', rhs = '<cmd>MarkdownPreview<CR>' })
        map({ lhs = '<leader>mp', rhs = '<Plug>MarkdownPreview' })
        map({ lhs = '<leader>ms', rhs = '<Plug>MarkdownStop' })
        map({ lhs = '<leader>mt', rhs = '<Plug>MarkdownToggle' })
      end,
    },
  },
}

for _, cmds in ipairs(file_reloads_commands) do
  if cmds.opts.callback == nil and cmds.opts.command == nil then
    print "You didn't specify and command or callback for autocommand"
    return 1
  end
  cmds.opts.pattern = cmds.opts.pattern or '*'
  cmds.opts.group = File_Reloads
  vim.api.nvim_create_autocmd(cmds.event, cmds.opts)
end

--- }}}
--- Comments {{{
local Comments = vim.api.nvim_create_augroup('Comments', { clear = true })

local comments_commands = {
  {
    event = 'FileType',
    opts = {
      command = [[ setlocal commentstring=##\ %s ]],
      pattern = { 'apache', 'sxhkdrc', 'toml', 'fish', 'template', 'mbsyncrc' },
    },
  },
  {
    event = 'FileType',
    opts = {
      command = [[ setlocal commentstring=;;\ %s ]],
      pattern = { 'lisp' },
    },
  },
  {
    event = 'FileType',
    opts = {
      command = [[ setlocal commentstring=<!--%s--> ]],
      pattern = { 'htmldjango' },
    },
  },
  {
    event = 'FileType',
    opts = {
      command = [[ setlocal commentstring=!\ %s ]],
      pattern = { 'xdefaults' },
    },
  },
  {
    event = 'BufEnter',
    opts = {
      callback = function()
        vim.opt.commentstring = '" %s'
      end,
      pattern = { '*.vifm' },
    },
  },
  {
    event = 'FileType',
    opts = {
      callback = function()
        vim.opt_local.formatoptions = vim.opt_local.formatoptions
            - 'a' -- Auto formatting is BAD.
            - 't' -- Don't auto format my code. I got linters for that.
            + 'c' -- In general, I like it when comments respect textwidth
            + 'q' -- Allow formatting comments w/ gq
            - 'o' -- O and o, don't continue comments
            + 'r' -- But do continue when pressing enter.
            + 'n' -- Indent past the formatlistpat, not underneath it.
            + 'j' -- Auto-remove comments if possible.
            - '2' -- I'm not in gradeschool anymore
      end,
      desc = 'Pressing O/o inside comment wont open another comment',
    },
  },
  {
    event = 'FileType',
    opts = {
      command = 'setlocal comments-=:// comments+=f://',
      pattern = { 'c', 'cpp' },
    },
  },
}

for _, cmds in ipairs(comments_commands) do
  if cmds.opts.callback == nil and cmds.opts.command == nil then
    print "You didn't specify and command or callback for autocommand"
    return 1
  end
  cmds.opts.pattern = cmds.opts.pattern or '*'
  cmds.opts.group = Comments
  vim.api.nvim_create_autocmd(cmds.event, cmds.opts)
end

--- }}}
--- Dynamic_Smartcase {{{
local Dynamic_Smartcase = vim.api.nvim_create_augroup('Dynamic_Smartcase', { clear = true })

local dynamic_smartcase_commands = {
  {
    event = 'CmdLineEnter',
    opts = { command = 'set nosmartcase' },
  },
  {
    event = 'CmdLineLeave',
    opts = { command = 'set smartcase' },
  },
}

for _, cmds in ipairs(dynamic_smartcase_commands) do
  if cmds.opts.callback == nil and cmds.opts.command == nil then
    print "You didn't specify and command or callback for autocommand"
    return 1
  end
  cmds.opts.pattern = cmds.opts.pattern or '*'
  cmds.opts.group = Dynamic_Smartcase
  vim.api.nvim_create_autocmd(cmds.event, cmds.opts)
end

--- }}}
--Set Appropriate FileTypes {{{
vim.filetype.add({
  extension = {
    fish = 'fish',
  },
  filename = {
    vimfrc = 'vim',
    dwm_sxhkdrc = 'sxhkdrc',
  },
  pattern = {
    ['*profile'] = 'sh',
    ['*.kbd'] = 'lisp',
  },
}) --- }}}
