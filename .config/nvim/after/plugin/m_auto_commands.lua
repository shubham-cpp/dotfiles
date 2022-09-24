local map = function(options)
  options.opts = { buffer = true, silent = true, noremap = true }
  options.mode = options.mode or 'n'
  vim.keymap.set(options.mode, options.lhs, options.rhs, options.opts)
end
--- Highlight_Yank {{{
local highlight_yank = vim.api.nvim_create_augroup('Highlight_Yank', { clear = true })

local highlight_yank_commands = {
  TextYankPost = {
    callback = function()
      vim.highlight.on_yank()
    end,
    desc = 'Show highlight when copying,deleting',
  },
  BufReadPost = {
    pattern = '*',
    command = [[ if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif ]],
    desc = 'Continue where left off',
  },
  BufWritePre = {
    command = [[ %s/\n\+\%$//e ]],
    desc = 'Cleanup empty new lines',
  },
  TermOpen = {
    command = 'startinsert',
    pattern = 'term://*',
    desc = 'Start with insert mode in terminal',
  },
}

for event, opts in pairs(highlight_yank_commands) do
  if opts.callback == nil and opts.command == nil then
    print("You didn't specify and command or callback for autocommand")
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
        vim.opt.foldmethod = 'marker'
        map({ lhs = '<F5>', rhs = '<cmd>so %<cr>', opts = { noremap = true, buffer = true } })
      end,
      pattern = { 'vim', 'lua' },
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
  {
    event = { 'BufEnter', 'FocusGained' },
    opts = { command = 'checktime', desc = 'Check for changes to current file from other programs' },
  },
  {
    event = 'BufEnter',
    opts = {
      command = 'setlocal ft=fish',
      pattern = '*.fish',
      desc = 'Properly set filetype to fish',
    },
  },
  {
    event = 'BufEnter',
    opts = {
      command = 'setlocal ft=vim',
      pattern = 'vifmrc',
      desc = 'Properly set filetype to vim',
    },
  },
  {
    event = 'BufWritePost',
    opts = {
      command = "call system('pkill -USR1 -x sxhkd && notify-send -t 1000 BSPWM SXHKD Restarted')",
      pattern = 'sxhkdrc',
      desc = 'Restart sxhkd after saving sxhkdrc',
    },
  },
  {
    event = 'BufEnter',
    opts = {
      command = 'setlocal ft=sh',
      pattern = '*profile',
      desc = 'Properly set filetype to bash/sh for zprofile,profile,xprofile,etc',
    },
  },
  {
    event = 'BufEnter',
    opts = {
      command = 'setlocal ft=sxhkdrc',
      pattern = { 'dwm_sxhkdrc', 'sxhkdrc' },
      desc = 'Properly set filetype dwm_sxhkdrc',
    },
  },

  {
    event = 'BufWritePre',
    opts = {
      command = [[ %s/\s\+$//e ]],
      desc = 'Clean whitespaces',
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
    event = 'BufWritePost',
    opts = {
      command = 'source <afile> | PackerCompile',
      desc = 'Auto Compile plugins file',
      pattern = { 'packer-plugins.lua', 'plugins.lua' },
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
}

for _, cmds in ipairs(file_reloads_commands) do
  if cmds.opts.callback == nil and cmds.opts.command == nil then
    print("You didn't specify and command or callback for autocommand")
    return 1
  end
  cmds.opts.pattern = cmds.opts.pattern or '*'
  cmds.opts.group = File_Reloads
  vim.api.nvim_create_autocmd(cmds.event, cmds.opts)
end

--- }}}
--- LspFormattings {{{
local LspFormattings = vim.api.nvim_create_augroup('LspFormattings', { clear = true })

vim.api.nvim_create_autocmd('BufWritePre', {
  command = 'lua vim.lsp.buf.format({async=false, tabSize = 2, trimFinalNewlines = true, trimTrailingWhitespace = true })',
  pattern = {
    '*.js',
    '*.ts',
    '*.tsx',
    '*.jsx',
    '*.vue',
    '*.html',
    '*.css',
    '*.scss',
    '*.json',
    '*.yaml',
    '*.lua',
    '*.go',
    '*.py',
    '*.hs',
    '*.md',
  },
  desc = 'Auto format buffer before saving',
  group = LspFormattings,
})
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
      command = [[ setlocal ft=lisp | setlocal commentstring=;;\ %s ]],
      pattern = { '*.kbd' },
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
      command = 'set formatoptions-=c formatoptions-=r formatoptions-=o',
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
    print("You didn't specify and command or callback for autocommand")
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
    opts = {
      command = 'set nosmartcase',
    },
  },
  {
    event = 'CmdLineLeave',
    opts = {
      command = 'set smartcase',
    },
  },
}

for _, cmds in ipairs(dynamic_smartcase_commands) do
  if cmds.opts.callback == nil and cmds.opts.command == nil then
    print("You didn't specify and command or callback for autocommand")
    return 1
  end
  cmds.opts.pattern = cmds.opts.pattern or '*'
  cmds.opts.group = Dynamic_Smartcase
  vim.api.nvim_create_autocmd(cmds.event, cmds.opts)
end

--- }}}
--- KeyBindings {{{
local KeyBindings = vim.api.nvim_create_augroup('KeyBindings', { clear = true })

local key_bindings_commands = {
  {
    event = 'FileType',
    opts = {
      pattern = { 'markdown', 'gitcommit' },
      callback = function()
        vim.opt_local.spell = true
        map({ lhs = '<leader>mp', rhs = ':MarkdownPreview<cr>' })
        map({ lhs = '<leader>mtt', rhs = ':GenTocMarked<cr>' })
        map({ lhs = '<leader>mtg', rhs = ':GenTocGitLab<cr>' })
      end,
    },
  },
}

for _, cmds in ipairs(key_bindings_commands) do
  if cmds.opts.callback == nil and cmds.opts.command == nil then
    print("You didn't specify and command or callback for autocommand")
    return 1
  end
  cmds.opts.pattern = cmds.opts.pattern or '*'
  cmds.opts.group = KeyBindings
  vim.api.nvim_create_autocmd(cmds.event, cmds.opts)
end

--- }}}
--- File Specific Keybindings {{{
local ft_key_binds = vim.api.nvim_create_augroup('FT_Keys', { clear = true })

local ft_key_binds_commands = {
  {
    event = { 'FileType' },
    opts = {
      pattern = { 'markdown' },
      callback = function()
        map({ lhs = '<leader>mp', rhs = ':MarkdownPreview<cr>' })
        map({ lhs = '<leader>mtt', rhs = ':GenTocMarked<cr>' })
        map({ lhs = '<leader>mtg', rhs = ':GenTocGitLab<cr>' })
      end,
      desc = 'Keybindings for Markdown files',
    },
  },
}

for _, aucmd in ipairs(ft_key_binds_commands) do
  aucmd.opts.pattern = aucmd.opts.pattern or '*'
  aucmd.opts.group = ft_key_binds
  vim.api.nvim_create_autocmd(aucmd.event, aucmd.opts)
end
--- }}}

vim.cmd([[
" Some commands from vimfandom {{{
" Search for selected text, forwards or backwards.
vnoremap <silent> * :<C-U>
            \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
            \gvy/<C-R>=&ic?'\c':'\C'<CR><C-R><C-R>=substitute(
            \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
            \gVzv:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
            \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
            \gvy?<C-R>=&ic?'\c':'\C'<CR><C-R><C-R>=substitute(
            \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
            \gVzv:call setreg('"', old_reg, old_regtype)<CR>
"}}}
]])
