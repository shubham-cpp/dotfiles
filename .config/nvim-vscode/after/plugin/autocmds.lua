local highlight_yank = vim.api.nvim_create_augroup('Highlight_Yank', { clear = true })

local highlight_yank_commands = {
  {
    event = 'TextYankPost',
    opts = {
      callback = function()
        vim.highlight.on_yank()
      end,
      desc = 'Show highlight when copying,deleting',
    },
  },
  {
    event = 'FileType',
    opts = {
      callback = function()
        vim.opt_local.shiftwidth = 2
        vim.opt_local.tabstop = 2
        vim.opt_local.softtabstop = 2
      end,
      pattern = {
        'css',
        'javascript',
        'javascriptreact',
        'typescript',
        'typescriptreact',
        'html',
        'json',
        'vue',
        'svelte',
      },
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
      -- command = 'set formatoptions-=c formatoptions-=r formatoptions-=o'
      -- this should not be necessary because
      callback = function()
        vim.opt_local.formatoptions = vim.opt_local.formatoptions
          - 'a' -- Auto formatting is BAD.
          - 't' -- Don't auto format my code. I got linters for that.
          - '2' -- I'm not in gradeschool anymore
          - 'l' -- Yes break the lines
          - 'o' -- O and o, don't continue comments
          + 'c' -- In general, I like it when comments respect textwidth
          + 'q' -- Allow formatting comments w/ gq
          + 'r' -- But do continue when pressing enter.
          + 'n' -- Indent past the formatlistpat, not underneath it.
          + 'j' -- Auto-remove comments if possible.
      end,
      pattern = '*',
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
  -- {
  -- 	event = 'FileType',
  -- 	opts = {
  -- 		callback = function()
  -- 			vim.opt_local.formatprg =
  -- 				'stylua --call-parentheses NoSingleString --indent-width 2 --quote-style AutoPreferSingle -'
  -- 		end,
  -- 		pattern = { 'lua' },
  -- 	},
  -- },
}

for _, cmds in ipairs(highlight_yank_commands) do
  if cmds.opts.callback == nil and cmds.opts.command == nil then
    print "You didn't specify and command or callback for autocommand"
    return 1
  end
  cmds.opts.pattern = cmds.opts.pattern or '*'
  cmds.opts.group = highlight_yank
  vim.api.nvim_create_autocmd(cmds.event, cmds.opts)
end
