local opt = vim.opt

opt.number = true
opt.relativenumber = true
opt.path:append '**'
opt.iskeyword:append '-' -- for css make 'is-active' as one word
opt.cursorline = true    -- Enable highlighting of the current line
opt.expandtab = true     -- Use spaces instead of tabs
opt.autowrite = true     -- Enable auto write
opt.completeopt = 'menu,menuone,noselect'
opt.conceallevel = 2     -- Hide * markup for bold and italic, but not markers with substitutions
opt.fillchars = { foldopen = '', foldclose = '', fold = ' ', foldsep = ' ', diff = '╱', eob = ' ', }
opt.ignorecase = true -- Ignore case
opt.smartcase = true  -- Ignore case
opt.foldlevel = 99
opt.jumpoptions = 'view'
opt.inccommand = 'nosplit'     -- preview incremental substitute
opt.formatoptions = 'jcroqlnt' -- tcqj
opt.laststatus = 3             -- global statusline
opt.linebreak = true           -- Wrap lines at convenient points
opt.list = true                -- Show some invisible characters (tabs...
opt.mouse = 'a'                -- Enable mouse mode
opt.pumblend = 10              -- Popup blend
opt.pumheight = 10             -- Maximum number of entries in a popup
opt.scrolloff = 8              -- Lines of context
opt.shiftround = true          -- Round indent
opt.tabstop = 2
opt.shiftwidth = 0             -- will use the value from `tabstop`
opt.signcolumn = 'yes'         -- Always show the signcolumn, otherwise it would shift the text each time
opt.showmode = false           -- Dont show mode since we have a statusline
opt.smartindent = true         -- Insert indents automatically
opt.splitbelow = true          -- Put new windows below current
opt.splitright = true          -- Put new windows right of current
opt.splitkeep = 'screen'
opt.termguicolors = true       -- True color support
opt.undofile = true
opt.undolevels = 10000
opt.updatetime = 200                          -- Save swap file and trigger CursorHold
opt.timeoutlen = vim.g.vscode and 1000 or 500 -- Lower than default (1000) to quickly trigger which-key
opt.backupdir = { vim.fn.stdpath 'state' .. '/backup//' }
opt.virtualedit = 'block'                     -- Allow cursor to move where there is no text in visual block mode
opt.exrc = true                               -- allows to create project specific settings

opt.wildmode = 'longest:full,full'
opt.wildignorecase = true
opt.wildignore:append({
  '**/node_modules/*',
  '**/.git/*',
  '**/dist/*',
  '**/build/*',
  '**/static/*',
  '**/.next/*',
  '*.o',
  '*.out',
  '*.obj',
  '*.exe',
  '*.dll',
  '*.jar',
  '*.pyc',
  '*.rbc',
  '*.class',
  '*.gif',
  '*.ico',
  '*.jpg',
  '*.jpeg',
  '*.png',
  '*.mov',
  '*.mht',
  '*.swp',
  '*.zip',
  '*.tar.gz',
  '*.tar.bz2',
})

opt.sessionoptions:append({ 'globals', })
opt.sessionoptions:remove 'folds'

vim.schedule(function()
  -- opt.shell = '/opt/homebrew/bin/fish'
  opt.clipboard = vim.env.SSH_TTY and '' or 'unnamedplus' -- Sync with system clipboard
  opt.shortmess:append({ W = true, I = true, c = true, C = true })
  opt.diffopt:append 'linematch:60'
  if vim.fn.has 'nvim-0.10' == 1 then
    opt.smoothscroll = true
  end
end)

if vim.fn.executable 'rg' == 1 then
  opt.grepformat = '%f:%l:%c:%m'
  opt.grepprg = 'rg --vimgrep --smart-case'
end

vim.g['markdown_recommended_style'] = 0
