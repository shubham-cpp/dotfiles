local opt = vim.opt

opt.number = true
opt.relativenumber = true
opt.ignorecase = true
opt.smartcase = true
opt.conceallevel = 2 -- Hide * markup for bold and italic, but not markers with substitutions
opt.cursorline = true -- Enable highlighting of the current line
opt.tabstop = 2
opt.shiftwidth = 0 -- will use the value from `tabstop`
opt.expandtab = true -- Use spaces instead of tabs
opt.autowrite = true -- Enable auto write
opt.termguicolors = true -- True color support
opt.scrolloff = 8 -- Lines of context
opt.undofile = true
opt.undolevels = 10000
opt.updatetime = 200 -- Save swap file and trigger CursorHold
opt.timeoutlen = vim.g.vscode and 1000 or 250 -- Lower than default (1000) to quickly trigger which-key
opt.backupdir = { vim.fn.stdpath "state" .. "/backup//" }
opt.virtualedit = "block" -- Allow cursor to move where there is no text in visual block mode
opt.exrc = true -- allows to create project specific settings
opt.showmode = false -- Dont show mode since we have a statusline
opt.smartindent = true -- Insert indents automatically
opt.splitbelow = true -- Put new windows below current
opt.splitright = true -- Put new windows right of current
opt.splitkeep = "screen"
opt.signcolumn = "yes" -- Always show the signcolumn, otherwise it would shift the text each time
opt.mouse = "a" -- Enable mouse mode
opt.wrap = true
opt.showbreak = ""
opt.fillchars = { foldopen = "", foldclose = "", fold = " ", foldsep = " ", diff = "╱", eob = " " }

opt.wildmode = "longest:full,full"
opt.wildignorecase = true
opt.wildignore:append({
  "**/node_modules/*",
  "**/.git/*",
  "**/dist/*",
  "**/build/*",
  "**/static/*",
  "**/.next/*",
  "*.o",
  "*.out",
  "*.obj",
  "*.exe",
  "*.dll",
  "*.jar",
  "*.pyc",
  "*.rbc",
  "*.class",
  "*.gif",
  "*.ico",
  "*.jpg",
  "*.jpeg",
  "*.png",
  "*.mov",
  "*.mht",
  "*.swp",
  "*.zip",
  "*.tar.gz",
  "*.tar.bz2",
})

opt.sessionoptions:append({ "globals" })
opt.sessionoptions:remove "folds"
opt.path:append "**"
opt.iskeyword:append "-" -- for css make 'is-active' as one word

opt.laststatus = 3 -- global statusline

vim.schedule(function()
  -- opt.shell = '/opt/homebrew/bin/fish'
  opt.clipboard = vim.env.SSH_TTY and "" or "unnamedplus" -- Sync with system clipboard
  opt.shortmess:append({ W = true, I = true, c = true, C = true })
  opt.diffopt:append "linematch:60"
  if vim.fn.has "nvim-0.10" == 1 then opt.smoothscroll = true end
end)

if vim.fn.executable "rg" == 1 then
  opt.grepformat = "%f:%l:%c:%m"
  opt.grepprg = "rg --vimgrep --smart-case"
end

vim.g["markdown_recommended_style"] = 0

vim.api.nvim_create_user_command("PrintConfig", function(opts)
  local plugins = vim.tbl_keys(require("lazy.core.config").plugins)
  local args = opts.args
  local function callback(plugin_name)
    local cmd = "Redir lua =require('lazy.core.config').plugins['" .. plugin_name .. "']"
    vim.notify(cmd, vim.log.levels.INFO, { title = "Command" })
    vim.fn.execute(cmd)
  end
  if args ~= "" then
    callback(args)
    return
  end

  vim.ui.select(plugins, { prompt = "Select Config to print" }, function(item)
    if not item then return end
    callback(item)
  end)
end, {
  desc = "Print final lazy config",
  nargs = "?",
  complete = function(prefix)
    local plugins = vim.tbl_keys(require("lazy.core.config").plugins)
    return vim
      .iter(plugins)
      :filter(function(t)
        if string.len(prefix:gsub("%s+", "")) > 0 then return t:match(prefix) end
        return true
      end)
      :totable()
  end,
})
