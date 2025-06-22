local g = vim.g
local o = vim.opt

o.number = true
o.relativenumber = true

o.mouse = "a"
o.signcolumn = "yes"
o.whichwrap:append "<>[]hl"
o.iskeyword:append "-"
o.path:append "**"
o.shortmess:append { W = true, I = true, c = true, C = true }

o.expandtab = true
o.tabstop = 2
o.shiftwidth = 0
o.softtabstop = -1
o.smartindent = true
o.smarttab = true

o.autowrite = true
o.wrap = true
o.showbreak = '󰄾'
-- o.listchars:append { precedes = "󰄾" }
o.cursorline = true
o.conceallevel = 2

o.ignorecase = true
o.smartcase = true

o.splitright = true
o.splitbelow = true

o.showmode = false
o.scrolloff = 8

o.timeoutlen = 500
o.updatetime = 300

o.undofile = true
o.undolevels = 10000
o.laststatus = 3
o.virtualedit = "block"

o.clipboard = vim.env.SSH_TTY and "" or "unnamedplus"
o.sessionoptions = { "blank", "buffers", "curdir", "globals", "help", "tabpages", "winsize", "terminal" }

-- disable some default providers
g.loaded_node_provider = 0
g.loaded_python3_provider = 0
g.loaded_perl_provider = 0
g.loaded_ruby_provider = 0
g.markdown_recommended_style = 0

if vim.fn.executable "rg" == 1 then
  o.grepformat = "%f:%l:%c:%m"
  o.grepprg = "rg --vimgrep --smart-case"
end

if vim.fn.has "nvim-0.10" == 1 then
  o.smoothscroll = true
end

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
    if not item then
      return
    end
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
        if string.len(prefix:gsub("%s+", "")) > 0 then
          return t:match(prefix)
        end
        return true
      end)
      :totable()
  end,
})

function _G.Fd(file_pattern, _)
  -- if first char is * then fuzzy search
  if file_pattern:sub(1, 1) == "*" then
    file_pattern = file_pattern:gsub(".", ".*%0") .. ".*"
  end
  local cmd = 'fd  --color=never --full-path --type file "' .. file_pattern .. '"'
  local result = vim.fn.systemlist(cmd)
  return result
end
if vim.fn.has "nvim-0.11" == 1 and vim.fn.executable "fd" then
  vim.opt.findfunc = "v:lua.Fd"
end

vim.env.PATH = vim.env.PATH .. ":" .. vim.fn.stdpath "data" .. "/mason/bin/"

o.wildignore:append {
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
}
