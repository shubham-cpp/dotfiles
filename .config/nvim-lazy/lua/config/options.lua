-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
vim.opt.wrap = true
vim.opt.showbreak = "󰄾 "
vim.opt.iskeyword:append("-")
vim.opt.scrolloff = 8
vim.opt.sessionoptions = "buffers,curdir,tabpages,winsize,help,globals,skiprtp"

vim.opt.wildignore:append({
  "*.o",
  "*.obj",
  "*.out",
  "*.class",

  "*.jpg",
  "*.jpeg",
  "*.png",
  "*.gif",

  "*.zip",
  "*.tar.gz",
  "*.tar.xz",
  "*.tar.bz2",
  "*.rar",

  "*.pyc",
  "*.pyo",

  "*.DS_Store",
  "*.swp",

  "node_modules",
  "vendor",
  ".git",
})

if vim.fn.executable("rg") == 1 then
  vim.opt.grepprg = "rg --vimgrep --smart-case" -- Also check RIPGREP_CONFIG_PATH="$HOME/.config/ripgreprc"
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
