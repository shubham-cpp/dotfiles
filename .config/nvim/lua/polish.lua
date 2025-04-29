vim.opt.path:append "**"
vim.opt.iskeyword:append "-" -- for css make 'is-active' as one word
vim.opt.wildignore:append {
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

for i = 1, 9 do
  vim.keymap.set("n", "<leader>" .. i, i .. "gt", { desc = "Goto tab " .. i })
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
