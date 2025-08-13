local now, later = MiniDeps.now, MiniDeps.later

now(function()
  require("mini.icons").setup()
  require("mini.notify").setup()
  vim.notify = require("mini.notify").make_notify()
end)

later(function()
  require("mini.git").setup()
  require("mini.diff").setup({
    view = {
      style = "sign",
      signs = { add = "▒", change = "▒", delete = "▒" },
      -- Priority of used visualization extmarks
      priority = 199,
    },
  })
  require("mini.indentscope").setup({
    draw = { delay = 80 },
  })
end)
