local add, later = MiniDeps.add, MiniDeps.later
later(function()
  add({ source = "cbochs/grapple.nvim" })
  local grapple = require "grapple"
  grapple.setup({
    --- @type "git_branch"|"git"|"cwd"|"global"|"static"|"lsp"
    scope = "git_branch",
    icons = true,
    status = false,
  })
  local cmd_str = require("u.utils").cmd_str
  -- stylua: ignore start
  vim.keymap.set("n", "<leader>A", cmd_str("Grapple toggle"),          { desc = "Tag a file" })
  vim.keymap.set("n", "<leader>`", cmd_str("Grapple toggle"),          { desc = "Tag a file" })
  vim.keymap.set("n", "<c-e>",     cmd_str("Grapple toggle_tags"),     { desc = "Toggle tags menu" })
  vim.keymap.set("n", "<c-s-n>",   cmd_str("Grapple cycle_tags next"), { desc = "Go to next tag" })
  vim.keymap.set("n", "<c-s-p>",   cmd_str("Grapple cycle_tags prev"), { desc = "Go to previous tag" })

  for i = 1, 9, 1 do
    local cmd = cmd_str("Grapple select index=" .. i )
    vim.keymap.set("n", "<localleader>" .. i, cmd, {desc = "Select Tag #" .. i} )
  end
  -- stylua: ignore end
end)
