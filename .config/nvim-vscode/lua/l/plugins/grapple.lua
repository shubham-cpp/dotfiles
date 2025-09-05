local cmd_str = require("l.config.utils").cmd_str
---@type LazySpec
return {
  "cbochs/grapple.nvim",
  enabled = vim.g.vscode == nil,
  opts = {
    --- @type "git_branch"|"git"|"cwd"|"global"|"static"|"lsp"
    scope = "git_branch",
    icons = true,
    status = false,
  },
  keys = function()
    local k = {}
    -- stylua: ignore start
    table.insert(k, { "<leader>A", cmd_str("Grapple toggle"),          desc = "Tag a file" })
    table.insert(k, { "<leader>`", cmd_str("Grapple toggle"),          desc = "Tag a file" })
    table.insert(k, { "<c-e>",     cmd_str("Grapple toggle_tags"),     desc = "Toggle tags menu" })
    table.insert(k, { "<c-s-n>",   cmd_str("Grapple cycle_tags next"), desc = "Go to next tag" })
    table.insert(k, { "<c-s-p>",   cmd_str("Grapple cycle_tags prev"), desc = "Go to previous tag" })

    for i = 1, 9, 1 do
      local cmd = cmd_str("Grapple select index=" .. i )
      table.insert(k, {"<localleader>" .. i, cmd, desc = "Select Tag #" .. i })
    end
    -- stylua: ignore end
    return k
  end,
}
