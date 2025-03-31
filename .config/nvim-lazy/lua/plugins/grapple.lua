---@type LazySpec
return {
  "cbochs/grapple.nvim",
  enabled = true,
  dependencies = "echasnovski/mini.icons",
  cmd = "Grapple",
  opts = {
    --- @type "git_branch"|"git"|"cwd"|"global"|"static"|"lsp"
    scope = "git_branch",
    icons = true,
    status = false,
  },
  keys = function()
    local k = {
      { "<leader>A", "<cmd>Grapple toggle<cr>", desc = "Tag a file" },
      { "<leader>`", "<cmd>Grapple toggle<cr>", desc = "Tag a file" },
      { "<c-e>", "<cmd>Grapple toggle_tags<cr>", desc = "Toggle tags menu" },

      { "<c-s-n>", "<cmd>Grapple cycle_tags next<cr>", desc = "Go to next tag" },
      { "<c-s-p>", "<cmd>Grapple cycle_tags prev<cr>", desc = "Go to previous tag" },
    }
    for i = 1, 9, 1 do
      local cmd = "<cmd>Grapple select index=" .. i .. "<cr>"
      table.insert(k, { "<localleader>" .. i, cmd, desc = "Select Tag #" .. i })
    end
    return k
  end,
}
