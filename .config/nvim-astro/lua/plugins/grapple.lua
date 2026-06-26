---@type LazySpec
return {
  "cbochs/grapple.nvim",
  opts = { scope = "git_branch", icons = true, status = false },
  keys = {
    { "<Leader>A", "<cmd>Grapple toggle<cr>", desc = "Grapple toggle" },
    { "<Leader>`", "<cmd>Grapple toggle<cr>", desc = "Grapple toggle" },
    { "<C-e>", "<cmd>Grapple toggle_tags<cr>", desc = "Grapple toggle tags" },
    { "<C-s-n>", "<cmd>Grapple cycle_tags next<cr>", desc = "Grapple cycle next tag" },
    { "<C-s-p>", "<cmd>Grapple cycle_tags prev<cr>", desc = "Grapple cycle prev tag" },
    { "<localleader>1", "<cmd>Grapple select index=1<cr>", desc = "Grapple select 1" },
    { "<localleader>2", "<cmd>Grapple select index=2<cr>", desc = "Grapple select 2" },
    { "<localleader>3", "<cmd>Grapple select index=3<cr>", desc = "Grapple select 3" },
    { "<localleader>4", "<cmd>Grapple select index=4<cr>", desc = "Grapple select 4" },
    { "<localleader>5", "<cmd>Grapple select index=5<cr>", desc = "Grapple select 5" },
    { "<localleader>6", "<cmd>Grapple select index=6<cr>", desc = "Grapple select 6" },
    { "<localleader>7", "<cmd>Grapple select index=7<cr>", desc = "Grapple select 7" },
    { "<localleader>8", "<cmd>Grapple select index=8<cr>", desc = "Grapple select 8" },
    { "<localleader>9", "<cmd>Grapple select index=9<cr>", desc = "Grapple select 9" },
  },
}
