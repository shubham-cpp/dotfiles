local tree_cb = require("nvim-tree.config").nvim_tree_callback

local g = vim.g
g.nvim_tree_add_trailing = 1
g.nvim_tree_special_files = { ["README.md"] = 1, Makefile = 1, MAKEFILE = 1 }

local list = {
    { key = { "e", "l" }, cb = tree_cb("edit") },
    { key = "<M-CR>", cb = tree_cb("tabnew") },
    { key = "h", cb = tree_cb("code_node") },
    { key = ".", cb = tree_cb("toggle_dotfiles") },
    { key = "?", cb = tree_cb("toggle_help") },
}
require("nvim-tree").setup({
    auto_close = true,
    diagnostics = {
        enable = true,
    },
    filters = {
        dotfiles = true,
        custom = {
            ".git",
            "node_modules",
            ".cache",
            "__pycache__",
            ".vscode",
            ".steam",
            ".mozilla",
            ".pki",
        },
    },
    view = { mappings = { list = list }, relativenumber = true },
    trash = {
        cmd = "trash",
        require_confirm = false
    }
})
