local fts = { "markdown", "gitcommit", "text", "Avante" }
---@type LazySpec
return {
  "bullets-vim/bullets.vim",
  ft = fts,
  init = function()
    vim.g.bullets_enabled_file_types = fts
    -- vim.g.bullets_set_mappings = 0
  end,
  -- config = function()
  --   vim.api.nvim_create_autocmd("FileType", {
  --     desc = "Setup Bullet-vim mappings because the default setup is messing up the snacks.picker input box",
  --     group = vim.api.nvim_create_augroup("sp_markdow", { clear = true }),
  --     pattern = fts,
  --     callback = function(args)
  --       local bufnr = args.buf
  --       vim.keymap.set("i", "<cr>", "<Plug>(bullets-newline)", { silent = true, buffer = bufnr, remap = true })
  --       vim.keymap.set("i", "<C-cr>", "<cr>", { silent = true, buffer = bufnr })
  --       vim.keymap.set("n", "o", "<Plug>(bullets-newline)", { silent = true, buffer = bufnr, remap = true })
  --
  --       vim.keymap.set({ "n", "v" }, "gN", "<Plug>(bullets-renumber)", { silent = true, buffer = bufnr, remap = true })
  --
  --       vim.keymap.set("i", "<c-t>", "<Plug>(bullets-demote)", { silent = true, buffer = bufnr, remap = true })
  --       vim.keymap.set("i", "<c-d>", "<Plug>(bullets-promote)", { silent = true, buffer = bufnr, remap = true })
  --
  --       vim.keymap.set("n", ">>", "<Plug>(bullets-demote)", { silent = true, buffer = bufnr, remap = true })
  --       vim.keymap.set("n", "<<", "<Plug>(bullets-promote)", { silent = true, buffer = bufnr, remap = true })
  --
  --       vim.keymap.set("x", ">", "<Plug>(bullets-demote)", { silent = true, buffer = bufnr, remap = true })
  --       vim.keymap.set("x", "<", "<Plug>(bullets-promote)", { silent = true, buffer = bufnr, remap = true })
  --
  --       vim.keymap.set(
  --         "n",
  --         "<leader>xx",
  --         "<Plug>(bullets-toggle-checkbox)",
  --         { desc = "Toggle Checkbox", silent = true, buffer = bufnr, remap = true }
  --       )
  --     end,
  --   })
  -- end,
}
