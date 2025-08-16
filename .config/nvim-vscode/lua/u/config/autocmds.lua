vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight copied text",
  group = vim.api.nvim_create_augroup("sp_buffer", {}),
  callback = function()
    vim.highlight.on_yank({ higroup = "IncSearch", timeout = 100 })
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  desc = "Fix Comment Continuation",
  group = vim.api.nvim_create_augroup("sp_buffer", {}),
  callback = function()
    vim.opt_local.formatoptions = "jcrqlnt"
  end,
})
