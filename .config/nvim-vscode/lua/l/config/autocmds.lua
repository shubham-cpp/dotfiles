local au_buffer = vim.api.nvim_create_augroup("sp_buffer", {})

vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight copied text",
  group = au_buffer,
  callback = function()
    vim.highlight.on_yank({ higroup = "IncSearch", timeout = 100 })
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  desc = "Fix Comment Continuation",
  group = au_buffer,
  callback = function()
    vim.opt_local.formatoptions = "jcrqlnt"
  end,
})

vim.api.nvim_create_autocmd({ "FocusGained", "TermClose", "TermLeave" }, {
  desc = "Check if we need to reload the file when it changed",
  group = au_buffer,
  callback = function()
    if vim.o.buftype ~= "nofile" then
      vim.cmd "checktime"
    end
  end,
})
