local augroup = vim.api.nvim_create_augroup("user_autocmds", { clear = true })

-- Big file detection
local bigfile = require("core.bigfile")

vim.api.nvim_create_autocmd("BufReadPre", {
  desc = "Big file detection (before load)",
  group = vim.api.nvim_create_augroup("bigfile_pre", { clear = true }),
  callback = function(args)
    if bigfile.is_big(args.buf) then
      bigfile.setup(args.buf)
    end
  end,
})

vim.api.nvim_create_autocmd("BufReadPost", {
  desc = "Big file detection (after load, for line count/length checks)",
  group = vim.api.nvim_create_augroup("bigfile_post", { clear = true }),
  callback = function(args)
    if not vim.b[args.buf].bigfile and bigfile.is_big(args.buf) then
      bigfile.setup(args.buf)
    end
  end,
})

vim.api.nvim_create_autocmd("BufWritePre", {
  desc = "Format on save",
  group = augroup,
  callback = function(args)
    if vim.b[args.buf].bigfile then
      return
    end
    if vim.b[args.buf].disable_auto_format or vim.g.disable_auto_format then
      return
    end
    require("conform").format({ async = false, lsp_fallback = true })
  end,
})

vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight yanked text",
  group = augroup,
  callback = function()
    vim.hl.on_yank({ timeout = 150 })
  end,
})

vim.api.nvim_create_autocmd({ "FocusGained", "TermClose", "TermLeave" }, {
  desc = "Reload file when regaining focus or closing terminal",
  group = augroup,
  callback = function()
    if vim.bo.buftype ~= "nofile" then
      vim.cmd("checktime")
    end
  end,
})

vim.api.nvim_create_autocmd("BufReadPost", {
  desc = "Restore cursor to last position when reopening a file",
  group = augroup,
  callback = function(args)
    local mark = vim.api.nvim_buf_get_mark(args.buf, '"')
    if mark[1] > 0 and mark[1] <= vim.api.nvim_buf_line_count(args.buf) then
      vim.api.nvim_win_set_cursor(0, mark)
    end
  end,
})

vim.api.nvim_create_autocmd("BufWritePre", {
  desc = "Auto-create parent directories on save",
  group = augroup,
  callback = function(args)
    local path = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(args.buf), ":p:h")
    if not path:match("^%w%w+:[\\/][\\/]") then
      vim.fn.mkdir(path, "p")
    end
  end,
})

vim.api.nvim_create_autocmd("VimResized", {
  desc = "Equalize splits when terminal window is resized",
  group = augroup,
  callback = function()
    local curtab = vim.api.nvim_get_current_tabpage()
    vim.cmd("tabdo wincmd =")
    vim.api.nvim_set_current_tabpage(curtab)
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  desc = "Close special buffers with q",
  group = augroup,
  pattern = { "help", "lspinfo", "notify", "qf", "checkhealth", "man", "gitcommit", "sql" },
  callback = function(args)
    vim.bo[args.buf].buflisted = false
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = args.buf, silent = true })
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  desc = "Enable spellcheck for prose filetypes",
  group = augroup,
  pattern = { "gitcommit", "markdown", "text" },
  callback = function()
    vim.opt_local.spell = true
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  desc = "Unlist quickfix and man pages from buffer list",
  group = augroup,
  pattern = { "qf", "man" },
  callback = function(args)
    vim.bo[args.buf].buflisted = false
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  desc = "Fix JSON conceal (Treesitter hides quotes by default)",
  group = augroup,
  pattern = { "json", "jsonc", "json5" },
  callback = function()
    vim.opt_local.conceallevel = 0
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  desc = "Fix comment continuation",
  group = augroup,
  callback = function()
    vim.opt_local.formatoptions = "jcrqlnt"
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  desc = "treesitter setup",
  pattern = {
    "rust",
    "javascript",
    "javascriptreact",
    "typescript",
    "typescriptreact",
    "json",
    "json5",
    "yaml",
    "toml",
    "dockerfile",
    "html",
    "css",
    "scss",
    "go",
    "python",
    "vim",
    "git",
    "gitcommit",
    "gitignore",
    "svelte",
    "vue",
    "markdown",
    "lua",
    "bash",
    "sh",
    "zsh",
    "fish",
  },
  group = augroup,
  callback = function()
    vim.treesitter.start()
  end,
})
