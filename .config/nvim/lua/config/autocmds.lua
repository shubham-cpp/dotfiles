-- vim.cmd("packadd nvim.undotree")

local function augroup(name)
	return vim.api.nvim_create_augroup("Config" .. name, { clear = true })
end

vim.api.nvim_create_autocmd("TextYankPost", {
	group = augroup("HighlightYank"),
	desc = "Highlight yanked text",
	callback = function()
		vim.hl.on_yank()
	end,
})

vim.api.nvim_create_autocmd({ "FocusGained", "TermClose", "TermLeave" }, {
	group = augroup("Checktime"),
	desc = "Check for externally changed files",
	callback = function()
		if vim.bo.buftype ~= "nofile" then
			vim.cmd.checktime()
		end
	end,
})

vim.api.nvim_create_autocmd("VimResized", {
	group = augroup("ResizeSplits"),
	desc = "Equalize splits after resize",
	callback = function()
		local current_tab = vim.api.nvim_get_current_tabpage()
		vim.cmd("tabdo wincmd =")
		pcall(vim.api.nvim_set_current_tabpage, current_tab)
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	group = augroup("CloseWithQ"),
	desc = "Close temporary windows with q",
	pattern = { "nvim-undotree", "checkhealth", "help", "lspinfo", "man", "qf", "git" },
	callback = function(args)
		vim.bo[args.buf].buflisted = false

		for _, keymap in ipairs(vim.api.nvim_buf_get_keymap(args.buf, "n")) do
			if keymap.lhs == "q" then
				return
			end
		end

		vim.keymap.set("n", "q", "<cmd>close<cr>", {
			buffer = args.buf,
			desc = "Win: Close",
			nowait = true,
			silent = true,
		})
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	group = augroup("Prose"),
	desc = "Enable writing aids for prose",
	pattern = { "gitcommit", "markdown", "text" },
	callback = function()
		vim.opt_local.breakindent = true
		vim.opt_local.linebreak = true
		vim.opt_local.spell = true
		vim.opt_local.wrap = true
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	group = augroup("FormatOptions"),
	desc = "Fix Comment Continuation",
	callback = function()
		vim.opt_local.formatoptions = "jcrqlnt"
	end,
})

vim.api.nvim_create_autocmd("TermOpen", {
	group = augroup("terminal_settings"),
	pattern = { "term://*fish", "term://*zsh" },
	desc = "Disable line number/fold column/sign column for terminals",
	callback = function()
		vim.opt_local.number = false
		vim.opt_local.relativenumber = false
		vim.opt_local.foldcolumn = "0"
		vim.opt_local.signcolumn = "no"
		vim.opt_local.foldmethod = "manual"
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	group = augroup("Json"),
	desc = "Show JSON quotes",
	pattern = { "json", "jsonc" },
	callback = function()
		vim.opt_local.conceallevel = 0
	end,
})

vim.api.nvim_create_autocmd("BufWritePre", {
	group = augroup("UndoSwap"),
	desc = "Skip swapfile, undo, for sensitive files",
	pattern = { "/tmp/*", "*.env*", "*.key" },
	callback = function()
		vim.opt_local.undofile = false
		vim.opt_local.swapfile = false
	end,
})
