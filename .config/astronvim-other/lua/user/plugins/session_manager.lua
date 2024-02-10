return {
	{
		"Shatur/neovim-session-manager",
		enabled = true,
		event = "BufWinEnter",
		config = function()
			local sm = require("session_manager")
			local conf = require("session_manager.config")
			sm.setup({
				-- Define what to do when Neovim is started without arguments.
				-- Possible values: Disabled, CurrentDir, LastSession
				autoload_mode = conf.AutoloadMode.CurrentDir,
			})
		end,
	},
	{
		"stevearc/resession.nvim",
		event = "VeryLazy",
		enabled = false,
		keys = {
			{
				"<leader>rs",
				function()
					require("resession").save()
				end,
				desc = "Resession Save",
			},
			{
				"<leader>rl",
				function()
					require("resession").load()
				end,
				desc = "Resession Load",
			},
			{
				"<leader>rd",
				function()
					require("resession").delete()
				end,
				desc = "Resession Delete",
			},
		},
		config = function()
			local rs = require("resession")
			rs.setup({
				autosave = {
					enabled = true,
					interval = 60,
					notify = false,
				},
			})
			vim.api.nvim_create_autocmd("VimEnter", {
				callback = function()
					-- Only load the session if nvim was started with no args
					if vim.fn.argc(-1) == 0 then
						-- Save these to a different directory, so our manual sessions don't get polluted
						rs.load(vim.fn.getcwd(), { dir = "dirsession", silence_errors = true })
					end
				end,
			})
			vim.api.nvim_create_autocmd("VimLeavePre", {
				callback = function()
					rs.save(vim.fn.getcwd(), { dir = "dirsession", notify = false })
				end,
			})
		end,
	},
}
