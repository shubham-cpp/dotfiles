local u = require("config.utils")

vim.pack.add({ u.gh("stevearc/overseer.nvim") })

require("overseer").setup()

vim.cmd.cnoreabbrev("OS OverseerShell")

vim.api.nvim_create_user_command("OverseerRestartLast", function()
	local overseer = require("overseer")
	local task_list = require("overseer.task_list")
	local tasks = overseer.list_tasks({
		status = {
			overseer.STATUS.SUCCESS,
			overseer.STATUS.FAILURE,
			overseer.STATUS.CANCELED,
		},
		sort = task_list.sort_finished_recently,
	})
	if vim.tbl_isempty(tasks) then
		vim.notify("No tasks found", vim.log.levels.WARN)
	else
		local most_recent = tasks[1]
		overseer.run_action(most_recent, "restart")
	end
end, {})

local rtps = vim.api.nvim_list_runtime_paths()
local all_comps = {}
for _, p in ipairs(rtps) do
	for _, f in ipairs(vim.fn.globpath(p, "compiler/*.vim", false, true)) do
		table.insert(all_comps, vim.fn.fnamemodify(f, ":t:r"))
	end
end
vim.api.nvim_create_user_command("Make", function(params)
	-- Insert args at the '$*' in the makeprg
	local cmd, num_subs = vim.o.makeprg:gsub("%$%*", params.args)
	if num_subs == 0 then
		cmd = cmd .. " " .. params.args
	end
	local task = require("overseer").new_task({
		cmd = vim.fn.expandcmd(cmd),
		components = {
			{
				"on_output_quickfix",
				open = not params.bang,
				open_height = 8,
				errorformat = vim.o.errorformat,
			},
			"default",
		},
	})
	task:start()
end, {
	desc = "Run your makeprg as an Overseer task",
	nargs = "*",
	bang = true,
	complete = function(arg_lead, cmd_line)
		local parts = vim.split(cmd_line, "%s+")
		if #parts == 1 or (#parts == 2 and arg_lead == parts[2]) then
			return vim.tbl_filter(function(c)
				return vim.startswith(c, arg_lead)
			end, all_comps)
		else
			return vim.fn.getcompletion(arg_lead, "file")
		end
	end,
})
