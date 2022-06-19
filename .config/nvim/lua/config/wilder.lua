-- config goes here
vim.fn['wilder#setup']({
	modes = { ':', '/', '?' },
	next_key = '<Tab>',
	previous_key = '<S-Tab>',
	accept_key = '<Down>',
	reject_key = '<Up>',
})

-- local wilder = require("wilder")
-- wilder.setup({
-- 	modes = { ":", "/", "?" },
-- 	next_key = "<Tab>",
-- 	previous_key = "<S-Tab>",
-- 	accept_key = "<Down>",
-- 	reject_key = "<Up>",
-- })
-- wilder.set_option("pipeline", {
-- 	wilder.branch(
-- 		wilder.cmdline_pipeline({
-- 			language = "python", -- vim or python
-- 			-- 0 turns off fuzzy matching
-- 			-- 1 turns on fuzzy matching
-- 			-- 2 partial fuzzy matching (match does not have to begin with the same first letter)
-- 			fuzzy = 1,
-- 		}),
-- 		wilder.python_search_pipeline({
-- 			pattern = wilder.python_fuzzy_pattern(),
-- 			sorter = wilder.python_difflib_sorter(),
-- 			-- see :h wilder#python_search() for more details
-- 			engine = "re",
-- 		})
-- 	),
-- 	-- wilder.branch(
-- 	-- 	wilder.python_file_finder_pipeline({
-- 	-- 		file_command = { "fd", "-tf" },
-- 	-- 		dir_command = { "fd", "-td" },
-- 	-- 		-- use {'cpsm_filter'} for performance, requires cpsm vim plugin
-- 	-- 		-- found at https://github.com/nixprime/cpsm
-- 	-- 		filters = { "fuzzy_filter", "difflib_sorter" },
-- 	-- 	}),
-- 	-- 	wilder.cmdline_pipeline(),
-- 	-- 	wilder.python_search_pipeline()
-- 	-- ),
-- })
-- wilder.set_option(
-- 	"renderer",
-- 	wilder.popupmenu_renderer(wilder.popupmenu_border_theme({
-- 		highlighter = wilder.basic_highlighter(),
-- 		min_width = "100%", -- minimum height of the popupmenu, can also be a number
-- 		min_height = "50%", -- to set a fixed height, set max_height to the same value
-- 		reverse = 0, -- if 1, shows the candidates from bottom to top
-- 	}))
-- )
-- wilder.set_option("pipeline", {
-- 	wilder.branch(
-- 		wilder.python_file_finder_pipeline({
-- 			file_command = { "fd", "-tf" },
-- 			dir_command = { "fd", "-td" },
-- 			filters = { "fuzzy_filter", "difflib_sorter" },
-- 		}),
-- 		wilder.cmdline_pipeline(),
-- 		wilder.python_search_pipeline()
-- 	),
-- })

vim.cmd([[
call wilder#set_option('pipeline', [
      \   wilder#branch(
      \     wilder#cmdline_pipeline({
      \       'language': 'python',
      \       'fuzzy': 1,
      \     }),
      \     wilder#python_search_pipeline({
      \       'pattern': wilder#python_fuzzy_pattern(),
      \       'sorter': wilder#python_difflib_sorter(),
      \       'engine': 're',
      \     }),
      \   ),
      \ ])

call wilder#set_option('pipeline', [
      \   wilder#branch(
      \     wilder#python_file_finder_pipeline({
      \       'file_command': ['fd', '-tf'],
      \       'dir_command': ['fd', '-td'],
      \       'filters': ['fuzzy_filter', 'difflib_sorter'],
      \     }),
      \     wilder#cmdline_pipeline(),
      \     wilder#python_search_pipeline(),
      \   ),
      \ ])

call wilder#set_option('renderer', wilder#popupmenu_renderer(wilder#popupmenu_border_theme({
      \ 'highlighter': wilder#basic_highlighter(),
      \ 'min_width': '100%',
      \ 'min_height': '50%',
      \ 'reverse': 0,
      \ })))
]])
