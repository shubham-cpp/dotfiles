require('neorg').setup({
	-- Tell Neorg what modules to load
	load = {
		['core.defaults'] = {}, -- Load all the default modules
		['core.norg.concealer'] = {
			config = {
				conceals = true,
			},
		}, -- Allows for use of icons
		['core.norg.dirman'] = { -- Manage your directories with Neorg
			config = {
				workspaces = {
					work = '~/Documents/Notes/Neorg/work',
					me = '~/Documents/Notes/Neorg/personal',
				},
			},
		},
		['core.norg.completion'] = {
			config = {
				engine = 'nvim-cmp',
			},
		},
		['core.presenter'] = {
			config = {
				zen_mode = 'truezen',
			},
		},
		['core.gtd.base'] = {
			config = {
				workspace = 'work',
				custom_tag_completion = true,
				syntax = {
					context = '#contexts',
					start = '#time.start',
					due = '#time.due',
					waiting = '#waiting.for',
				},
				default_lists = {
					inbox = 'inbox.norg',
					review = 'review.norg',
					testing = 'testing.norg',
				},
			},
		},
		['core.gtd.ui'] = {
			config = { -- Note that this table is optional and doesn't need to be provided
				-- Configuration here
			},
		},
	},
})
