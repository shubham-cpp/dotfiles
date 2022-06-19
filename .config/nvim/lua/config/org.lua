require('orgmode').setup_ts_grammar()
require('orgmode').setup({
	org_agenda_files = { '~/Documents/Notes/Neorg/*' },
	org_default_notes_file = '~/Documents/Notes/Neorg/refile.org',
	org_agenda_text_search_extra_files = { 'agenda-archives' },
	org_hide_leading_stars = true,
	org_deadline_warning_days = 7,
})
