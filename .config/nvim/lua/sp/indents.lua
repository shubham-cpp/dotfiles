require('indent_blankline').setup({
    filetype_exclude = {
        'help',
        'dashboard',
        'dashpreview',
        'LuaTree',
        'vista',
        'sagahover',
        'NvimTree',
        'fzf',
        'floaterm',
        'startify',
        'packer',
        'toggleterm',
    },
    space_char_blankline = ' ',
    show_current_context = true,
    show_current_context_start = true,
    -- char_highlight_list = {
    --     'IndentBlanklineIndent1',
    --     'IndentBlanklineIndent2',
    -- },
    -- space_char_highlight_list = {
    --     'IndentBlanklineIndent1',
    --     'IndentBlanklineIndent2',
    -- },
})
