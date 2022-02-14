local function define_augroups(definitions) 
    -- Create autocommand groups based on the passed definitions
    --
    -- The key will be the name of the group, and each definition
    -- within the group should have:
    --    1. Trigger
    --    2. Pattern
    --    3. Text
    -- just like how they would normally be defined from Vim itself
    for group_name, definition in pairs(definitions) do
        vim.cmd("augroup " .. group_name)
        vim.cmd "autocmd!"

        for _, def in pairs(definition) do
            local command = table.concat(vim.tbl_flatten { "autocmd", def }, " ")
            vim.cmd(command)
        end

        vim.cmd "augroup END"
    end
end

define_augroups({
    highlight_yank = {
        "TextYankPost * silent! lua vim.highlight.on_yank()"
    },
    file_handling = {
        "FileType vim,lua set foldmethod=marker",
        "FileType vim,lua nnoremap <buffer> <silent> <F5> :so %<cr>",
        "FileType vim,tex let b:autoformat_autoindent=0",
        [[ BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif ]],

        "BufWritePost Xresources,Xdefaults !xrdb %",
        "BufEnter *.fish set filetype=fish",
        "BufEnter *profile set filetype=sh",
        "BufEnter vifmrc set filetype=vim",

        [[ BufWritePre * %s/\s\+$//e, ]],
        [[ BufWritepre * %s/\n\+\%$//e ]],
        "FocusGained,BufEnter * checktime",
        "FileType css,javascript,html,json setlocal sw=2 ts=2",
        [[ BufWritePre * let &bex = '@' . strftime("%F.%H:%M") ]],
    },
    dynamic_case = {
        "CmdLineEnter : set nosmartcase",
        "CmdLineLeave : set smartcase"
    },
    terminals = {
        "TermOpen term://* startinsert"
    },
    comments = {
        [[ FileType apache,sxhkdrc,toml,fish setlocal commentstring=#\ %s ]],
        [[ FileType lisp setlocal commentstring=;;\ %s ]],
        [[ FileType htmldjango setlocal commentstring=<!--%s--> ]],
        [[ FileType xdefaults setlocal commentstring=!\ %s ]],
        [[ BufEnter *.vifm setlocal commentstring=\"\ %s ]],
        [[ BufEnter template,mbsyncrc setlocal commentstring=##\ %s ]],
        [[ BufEnter *.kbd setlocal ft=lisp | setlocal commentstring=;;\ %s ]],
        [[ FileType * set formatoptions-=c formatoptions-=r formatoptions-=o ]],
        [[ FileType c,cpp setlocal comments-=:// comments+=f:// ]],

    },
    plugins = {
        "BufWritePost packer-plugins.lua source <afile> | PackerCompile",
        "BufWritePost plugins.lua source <afile> | PackerCompile",
    }
})
