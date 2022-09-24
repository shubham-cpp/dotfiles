require('nvim-treesitter.configs').setup({
  ensure_installed = {
    'json',
    'json5',
    'jsonc',
    'jsdoc',
    'yaml',
    'toml',
    'html',
    'css',
    'scss',
    'javascript',
    'typescript',
    'tsx',
    'vue',
    'svelte',
    'lua',
    'vim',
    'python',
    'java',
    'c',
    'cpp',
    'regex',
    'bash',
    'fish',
    'markdown',
    'markdown_inline',
    'comment',
    'dockerfile',
  },
  highlight = {
    enable = true,
    use_languagetree = true,
    -- disable = { 'org' }, -- Remove this to use TS highlighter for some of the highlights (Experimental)
    -- additional_vim_regex_highlighting = { 'org' }, -- Required since TS highlighter doesn't support all syntax features (conceal)
  },
  autotag = { enable = true },
  context_commentstring = {
    enable = true,
    enable_autocmd = false,
  },
  textobjects = {
    select = {
      enable = true,
      -- Automatically jump forward to textobj, similar to targets.vim
      lookahead = true,
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        ['ac'] = '@class.outer',
        ['ic'] = '@class.inner',
      },
    },
    swap = {
      enable = true,
      swap_next = {
        [']z'] = '@parameter.inner',
      },
      swap_previous = {
        ['[z'] = '@parameter.inner',
      },
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        [']m'] = '@function.outer',
        [']]'] = '@class.outer',
      },
      goto_next_end = {
        [']M'] = '@function.outer',
        [']['] = '@class.outer',
      },
      goto_previous_start = {
        ['[m'] = '@function.outer',
        ['[['] = '@class.outer',
      },
      goto_previous_end = {
        ['[M'] = '@function.outer',
        ['[]'] = '@class.outer',
      },
    },
  },
  playground = {
    enable = true,
    disable = {},
    updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
    persist_queries = false, -- Whether the query persists across vim sessions
    keybindings = {
      toggle_query_editor = 'o',
      toggle_hl_groups = 'i',
      toggle_injected_languages = 't',
      toggle_anonymous_nodes = 'a',
      toggle_language_display = 'I',
      focus_language = 'f',
      unfocus_language = 'F',
      update = 'R',
      goto_node = '<cr>',
      show_help = '?',
    },
  },
  query_linter = {
    enable = true,
    use_virtual_text = true,
    lint_events = { 'BufWrite', 'InsertLeave' },
  },
  --matchup = {
  --	enable = true,
  --},
})

local fn = vim.fn
local opt = vim.opt

function _G.foldtext()
  local indent_level = fn.indent(vim.v.foldstart)
  local indent = fn['repeat'](' ', indent_level)
  local first = fn.substitute(fn.getline(vim.v.foldstart), '\\v\\s*', '', '')

  return indent .. first .. '...' .. fn.getline(vim.v.foldend):gsub('^%s*', '')
end

opt.foldmethod = 'expr'
opt.foldexpr = 'nvim_treesitter#foldexpr()'
opt.foldtext = 'v:lua.foldtext()'
opt.foldlevelstart = 99
