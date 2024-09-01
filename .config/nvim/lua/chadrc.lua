---@type ChadrcConfig
local M = {}

M.ui = {
  cmp = {
    icons = true,
    lspkind_text = true,
    ---@type 'default'|'flat_light'|'flat_dark'|'atom'|'atom_colored'
    style = 'default', --
  },
  tabufline = {
    enabled = true,
  },
  theme = 'gruvchad', --'poimandres' | 'tomorrow_night' | 'nightlamp' | 'gruvchad' |  'everforest' |  'kanagawa'
}
M.base46 = {
  transparency = true,
  integrations = {
    'defaults',
    'cmp',
    'dap',
    'codeactionmenu',
    'git',
    'lsp',
    'mason',
    'navic',
    'neogit',
    'whichkey',
    'treesitter',
    'semantic_tokens',
    'todo',
    -- more
  },
  hl_override = {
    LspReferenceText = { bg = '#2D3031', fg = 'white', bold = true },
    LspReferenceRead = { bg = '#2D3031', fg = 'white', bold = true },
    LspReferenceWrite = { bg = '#2D3031', fg = 'white', bold = true },
    QuickScopePrimary = { fg = '#ffdd33', bg = '#2D3031', underline = true, bold = true },
    QuickScopeSecondary = { fg = '#cc8c3c', bg = '#2D3031', underline = true, bold = true },
  },
}
vim.api.nvim_set_hl(0, 'QuickScopePrimary', {
  fg = '#ffdd33',
  bg = '#2D3031',
  underline = true,
  bold = true,
})
vim.api.nvim_set_hl(0, 'QuickScopeSecondary', {
  fg = '#cc8c3c',
  bg = '#2D3031',
  underline = true,
  bold = true,
})
return M
