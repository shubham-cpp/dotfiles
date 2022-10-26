local conditions = {
  buffer_not_empty = function()
    if vim.opt.filetype._value == 'toggleterm' then
      return false
    end
    return vim.fn.empty(vim.fn.expand '%:t') ~= 1
  end,
  hide_in_width = function()
    return vim.fn.winwidth(0) > 80
  end,
  lsp_active = function()
    return next(vim.lsp.get_active_clients()) ~= nil
  end,
}
local function LspAttach()
  local msg = 'Nope'
  local buf_ft = vim.api.nvim_buf_get_option(0, 'filetype')
  local clients = vim.lsp.get_active_clients()
  if next(clients) == nil then
    return msg
  end
  for _, client in ipairs(clients) do
    local filetypes = client.config.filetypes
    if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
      return client.name
    end
  end
  return msg
end

require('lualine').setup({
  options = {
    icons_enabled = true,
    -- Options: palenight | ayu_dark | ayu_mirage | dracula | material
    -- everforest | horizon | iceberg_dark | moonfly | nightfly
    -- modus-vivendi | nord
    -- If themes: material-nvim = material-stealth | tokyonight = tokyonight
    theme = 'horizon',
    component_separators = '|',
    section_separators = { left = '', right = '' },
    disabled_filetypes = {},
    always_divide_middle = true,
    -- globalstatus = true,
  },
  sections = {
    lualine_a = {
      {
        'mode',
        separator = { left = '' },
        fmt = function(str)
          return str:sub(1, 1)
        end,
      },
    },
    lualine_b = {
      { 'branch', icon = '' },
      {
        'diff',
        symbols = { added = ' ', modified = '柳 ', removed = ' ' },
        cond = conditions.hide_in_width,
      },
      {
        'diagnostics',
        sources = { 'nvim_diagnostic' },
        symbols = { error = ' ', warn = ' ', info = ' ' },
      },
      { LspAttach, icon = ' LSP:', color = { fg = '#ffffff', gui = 'bold' }, cond = conditions.lsp_active },
    },
    lualine_c = {
      { 'filename', path = 1, cond = conditions.buffer_not_empty },
      { 'filesize', cond = conditions.buffer_not_empty },
    },
    lualine_x = {
      { 'encoding', fmt = string.upper, cond = conditions.hide_in_width },
      { 'fileformat', fmt = string.upper, cond = conditions.hide_in_width },
      { 'filetype', cond = conditions.hide_in_width },
    },
    lualine_y = { { 'progress', cond = conditions.hide_in_width } },
    lualine_z = { { 'location', separator = { right = '' }, left_padding = 2 } },
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {
      {
        'filename',
        path = 1,
        cond = conditions.buffer_not_empty,
        color = { fg = '#69708C', bg = '#292D3E' },
        separator = { left = '' },
      },
      {
        'filesize',
        cond = conditions.buffer_not_empty,
        color = { fg = '#69708C', bg = '#292D3E' },
        separator = { right = '' },
      },
    },
    lualine_x = {
      { 'location', { fg = '#69708C', bg = '#292D3E' }, separator = { left = '', right = '' } },
    },
    lualine_y = {},
    lualine_z = {},
  },
  extensions = { 'nvim-tree', 'toggleterm', 'quickfix' },
})
