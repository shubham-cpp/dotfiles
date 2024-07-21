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
    return vim.fn.winwidth(0) > 80 and next(vim.lsp.get_clients({ bufnr = 0 })) ~= nil
  end,
}
local function servers_attached()
  local msg = 'None'
  local clients = vim
    .iter(vim.lsp.get_clients({ bufnr = 0 }))
    :map(function(client)
      return client.name
    end)
    :flatten()
    :join ','
  return clients == '' and msg or clients
end

-- local navic = require 'nvim-navic'
local config = {
  options = {
    globalstatus = true,
    component_separators = '',
    section_separators = '',
    theme = {
      normal = { c = { bg = '#181818', fg = '#303540', blend = 100 } },
      inactive = { c = { bg = '#101010', fg = '#303540' } },
    },
  },
  sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_y = {},
    lualine_z = {},
    lualine_c = {},
    lualine_x = {},
  },
  inactive_sections = {
    lualine_a = {},
    lualine_v = {},
    lualine_y = {},
    lualine_z = {},
    lualine_c = {},
    lualine_x = {},
  },
  tabline = {
    lualine_a = {},
    lualine_v = {},
    lualine_y = {},
    lualine_z = {},
    lualine_c = {},
    lualine_x = {},
  },

  winbar = { lualine_c = {} },
  inactive_winbar = { lualine_c = {} },
  extensions = { 'lazy', 'neo-tree', 'quickfix', 'toggleterm', 'man' },
}
local function ins_left(component)
  table.insert(config.sections.lualine_c, component)
end
local function ins_right(component)
  table.insert(config.sections.lualine_x, component)
end
local function ins_left_tab(component)
  table.insert(config.tabline.lualine_c, component)
end
local function ins_right_tab(component)
  table.insert(config.tabline.lualine_x, component)
end
ins_left_tab({
  'buffers',
  buffers_color = {
    -- Same values as the general color option can be used here.
    active = { fg = '#96a6c8', bg = '#303540' }, -- Color for active tab.
    inactive = { fg = '#52494e', bg = '#181818' }, -- Color for inactive tab.
  },
})
ins_left_tab '%='
ins_right_tab({
  'tabs',
  tabs_color = {
    -- Same values as the general color option can be used here.
    active = { fg = '#96a6c8', bg = '#303540' }, -- Color for active tab.
    inactive = { fg = '#52494e', bg = '#181818' }, -- Color for inactive tab.
  },
})
ins_left({
  'filetype',
  icon_only = true,
  condition = conditions.buffer_not_empty,
  left_padding = 0,
})
ins_left({
  'filename',
  path = 1,
  -- shorting_target = 40,
  condition = conditions.buffer_not_empty,
  color = { fg = '#96a6c8' },
})
ins_left({
  'branch',
  icon = '',
  color = { fg = '#A3BE8C' },
})
ins_left({
  'diff',
  colored = true,
  symbols = { added = ' ', modified = '󰇊 ', removed = ' ' },
  -- symbols = { added = ' ', modified = ' ', removed = ' ' },
  cond = conditions.hide_in_width,
})
ins_left({
  'diagnostics',
  sources = { 'nvim_lsp' },
})

local navic = require 'nvim-navic'
ins_left({
  'navic',
  ---@type "static"| "dynamic"| nil
  color_correction = nil,
  navic_opts = nil,
})
ins_right({
  servers_attached,
  icon = ' LSPs:',
  color = { fg = '#8992a7', gui = 'bold' },
  cond = conditions.lsp_active,
})
ins_right({ 'progress', color = { fg = '#957FB8' } })
ins_right({ 'location', color = { fg = '#6CAF95' } })
ins_right({
  'searchcount',
  icon = '',
  -- color = { fg = '#DCA561' },
  color = { fg = '#cc8c3c' },
})
ins_right({
  function()
    if vim.fn.mode():find '[vV]' then
      local ln_beg = vim.fn.line 'v'
      local ln_end = vim.fn.line '.'

      local lines = ln_beg <= ln_end and ln_end - ln_beg + 1 or ln_beg - ln_end + 1

      return 'sel: ' .. tostring(vim.fn.wordcount().visual_chars) .. ' | ' .. tostring(lines)
    else
      return ''
    end
  end,
  color = { fg = '#c5c9c5' },
})

require('lualine.components.buffers').buffers = function(self)
  local buffers = {}
  self.bufpos2nr = {}
  for _, b in ipairs(vim.t['bufs']) do
    if vim.fn.buflisted(b) ~= 0 and vim.api.nvim_buf_get_option(b, 'buftype') ~= 'quickfix' then
      buffers[#buffers + 1] = self:new_buffer(b, #buffers + 1)
      self.bufpos2nr[#buffers] = b
    end
  end

  return buffers
end
require('lualine').setup(config)
