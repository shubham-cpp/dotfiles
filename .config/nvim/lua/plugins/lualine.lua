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
    return next(vim.lsp.get_clients({ bufnr = 0 })) ~= nil
  end,
}
local function servers_attached()
  local msg = 'None'
  local clients = vim.tbl_map(function(client)
    return client.name
  end, vim.lsp.get_clients({ bufnr = 0 }))
  if vim.tbl_isempty(clients) then
    return msg
  end
  return vim.fn.join(vim.tbl_flatten(clients), ',')
end

return {
  'nvim-lualine/lualine.nvim',
  event = 'VeryLazy',
  dependencies = {
    'nvim-tree/nvim-web-devicons',
  },
  opts = function()
    local config = {
      options = {
        globalstatus = true,
        component_separators = '',
        section_separators = '',
        theme = {
          normal = { c = { bg = '#2a2a37', fg = '#353B49', blend = 100 } },
          inactive = { c = { bg = '#2a2a37', fg = '#353B49' } },
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

      winbar = {
        lualine_c = {
          {
            function()
              return require('nvim-navic').get_location()
            end,
            cond = function()
              return package.loaded['nvim-navic'] and require('nvim-navic').is_available()
            end,
          },
          {
            'filename',
            path = 1,
            color = { fg = '#c8c093', bg = '#1E1E2E', gui = 'italic,bold' },
            cond = function()
              local is_ok = package.loaded['nvim-navic'] and require('nvim-navic').is_available()
              return not is_ok
            end,
          },
        },
      },
      inactive_winbar = {
        lualine_c = {
          {
            'filename',
            path = 1,
            color = { fg = '#c8c093', bg = '#1E1E2E', gui = 'italic,bold' },
          },
        },
      },
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
        active = { fg = '#010748', bg = '#a9a9a9' }, -- Color for active tab.
        inactive = { fg = '#c8c093', bg = '#16161d' }, -- Color for inactive tab.
      },
    })
    ins_left_tab '%='
    ins_right_tab({
      'tabs',
      tabs_color = {
        -- Same values as the general color option can be used here.
        active = { fg = '#010748', bg = '#a9a9a9' }, -- Color for active tab.
        inactive = { fg = '#c8c093', bg = '#16161d' }, -- Color for inactive tab.
      },
    })
    ins_left({
      'filetype',
      icon_only = true,
      condition = conditions.buffer_not_empty,
      left_padding = 0,
      -- color = { fg = vim.g.my_colors.purple },
    })
    ins_left({
      'filename',
      path = 1,
      -- shorting_target = 40,
      condition = conditions.buffer_not_empty,
      color = { fg = '#8992a7' },
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
      -- diff_color = {
      --   added = { fg = vim.g.my_colors.git.added },
      --   modified = { fg = vim.g.my_colors.git.modified },
      --   removed = { fg = vim.g.my_colors.git.removed },
      -- },
      cond = conditions.hide_in_width,
    })
    ins_left({
      'diagnostics',
      sources = { 'nvim_lsp' },
      -- diagnostics_color = {
      --   color_error = { fg = vim.g.my_colors.diagnostics.error },
      --   color_warn = { fg = vim.g.my_colors.diagnostics.warn },
      --   color_info = { fg = vim.g.my_colors.diagnostics.info },
      -- },
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
      color = { fg = '#DCA561' },
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
    return config
  end,
  config = function(_, opts)
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
    require('lualine').setup(opts)
  end,
}
