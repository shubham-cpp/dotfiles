return {
  'nvim-lualine/lualine.nvim',
  event = 'BufReadPost',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  config = function()
    local colors = require('onenord.colors').load()
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
        -- return next(vim.lsp.buf_get_clients()) ~= nil
        return next(vim.lsp.get_active_clients({ bufnr = 0 })) ~= nil
      end,
    }
    local function servers_attached()
      local msg = 'None'
      local clients = vim.tbl_map(
        function(client)
          return client.name
        end,
        -- vim.lsp.get_clients({ bufnr = 0 }))
        vim.lsp.get_active_clients({ bufnr = 0 })
      )
      if vim.tbl_isempty(clients) then
        return msg
      end
      return vim.fn.join(vim.tbl_flatten(clients), ',')
    end
    local config = {
      options = {
        globalstatus = true,
        component_separators = '',
        section_separators = '',
        theme = {
          normal = { c = { fg = colors.active, bg = colors.bg, blend = 100 } },
          inactive = { c = { fg = colors.active, bg = colors.bg } },
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
      extensions = { 'quickfix', 'lazy', 'neo-tree', 'toggleterm' },
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
        active = { fg = colors.bg, bg = colors.light_green }, -- Color for active tab.
        inactive = { fg = colors.gray }, -- Color for inactive tab.
      },
    })
    ins_left_tab({
      function()
        return '%='
      end,
    })
    ins_right_tab({
      'tabs',
      tabs_color = {
        -- Same values as the general color option can be used here.
        active = { fg = colors.bg, bg = colors.light_green }, -- Color for active tab.
        inactive = { fg = colors.gray }, -- Color for inactive tab.
      },
    })
    ins_left({
      function()
        return '▊'
      end,
      color = { fg = colors.blue },
      left_padding = 0,
    })
    ins_left({
      'filetype',
      icon_only = true,
      condition = conditions.buffer_not_empty,
      color = { fg = colors.light_purple },
    })
    ins_left({
      'filename',
      path = 1,
      -- shorting_target = 40,
      condition = conditions.buffer_not_empty,
      color = { fg = colors.light_purple },
    })
    ins_left({
      'branch',
      icon = '',
      color = { fg = colors.light_green },
    })
    ins_left({
      'diff',
      colored = true,
      symbols = { added = ' ', modified = '󰇊 ', removed = ' ' },
      -- symbols = { added = ' ', modified = ' ', removed = ' ' },
      diff_color = {
        added = { fg = colors.green },
        modified = { fg = colors.orange },
        removed = { fg = colors.red },
      },
      cond = conditions.hide_in_width,
    })
    ins_left({
      'diagnostics',
      sources = { 'nvim_lsp' },
      diagnostics_color = {
        color_error = { fg = colors.red },
        color_warn = { fg = colors.yellow },
        color_info = { fg = colors.cyan },
      },
    })
    -- Insert mid section. You can make any number of sections in neovim :)
    ins_left({
      function()
        return '%='
      end,
    })
    ins_right({
      servers_attached,
      icon = ' LSPs:',
      color = { fg = colors.fg, gui = 'bold' },
      cond = conditions.lsp_active,
    })
    ins_right({ 'progress', color = { fg = colors.light_purple } })
    ins_right({ 'location', color = { fg = colors.cyan } })
    ins_right({
      'searchcount',
      icon = '',
      color = { fg = colors.yellow },
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
      color = { fg = colors.fg },
    })

    require('lualine').setup(config)
  end,
}
