return {
  'nvim-lualine/lualine.nvim',
  event = 'BufReadPost',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  opts = {},
  config = function()
    require('lualine.components.buffers').buffers = function(self)
      local buffers = {}
      self.bufpos2nr = {}
      -- for b = 1, vim.fn.bufnr('$') do
      --   if vim.fn.buflisted(b) ~= 0 and vim.api.nvim_buf_get_option(b, 'buftype') ~= 'quickfix' then
      --     buffers[#buffers + 1] = self:new_buffer(b, #buffers + 1)
      --     self.bufpos2nr[#buffers] = b
      --   end
      -- end
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
    require('lualine').setup({
      options = {
        -- theme = 'ayu_mirage',
        component_separators = '|',
        section_separators = { left = '', right = '' },
      },
      sections = {
        lualine_a = {
          {
            'mode',
            fmt = function(str)
              return str:sub(1, 1)
            end,
            separator = { left = '' },
          },
        },
        lualine_b = {
          { 'filename', path = conditions.hide_in_width() and 0 or 1, cond = conditions.hide_in_width },
          { 'branch', icon = ' ' },
          {
            'diff',
            symbols = { added = ' ', modified = ' ', removed = ' ' },
            cond = conditions.hide_in_width,
          },
          { 'diagnostics', cond = conditions.hide_in_width },
        },
        lualine_c = {
          { 'searchcount', icon = '', cond = conditions.hide_in_width },
          {
            servers_attached,
            icon = ' LSPs:',
            color = { fg = '#ffffff', gui = 'bold' },
            cond = conditions.lsp_active,
          },
        },
        lualine_x = {
          'fileformat',
          { 'filetype', icon_only = vim.fn.winwidth '%' < 75, cond = conditions.hide_in_width },
        },
        lualine_y = { { 'progress', cond = conditions.hide_in_width } },
        lualine_z = { { 'location', cond = conditions.hide_in_width, separator = { right = '' } } },
      },
      inactive_sections = {
        -- lualine_a = { { 'buffers', cond = conditions.hide_in_width } },
        lualine_b = {
          { 'filename', path = conditions.hide_in_width() and 1 or 4, cond = conditions.hide_in_width },
        },
        lualine_c = {
          { 'searchcount', cond = conditions.hide_in_width },
          {
            servers_attached,
            icon = ' LSPs:',
            color = { fg = '#ffffff', gui = 'bold' },
            cond = conditions.lsp_active,
          },
        },
      },
      tabline = {
        lualine_a = {
          'buffers',
          -- 'tabs',
          -- { 'windows', mode = 2, disabled_buftypes = { 'quickfix', 'prompt', 'help' } },
        },
        lualine_z = {
          'tabs',
          -- { 'windows', mode = 2, disabled_buftypes = { 'quickfix', 'prompt', 'help' } },
        },
      },
      extensions = { 'neo-tree', 'toggleterm', 'quickfix' },
    })
  end,
}
