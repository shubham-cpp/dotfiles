return {
  'nvim-lualine/lualine.nvim',
  event = 'BufReadPost',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  opts = {},
  config = function()
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
        return next(vim.lsp.buf_get_clients()) ~= nil
      end,
    }
    local function servers_attached()
      local msg = 'None'
      local clients = vim.tbl_map(
        function(client) return client.name end,
        -- vim.lsp.get_clients({ bufnr = 0 }))
        vim.lsp.buf_get_clients())
      if vim.tbl_isempty(clients) then
        return msg
      end
      return vim.fn.join(vim.tbl_flatten(clients), ",")
    end
    require('lualine').setup({
      sections = {
        lualine_a = {
          {
            'mode',
            fmt = function(str)
              return str:sub(1, 1)
            end,
          },
        },
        lualine_b = {
          { 'filename', path = conditions.hide_in_width and 0 or 1 },
          { 'branch', icon = ' ' },
          {
            'diff',
            symbols = { added = ' ', modified = ' ', removed = ' ' },
            cond = conditions.hide_in_width,
          },
          'diagnostics',
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
        lualine_x = { 'fileformat', 'filetype' },
        lualine_y = { 'progress' },
        lualine_z = { 'location' },
      },
      inactive_sections = {
        -- lualine_a = { { 'buffers', cond = conditions.hide_in_width } },
        lualine_b = {
          { 'filename', path = conditions.hide_in_width and 1 or 4, cond = conditions.hide_in_width },
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
        lualine_a = {},
        lualine_b = { 'buffers' },
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = { {
          'tabs',
          mode = 2,
          use_mode_colors = false,
          fmt = function(name, context)
            -- Show + if buffer is modified in tab
            local buflist = vim.fn.tabpagebuflist(context.tabnr)
            local winnr = vim.fn.tabpagewinnr(context.tabnr)
            local bufnr = buflist[winnr]
            local mod = vim.fn.getbufvar(bufnr, '&mod')

            return name .. (mod == 1 and ' +' or '')
          end
        } },
      },
      extensions = { 'neo-tree', 'toggleterm', 'quickfix' },
    })
  end,
}
