---@diagnostic disable: missing-fields
local heirline = require 'heirline'
local lib = require 'heirline-components.all'
local hl = require 'heirline-components.core.hl'
-- Setup
vim.opt.laststatus = 3
lib.init.subscribe_to_events()
heirline.load_colors(lib.hl.get_colors())
heirline.setup({
  opts = {
    disable_winbar_cb = function(args) -- We do this to avoid showing it on the greeter.
      local is_disabled = not require('heirline-components.buffer').is_valid(args.buf)
        or lib.condition.buffer_matches({
          buftype = { 'terminal', 'prompt', 'nofile', 'help', 'quickfix' },
          filetype = { 'NvimTree', 'neo%-tree', 'dashboard', 'Outline', 'aerial' },
        }, args.buf)
      return is_disabled
    end,
  },
  tabline = { -- UI upper bar
    lib.component.tabline_conditional_padding(),
    lib.component.tabline_buffers({
      hl = function(self)
        local tab_type = self.tab_type
        if self._show_picker and self.tab_type ~= 'buffer_active' then
          tab_type = 'buffer_visible'
        end
        if tab_type == 'buffer_active' then
          -- tokyonight
          -- local fg = '#7ca1f2'
          -- local bg = '#393d56'
          -- rose-pine
          local fg = '#f2e9e1'
          local bg = '#214655'
          return vim.tbl_extend('force', hl.get_attributes(tab_type), { fg = fg, bg = bg })
        else
          return hl.get_attributes(tab_type)
        end
      end,
    }),
    lib.component.fill(),
    lib.component.tabline_tabpages({}),
  },
  winbar = { -- UI breadcrumbs bar
    init = function(self)
      self.bufnr = vim.api.nvim_get_current_buf()
    end,
    -- Regular winbar
    -- lib.component.neotree(),
    -- lib.component.compiler_play(),
    -- lib.component.fill(),
    {
      hl = hl.get_attributes('winbar', true),
      condition = function()
        return require('nvim-navic').is_available()
      end,
      provider = function()
        return require('nvim-navic').get_location({ highlight = true })
      end,
      update = 'CursorMoved',
    },
    -- lib.component.fill(),
    -- lib.component.compiler_redo(),
    -- lib.component.aerial(),
  },
  statuscolumn = { -- UI left column
    init = function(self)
      self.bufnr = vim.api.nvim_get_current_buf()
    end,
    lib.component.foldcolumn(),
    lib.component.numbercolumn(),
    lib.component.signcolumn(),
  } or nil,
  statusline = { -- UI statusbar
    hl = { fg = 'fg', bg = 'bg' },
    lib.component.mode(),
    lib.component.git_branch(),
    lib.component.file_info(),
    lib.component.git_diff(),
    lib.component.diagnostics(),
    lib.component.fill(),
    lib.component.fill(),
    lib.component.lsp(),
    lib.component.nav(),
    lib.component.mode({ surround = { separator = 'right' } }),
  },
})
