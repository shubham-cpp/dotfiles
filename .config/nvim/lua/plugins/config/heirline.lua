---@diagnostic disable: missing-fields
local heirline = require 'heirline'
local lib = require 'heirline-components.all'
local hl = require 'heirline-components.core.hl'
-- Setup
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
    lib.component.tabline_buffers(),
    lib.component.fill({ hl = { bg = 'tabline_bg' } }),
    lib.component.tabline_tabpages(),
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
    lib.component.cmd_info(),
    lib.component.fill(),
    lib.component.lsp(),
    lib.component.compiler_state(),
    lib.component.virtual_env(),
    lib.component.nav(),
    lib.component.mode({ surround = { separator = 'right' } }),
  },
})
