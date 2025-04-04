---@diagnostic disable: missing-fields
---@type LazySpec
return {
  'rebelot/heirline.nvim',
  event = 'VeryLazy',
  dependencies = { 'Zeioth/heirline-components.nvim' },
  enabled = false,
  config = function()
    vim.opt.showtabline = 2

    local heirline = require 'heirline'
    local lib = require 'heirline-components.all'
    local hl = require 'heirline-components.core.hl'
    local utils = require 'heirline.utils'
    local conditions = require 'heirline.conditions'

    local FileNameBlock = {
      -- let's first set up some attributes needed by this component and its children
      init = function(self)
        self.filename = vim.api.nvim_buf_get_name(0)
      end,
    }

    local FileName = {
      provider = function(self)
        -- first, trim the pattern relative to the current directory. For other
        -- options, see :h filename-modifers
        local filename = vim.fn.fnamemodify(self.filename, ':.')
        if filename == '' then
          return '[No Name]'
        end
        -- now, if the filename would occupy more than 1/4th of the available
        -- space, we trim the file path to its initials
        -- See Flexible Components section below for dynamic truncation
        if not conditions.width_percent_below(#filename, 0.25) then
          filename = vim.fn.pathshorten(filename)
        end
        return filename
      end,
      hl = { fg = utils.get_highlight('Directory').fg },
    }

    local FileFlags = {
      {
        condition = function()
          return vim.bo.modified
        end,
        provider = '[+]',
        hl = { fg = '#73d936' },
      },
      {
        condition = function()
          return not vim.bo.modifiable or vim.bo.readonly
        end,
        provider = '',
        hl = { fg = '#cc8c3c' },
      },
    }

    -- Now, let's say that we want the filename color to change if the buffer is
    -- modified. Of course, we could do that directly using the FileName.hl field,
    -- but we'll see how easy it is to alter existing components using a "modifier"
    -- component

    local FileNameModifer = {
      hl = function()
        if vim.bo.modified then
          -- use `force` because we need to override the child's hl foreground
          return { fg = 'cyan', bold = true, force = true }
        end
      end,
    }

    -- let's add the children to our FileNameBlock component
    FileNameBlock = utils.insert(
      FileNameBlock,
      -- FileIcon,
      utils.insert(FileNameModifer, FileName), -- a new table where FileName is a child of FileNameModifier
      FileFlags,
      { provider = '%<' } -- this means that the statusline is cut here when there's not enough space
    )
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
              -- gruber-darker
              -- local fg = '#f2e9e1'
              -- local bg = '#806a4d'
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
      },
      statusline = { -- UI statusbar
        hl = { fg = 'fg', bg = 'bg' },
        lib.component.mode(),
        lib.component.git_branch({
          on_click = {
            name = 'heirline_branch',
            callback = function()
              local ok_fzf, fzf = pcall(require, 'fzf-lua')
              local ok_telescope, builtin = pcall(require, 'telescope.builtin')
              local ok_snacks, picker = pcall(require, 'snacks.picker')
              if ok_telescope then
                builtin.git_branches({ use_file_path = true })
              elseif ok_fzf then
                fzf.git_branches()
              elseif ok_snacks then
                picker.git_branches()
              end
            end,
          },
        }),
        FileNameBlock,
        lib.component.file_info(),
        lib.component.git_diff({
          on_click = {
            name = 'heirline_git',
            callback = function()
              local ok_fzf, fzf = pcall(require, 'fzf-lua')
              local ok_telescope, builtin = pcall(require, 'telescope.builtin')
              local ok_snacks, picker = pcall(require, 'snacks.picker')
              if ok_telescope then
                builtin.git_status({ use_file_path = true })
              elseif ok_fzf then
                fzf.git_status()
              elseif ok_snacks then
                picker.git_status()
              end
            end,
          },
        }),
        lib.component.diagnostics({
          on_click = {
            name = 'heirline_diagnostic',
            callback = function()
              local ok_fzf, fzf = pcall(require, 'fzf-lua')
              local ok_telescope, builtin = pcall(require, 'telescope.builtin')
              local ok_snacks, picker = pcall(require, 'snacks.picker')
              local ok_mini, mini_pick = pcall(require, 'mini.pick')

              if ok_snacks then
                picker.diagnostics()
              elseif ok_fzf then
                fzf.diagnostics()
              elseif ok_telescope then
                builtin.diagnostics()
              elseif ok_mini then
                mini_pick.pickers.diagnostic()
              end
            end,
          },
        }),
        lib.component.fill(),
        lib.component.fill(),
        lib.component.lsp(),
        lib.component.nav(),
        lib.component.mode({ surround = { separator = 'right' } }),
      },
    })
  end,
}
