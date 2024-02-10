return {

  "rebelot/heirline.nvim",
  opts = function(_, opts)
    local utils = require("heirline.utils")
    local conditions = require("heirline.conditions")
    local C = require("astronvim.utils.status.env").fallback_colors
    local get_hlgroup = require("astronvim.utils").get_hlgroup
    local Normal = get_hlgroup("Normal", { fg = C.fg, bg = C.bg })
    local GitSignsAdd = get_hlgroup("GitSignsAdd", { fg = C.green, bg = C.dark_bg })
    local GitSignsChange = get_hlgroup("GitSignsChange", { fg = C.orange, bg = C.dark_bg })

    local FileNameBlock = {
      init = function(self)
        self.filename = vim.api.nvim_buf_get_name(0)
      end,
    }
    -- local FileIcon = {
    --   init = function(self)
    --     local filename = self.filename
    --     local extension = vim.fn.fnamemodify(filename, ":e")
    --     self.icon, self.icon_color =
    --         require("nvim-web-devicons").get_icon_color(filename, extension, { default = true })
    --   end,
    --   provider = function(self)
    --     return self.icon and (self.icon .. " ")
    --   end,
    --   hl = function(self)
    --     return { fg = self.icon_color }
    --   end,
    -- }

    local FileName = {
      provider = function(self)
        -- first, trim the pattern relative to the current directory. For other
        -- options, see :h filename-modifers
        local filename = vim.fn.fnamemodify(self.filename, ":~:.")
        if filename == "" then
          return "[No Name]"
        end
        -- now, if the filename would occupy more than 1/4th of the available
        -- space, we trim the file path to its initials
        -- See Flexible Components section below for dynamic truncation
        if not conditions.width_percent_below(#filename, 0.25) then
          filename = vim.fn.pathshorten(filename)
        end
        return filename
      end,
      hl = { fg = Normal.fg },
    }

    local FileFlags = {
      {
        condition = function()
          return vim.bo.modified
        end,
        provider = "[+]",
        hl = { fg = GitSignsAdd.fg },
      },
      {
        condition = function()
          return not vim.bo.modifiable or vim.bo.readonly
        end,
        provider = "",
        hl = { fg = GitSignsChange.fg },
      },
    }

    local FileNameModifer = {
      hl = function()
        if vim.bo.modified then
          -- use `force` because we need to override the child's hl foreground
          return { fg = "cyan", bold = true, force = true }
        end
      end,
    }

    -- let's add the children to our FileNameBlock component
    FileNameBlock = utils.insert(
      FileNameBlock,
      -- FileIcon,
      utils.insert(FileNameModifer, FileName), -- a new table where FileName is a child of FileNameModifier
      FileFlags,
      { provider = "%<" }                   -- this means that the statusline is cut here when there's not enough space
    )
    table.insert(opts.statusline, 4, FileNameBlock)
    return opts
  end,
}
