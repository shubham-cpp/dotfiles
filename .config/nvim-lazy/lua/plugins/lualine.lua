---@type LazySpec
return {
  {
    "nvim-lualine/lualine.nvim",
    optional = true,
    opts = {
      inactive_sections = { lualine_b = { "branch" } },
      extensions = { "quickfix", "toggleterm", "trouble" },
    },
  },
  {
    "nvim-lualine/lualine.nvim",
    optional = true,
    opts = function(_, opts)
      if next(opts) and next(opts.sections) and next(opts.sections.lualine_a) then
        opts.sections.lualine_a = {
          "mode",
          fmt = function(str)
            return str:sub(1, 1)
          end,
        }
      end

      if next(opts) and next(opts.sections) and next(opts.sections.lualine_y) then
        opts.sections.lualine_y = vim.tbl_deep_extend("force", opts.sections.lualine_y, {
          { "lsp_status", ignore_lsp = { "null_ls", "copilot" } },
          { "progress", separator = " ", padding = { left = 1, right = 0 } },
          { "location", padding = { left = 0, right = 1 } },
        })
      end
      opts.sections.lualine_z = {}
    end,
  },
}
