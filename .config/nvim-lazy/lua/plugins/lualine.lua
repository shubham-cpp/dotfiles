---@type LazySpec
return {
  {
    "nvim-lualine/lualine.nvim",
    optional = true,
    opts = {
      inactive_sections = {
        lualine_b = { "branch" },
      },
    },
  },
  {
    "nvim-lualine/lualine.nvim",
    optional = true,
    opts = function(_, opts)
      if next(opts) and next(opts.sections) and next(opts.sections.lualine_a) then
        opts.sections.lualine_a = {
          {
            "mode",
            fmt = function(str)
              return str:sub(1, 1)
            end,
          },
        }
      end
    end,
  },
}
