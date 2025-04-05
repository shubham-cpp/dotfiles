---@type LazySpec
return {
  "nvim-lualine/lualine.nvim",
  opts = function(_, opts)
    table.insert(opts.sections.lualine_x, {
      function()
        local ok_statusline, statusline = pcall(require, "hydra.statusline")
        if not ok_statusline then
          return ""
        end
        vim.print(vim.inspect(statusline.get_name()))
        return string.format("[%s] - %s", statusline.get_name(), statusline.get_hint())
      end,
      cond = function()
        local ok_statusline, statusline = pcall(require, "hydra.statusline")
        return ok_statusline and statusline.is_active()
      end,
    })
  end,
}
