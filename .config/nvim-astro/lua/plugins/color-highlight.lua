---@type LazySpec
return {
  "brenoprata10/nvim-highlight-colors",
  event = "User AstroFile",
  cmd = "HighlightColors",
  opts = {
    ---Render style
    ---@type 'background'|'foreground'|'virtual'
    render = "virtual",
    enable_tailwind = true,
  },
  dependencies = {
    {
      "AstroNvim/astrocore",
      opts = function(_, opts)
        local maps = opts.mappings
        maps.n["<Leader>uz"] = { function() vim.cmd.HighlightColors "Toggle" end, desc = "Toggle color highlight" }
      end,
    },
  },
  specs = {
    { "NvChad/nvim-colorizer.lua", optional = true, enabled = false },
  },
}
