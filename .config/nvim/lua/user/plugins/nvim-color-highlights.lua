---@type LazySpec
return {
  "brenoprata10/nvim-highlight-colors",
  event = "BufReadPost",
  enabled = true,
  lazy = true,
  opts = {
    ---Render style
    ---@type 'background'|'foreground'|'virtual'
    render = "background",
    enable_tailwind = true,
  },
}
