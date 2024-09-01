---@type LazySpec
return {
  'brenoprata10/nvim-highlight-colors',
  event = 'BufReadPost',
  lazy = true,
  opts = {
    ---Render style
    ---@type 'background'|'foreground'|'virtual'
    render = 'virtual',
    enable_tailwind = true,
  },
}
