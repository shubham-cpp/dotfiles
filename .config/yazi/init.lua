function Status:name()
  local h = cx.active.current.hovered
  if not h then
    return ui.Span ""
  end

  local linked = ""
  if h.link_to ~= nil then
    linked = " -> " .. tostring(h.link_to)
  end
  return ui.Span(" " .. h.name .. linked)
end

require("bookmarks"):setup({
  last_directory = { enable = true, persist = true },
  persist = "vim",
})
