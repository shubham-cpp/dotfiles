local inactive = {
  black = '#000000',
  white = '#ffffff',
  fg = '#696969',
  bg_1 = '#181A1F',
  bg_2 = '#202728',
  index = '#61afef',
}

local active = vim.tbl_extend('force', inactive, {
  fg = '#abb2bf',
  bg_2 = '#282c34',
  index = '#d19a66',
})

local render = function(f)
  f.add '  '
  f.make_tabs(function(info)
    local colors = info.current and active or inactive

    f.add({
      ' ' .. info.index .. ' ',
      fg = colors.index,
      bg = colors.bg_1,
    })

    f.set_colors({ fg = colors.fg, bg = colors.bg_2 })

    f.add ' '
    if info.filename then
      local filename = vim.fn.bufname(info.buf)
      filename = filename:gsub(os.getenv 'HOME', '~')
      f.add(info.modified and '+')
      f.add(filename)
      f.add({
        ' ' .. f.icon(info.filename),
        fg = info.current and f.icon_color(info.filename) or nil,
      })
    else
      f.add(info.modified and '[+]' or '[-]')
    end
    f.add ' '
    f.add({ ' ', bg = colors.black })
  end)

  f.add_spacer()

  local errors = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
  local warnings = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })

  f.add({ '  ' .. errors, fg = '#e86671' })
  f.add({ '  ' .. warnings, fg = '#e5c07b' })
  f.add ' '
end

return {
  'rafcamlet/tabline-framework.nvim',
  enabled = true,
  event = 'BufRead',
  config = function()
    local tf = require 'tabline_framework'
    -- local render = require 'tabline_framework.examples.fancy_indexes'
    tf.setup({
      render = render,
      -- hl = { fg = '#abb2bf', bg = '#181A1F' },
      -- hl_sel = { fg = '#abb2bf', bg = '#282c34' },
      -- hl_fill = { fg = '#ffffff', bg = '#000000' },
    })
  end,
}
