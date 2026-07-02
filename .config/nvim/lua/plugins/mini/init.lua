local u = require "config.utils"

local map = vim.keymap.set

vim.pack.add({
  u.gh "nvim-mini/mini.nvim",
  u.gh "rafamadriz/friendly-snippets",
})

--- {{{ Mini icons
require("mini.icons").setup({})
require("mini.icons").mock_nvim_web_devicons()
--- }}}

--- {{{ Mini move
require("mini.move").setup({
  mappings = {
    left = "<",
    right = ">",
    down = "J",
    up = "K",

    line_left = "<M-h>",
    line_right = "<M-l>",
    line_down = "<M-j>",
    line_up = "<M-k>",
  },
})
--- }}}

require("mini.extra").setup()

---{{{ Mini hipatterns
local hipatterns = require "mini.hipatterns"
local hi_words = require("mini.extra").gen_highlighter.words

hipatterns.setup({
  highlighters = {
    todo = hi_words({ "TODO", "Todo" }, "MiniHipatternsTodo"),
    hack = hi_words({ "FIXME", "DEBUG", "HACK", "Fixme", "Debug", "Hack" }, "MiniHipatternsFixme"),
    note = hi_words({ "NOTE", "LATER", "Note", "Later" }, "MiniHipatternsNote"),
  },
})
---}}}

require("mini.surround").setup({})

---{{{ Mini operators
require("mini.operators").setup({
  replace = { prefix = "x" },
})
vim.keymap.set("n", "X", "x$", { desc = "Replace to end of line", remap = true })
---}}}

---{{{ Mini bracketed
require("mini.bracketed").setup({
  treesitter = { suffix = "r", options = {} },
})
require("mini.bracketed").register_undo_state()
local put_keys = { "p", "P" }
for _, lhs in ipairs(put_keys) do
  local rhs = 'v:lua.MiniBracketed.register_put_region("' .. lhs .. '")'
  vim.keymap.set("n", lhs, rhs, { expr = true })
end
---}}}

--- {{{ Mini notify
local mini_notify = require "mini.notify"
mini_notify.setup({
  lsp_progress = { enable = false },
})
map("n", "<Leader>on", mini_notify.show_history, { desc = "Notification History" })
--- }}}

--- {{{ Mini Git
require("mini.git").setup({})
map("n", "<Leader>gs", function()
  MiniGit.show_at_cursor({ split = "horizontal" })
end, { desc = "Git Show At Cursor" })
--- }}}

--- {{{ Mini diff
local char = "┊"
require("mini.diff").setup({
  view = {
    style = "sign",
    signs = { add = char, change = char, delete = char },
  },
})
--- }}}

--- {{{ Mini Misc
require("mini.misc").setup()
require("mini.misc").setup_restore_cursor()
require("mini.misc").setup_termbg_sync()
vim.keymap.set("n", "<C-w>m", function()
  require("mini.misc").zoom()
end, { desc = "Zoom" })
--- }}}

---{{{ Mini Indentscope
require("mini.indentscope").setup({
  draw = { delay = 50, animation = require("mini.indentscope").gen_animation.none() },
  -- symbol = "🮍",
  symbol = "│",
  options = { try_as_border = true },
})
---}}}
require("mini.align").setup()
---{{{ Mini Cursorword
local cursorword_blocklist = {
  ["and"] = true,
  ["break"] = true,
  ["do"] = true,
  ["else"] = true,
  ["elseif"] = true,
  ["end"] = true,
  ["false"] = true,
  ["for"] = true,
  ["function"] = true,
  ["if"] = true,
  ["in"] = true,
  ["local"] = true,
  ["const"] = true,
  ["null"] = true,
  ["def"] = true,
  ["delete"] = true,
  ["new"] = true,
  ["interface"] = true,
  ["type"] = true,
  ["extends"] = true,
  ["await"] = true,
  ["async"] = true,
  ["nil"] = true,
  ["not"] = true,
  ["or"] = true,
  ["repeat"] = true,
  ["require"] = true,
  ["return"] = true,
  ["then"] = true,
  ["true"] = true,
  ["until"] = true,
  ["while"] = true,
}

vim.api.nvim_create_autocmd("CursorMoved", {
  group = vim.api.nvim_create_augroup("ConfigMiniCursorword", { clear = true }),
  callback = function()
    local words = cursorword_blocklist
    vim.b.minicursorword_disable = words ~= nil and words[vim.fn.expand "<cword>"] == true
  end,
})
require("mini.cursorword").setup()
---}}}

require "plugins.mini.mini-ai"
require "plugins.mini.mini-clue"
require "plugins.mini.mini-files"
require "plugins.mini.mini-pairs"
require "plugins.mini.mini-pick"
require "plugins.mini.mini-starter"
require "plugins.mini.mini-snippets"
require "plugins.mini.mini-statusline"
require "plugins.mini.mini-visits"
