local ok_dial, _ = pcall(require, "dial.config")
if not ok_dial then
  return
end
local augend = require "dial.augend"

local logical_alias = augend.constant.new({
  elements = { "&&", "||" },
  word = false,
  cyclic = true,
})

local ordinal_numbers = augend.constant.new({
  -- elements through which we cycle. When we increment, we go down
  -- On decrement we go up
  elements = {
    "first",
    "second",
    "third",
    "fourth",
    "fifth",
    "sixth",
    "seventh",
    "eighth",
    "ninth",
    "tenth",
  },
  -- if true, it only matches strings with word boundary. firstDate wouldn't work for example
  word = false,
  -- do we cycle back and forth (tenth to first on increment, first to tenth on decrement).
  -- Otherwise nothing will happen when there are no further values
  cyclic = true,
})

local weekdays = augend.constant.new({
  elements = {
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday",
  },
  word = true,
  cyclic = true,
})

local months = augend.constant.new({
  elements = {
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
  },
  word = true,
  cyclic = true,
})

local capitalized_boolean = augend.constant.new({
  elements = {
    "True",
    "False",
  },
  word = true,
  cyclic = true,
})
local opts = {
  dials_by_ft = {
    css = "css",
    javascript = "typescript",
    javascriptreact = "typescript",
    json = "json",
    lua = "lua",
    markdown = "markdown",
    python = "python",
    sass = "css",
    scss = "css",
    typescript = "typescript",
    typescriptreact = "typescript",
    php = "typescript",
    blade = "typescript",
    java = "typescript",
    cpp = "typescript",
  },
  groups = {
    default = {
      augend.integer.alias.decimal, -- nonnegative decimal number (0, 1, 2, 3, ...)
      augend.integer.alias.hex, -- nonnegative hex number  (0x01, 0x1a1f, etc.)
      augend.date.alias["%Y/%m/%d"], -- date (2022/02/19, etc.)
    },
    php = {
      augend.integer.alias.decimal, -- nonnegative and negative decimal number
      augend.constant.alias.bool, -- boolean value (true <-> false)
      logical_alias,
      augend.constant.new({ elements = { "public", "private", "protected" } }),
      ordinal_numbers,
      weekdays,
      months,
    },
    typescript = {
      augend.integer.alias.decimal, -- nonnegative and negative decimal number
      augend.constant.alias.bool, -- boolean value (true <-> false)
      logical_alias,
      augend.constant.new({ elements = { "let", "const" } }),
      ordinal_numbers,
      weekdays,
      months,
    },
    css = {
      augend.integer.alias.decimal, -- nonnegative and negative decimal number
      augend.hexcolor.new({
        case = "lower",
      }),
      augend.hexcolor.new({
        case = "upper",
      }),
    },
    markdown = {
      augend.misc.alias.markdown_header,
      ordinal_numbers,
      weekdays,
      months,
    },
    json = {
      augend.integer.alias.decimal, -- nonnegative and negative decimal number
      augend.semver.alias.semver, -- versioning (v1.1.2)
    },
    lua = {
      augend.integer.alias.decimal, -- nonnegative and negative decimal number
      augend.constant.alias.bool, -- boolean value (true <-> false)
      augend.constant.new({
        elements = { "and", "or" },
        word = true, -- if false, "sand" is incremented into "sor", "doctor" into "doctand", etc.
        cyclic = true, -- "or" is incremented into "and".
      }),
      ordinal_numbers,
      weekdays,
      months,
    },
    python = {
      augend.integer.alias.decimal, -- nonnegative and negative decimal number
      capitalized_boolean,
      logical_alias,
      ordinal_numbers,
      weekdays,
      months,
    },
  },
}
require("dial.config").augends:register_group(opts.groups)
vim.g.dials_by_ft = opts.dials_by_ft

---@param increment boolean
---@param g? boolean
local function dial(increment, g)
  local mode = vim.fn.mode(true)
  -- Use visual commands for VISUAL 'v', VISUAL LINE 'V' and VISUAL BLOCK '\22'
  local is_visual = mode == "v" or mode == "V" or mode == "\22"
  local func = (increment and "inc" or "dec") .. (g and "_g" or "_") .. (is_visual and "visual" or "normal")
  local group = vim.g.dials_by_ft[vim.bo.filetype] or "default"
  return require("dial.map")[func](group)
end
vim.keymap.set({ "n", "v" }, "<C-a>", function()
  return dial(true)
end, { expr = true, desc = "Increment" })
vim.keymap.set({ "n", "v" }, "<C-x>", function()
  return dial(false)
end, { expr = true, desc = "Increment" })

vim.keymap.set({ "n", "v" }, "g<C-a>", function()
  return dial(true, true)
end, { expr = true, desc = "Increment" })
vim.keymap.set({ "n", "v" }, "g<C-x>", function()
  return dial(false, true)
end, { expr = true, desc = "Increment" })
