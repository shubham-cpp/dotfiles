local jump2d = require "mini.jump2d"

jump2d.setup({
  labels = "asdfghjklqwertyuiopzxcvbnm",
  view = {
    dim = true,
    n_steps_ahead = 1,
  },
  allowed_windows = {
    current = true,
    not_current = true,
  },
  mappings = {
    start_jumping = "",
  },
})

-- local function make_treesitter_spotter()
--   local win_id = vim.api.nvim_get_current_win()
--   local bufnr = vim.api.nvim_win_get_buf(win_id)
--   local parser_ok, parser = pcall(vim.treesitter.get_parser, bufnr)
--   if not parser_ok or parser == nil then
--     return function()
--       return {}
--     end
--   end
--
--   local parse_ok, trees = pcall(parser.parse, parser)
--   trees = parse_ok and trees or {}
--
--   local spots_by_line = {}
--   local seen = {}
--
--   local function add_spot(node)
--     local start_row, start_col = node:range()
--     local line_num = start_row + 1
--     local column = start_col + 1
--     local key = line_num .. ":" .. column
--
--     if not seen[key] then
--       seen[key] = true
--       spots_by_line[line_num] = spots_by_line[line_num] or {}
--       table.insert(spots_by_line[line_num], column)
--     end
--   end
--
--   local function traverse(node)
--     if node:named() then
--       add_spot(node)
--     end
--
--     for child in node:iter_children() do
--       traverse(child)
--     end
--   end
--
--   for _, tree in ipairs(trees) do
--     traverse(tree:root())
--   end
--
--   for _, columns in pairs(spots_by_line) do
--     table.sort(columns)
--   end
--
--   local function treesitter_spotter(line_num, args)
--     if args.win_id ~= win_id then
--       return {}
--     end
--
--     return spots_by_line[line_num] or {}
--   end
--
--   return treesitter_spotter
-- end

vim.keymap.set({ "n", "x", "o" }, "S", "<Cmd>lua MiniJump2d.start(MiniJump2d.builtin_opts.query)<CR>", {
  desc = "Jump2d query",
})

-- vim.keymap.set({ "n", "x", "o" }, "gw", "<Cmd>lua MiniJump2d.start(MiniJump2d.builtin_opts.word_start)<CR>", {
--   desc = "Jump2d word",
-- })

-- vim.keymap.set({ "n", "o" }, "S", function()
--   MiniJump2d.start({
--     spotter = make_treesitter_spotter(),
--     view = { dim = false, n_steps_ahead = 3 },
--     allowed_windows = { not_current = false },
--     allowed_lines = { blank = false, fold = false },
--     labels = "abcdefghijklmnopqrstuvwxyz",
--     hl_group = "Search",
--   })
-- end, { desc = "Jump2d Treesitter file nodes" })
