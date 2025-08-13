-- Function to get visual selection as a string
local function get_visual_selection()
  local s_start = vim.fn.getpos "'<"
  local s_end = vim.fn.getpos "'>"
  local n_lines = math.abs(s_end[3] - s_start[3]) + 1
  local lines = vim.api.nvim_buf_get_lines(0, s_start[2] - 1, s_end[2], false)
  if #lines == 0 then
    return ""
  end
  lines[1] = string.sub(lines[1], s_start[3])
  if n_lines > 1 then
    lines[n_lines] = string.sub(lines[n_lines], 1, s_end[3])
  end
  return table.concat(lines, "\n")
end

-- Function to escape string for literal regex search (since mini.pick.builtin.grep uses ripgrep, which treats pattern as regex by default)
local function escape_pattern(pattern)
  return pattern:gsub("([%%\\^$().|?*+[{])", "\\%1")
end

local win_config = function()
  local height = math.floor(0.618 * vim.o.lines)
  local width = math.floor(0.618 * vim.o.columns)
  return {
    anchor = "NW",
    height = height,
    width = width,
    row = math.floor(0.5 * (vim.o.lines - height)),
    col = math.floor(0.5 * (vim.o.columns - width)),
  }
end

local add, later = MiniDeps.add, MiniDeps.later
later(function()
  local extra = require "mini.extra"
  local pick = require "mini.pick"
  local cmd = require("u.utils").cmd_str

  extra.setup()
  pick.setup({
    mappings = {
      -- choose_in_split = "<C-x>",
      -- mark = "<C-m>",
      move_down = "<C-j>",
      move_up = "<C-k>",
    },
    options = { use_cache = true },
    window = { config = win_config },
  })

  vim.keymap.set("n", "<leader>ff", cmd "Pick files")
  vim.keymap.set("n", "<c-p>", cmd "Pick files")
  vim.keymap.set("n", "<leader>fh", cmd "Pick help")
  vim.keymap.set("n", "<leader>fH", cmd "Pick hl_groups")
  vim.keymap.set("n", "<leader>fr", cmd "Pick resume")
  vim.keymap.set("n", "<leader>fk", cmd "Pick keymaps")
  vim.keymap.set("n", "<leader>fK", cmd "Pick keymaps scope='buf'")
  vim.keymap.set("n", "<leader>fq", cmd "Pick list scope='quickfix'")
  vim.keymap.set("n", "<leader>fQ", cmd "Pick list scope='location'")
  vim.keymap.set("n", "<leader>fd", function()
    MiniPick.builtin.files({ tool = "git" }, { source = { cwd = vim.fn.expand "~/Documents/dotfiles" } })
  end, { desc = "dotfiles" })
  vim.keymap.set("n", "<leader>fn", function()
    MiniPick.builtin.files(nil, { source = { cwd = vim.fn.stdpath "config" } })
  end, { desc = "dotfiles" })
  vim.keymap.set("n", "<leader>fs", cmd "Pick grep_live")
  vim.keymap.set("v", "<leader>fs", function()
    local pattern = get_visual_selection()
    if pattern == "" then
      return
    end
    pattern = escape_pattern(pattern)
    require("mini.pick").builtin.grep({ pattern = pattern })
  end, { desc = "Grep visual selection" })
  vim.keymap.set("n", "<leader>fw", cmd "Pick grep pattern='<cword>'")

  vim.keymap.set("n", "<leader>lR", cmd "Pick lsp scope='references'")
  vim.keymap.set("n", "<leader>ld", cmd "Pick lsp scope='definition'")
  vim.keymap.set("n", "<leader>li", cmd "Pick lsp scope='implementation'")
  vim.keymap.set("n", "<leader>lt", cmd "Pick lsp scope='type_definition'")
  vim.keymap.set("n", "<leader>lw", cmd "Pick lsp scope='document_symbol'")
  vim.keymap.set("n", "<leader>lW", cmd "Pick lsp scope='workspace_symbol'")

  vim.keymap.set("n", "<leader>fg", cmd "Pick files tool=git")
  vim.keymap.set("n", "<leader>gb", cmd "Pick git_branches")
  vim.keymap.set("n", "<leader>gc", cmd "Pick git_commits path=%")
  vim.keymap.set("n", "<leader>gC", cmd "Pick git_commits")

  vim.keymap.set("n", "<leader>fD", cmd "Pick diagnostic")
  vim.keymap.set("n", "<leader>fb", cmd "Pick buffers")
  vim.keymap.set("n", "<leader>fB", cmd "Pick buf_lines")
end)
