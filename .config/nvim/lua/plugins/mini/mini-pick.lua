local map = vim.keymap.set

--- {{{ Mini pick
local win_config = function()
  local height = math.floor(0.618 * vim.o.lines)
  local width = math.floor(0.618 * vim.o.columns)
  return {
    border = "solid",
    anchor = "NW",
    height = height,
    width = width,
    row = math.floor(0.5 * (vim.o.lines - height)),
    col = math.floor(0.5 * (vim.o.columns - width)),
  }
end
local choose_all = function()
  local mappings = MiniPick.get_picker_opts().mappings
  vim.api.nvim_input(mappings.mark_all .. mappings.choose_marked)
end
require("mini.pick").setup({
  mappings = {
    move_down = "<C-j>",
    move_up = "<C-k>",

    choose_all = { char = "<C-q>", func = choose_all },
  },
  window = { config = win_config, prompt_caret = "┋", prompt_prefix = "󰄾 " },
})

local pick_config = require "config.pick"

MiniPick.registry.files = function(local_opts)
  local_opts = local_opts or {}
  local opts = pick_config.source_opts({ cwd = local_opts.cwd })
  local_opts.cwd = nil
  return MiniPick.builtin.files(local_opts, opts)
end
MiniPick.registry.git_files = function(local_opts)
  return require("mini.extra").pickers.git_files(local_opts, pick_config.source_opts())
end
MiniPick.registry.oldfiles = function(local_opts)
  return require("mini.extra").pickers.oldfiles(local_opts, pick_config.source_opts())
end
MiniPick.registry.buffers = function(local_opts)
  return MiniPick.builtin.buffers(local_opts, pick_config.source_opts())
end
-- MiniPick.registry.grep_live = function(local_opts)
--   local_opts = local_opts or {}
--   local opts = pick_config.source_opts({ cwd = local_opts.cwd })
--   local_opts.cwd = nil
--   return MiniPick.builtin.grep_live(local_opts, opts)
-- end

local find_nvim_config = string.format("<Cmd>Pick files tool='rg' cwd='%s'<cr>", vim.fn.stdpath "config")
local find_dot_config = string.format("<Cmd>Pick files tool='git' cwd='%s'<cr>", vim.fn.expand "~/Documents/dotfiles")

map("n", "<C-p>", "<cmd>Pick files tool='rg'<cr>", { desc = "Pick File" })
map("n", "<Leader>ff", "<cmd>Pick files tool='rg'<cr>", { desc = "Find Files" })

map("n", "<Leader>fw", "<cmd>Pick grep_live tool='rg' pattern='<cword>'<cr>", { desc = "Grep cword" })
map("n", "<Leader>fs", "<cmd>Pick grep_live tool='rg'<cr>", { desc = "Live Grep" })
-- map("n", "<Leader>fS", function()
--   local cwd = vim.fn.expand "%:p:h"
--   if cwd == "" then
--     cwd = vim.uv.cwd()
--   end
--
--   MiniPick.registry.grep_live({ tool = "rg", cwd = cwd })
-- end, { desc = "Live Grep (File Dir)" })
-- map("n", "<Leader>fW", function()
--   local cwd = vim.fn.expand "%:p:h"
--   if cwd == "" then
--     cwd = vim.uv.cwd()
--   end
--
--   MiniPick.registry.grep({ tool = "rg", pattern = "<cword>", cwd = cwd })
-- end, { desc = "Grep cword(cwd)" })

map("n", "<Leader>fb", "<cmd>Pick buffers<cr>", { desc = "Find Buffers" })
map("n", "<Leader>fB", "<cmd>Pick buf_lines scope='current'<cr>", { desc = "Search line Buffers" })
map("n", "<Leader>fn", find_nvim_config, { desc = "Find Neovim" })
map("n", "<Leader>fd", find_dot_config, { desc = "Find Neovim" })
map("n", "<Leader>fh", "<cmd>Pick help<cr>", { desc = "Find Help" })
map("n", "<Leader>fk", "<cmd>Pick keymaps<cr>", { desc = "Find Keymaps" })
map("n", "<Leader>fm", "<cmd>Pick manpages<cr>", { desc = "Find Manpages" })
map("n", "<Leader>fr", "<cmd>Pick resume<cr>", { desc = "Find Resume" })
map(
  "n",
  "<Leader>ft",
  "<cmd>Pick hipatterns highlighters={'todo','fixme','note'}<cr>",
  { desc = "Find TODOs/FIXMEs/etc" }
)
map(
  "n",
  "<Leader>fT",
  "<cmd>Pick hipatterns scope='current' highlighters={'todo','fixme','note'}<cr>",
  { desc = "Find TODOs/FIXMEs (Current)" }
)
map("n", "<Leader>fO", "<cmd>Pick oldfiles current_dir=true<cr>", { desc = "Find Oldfiles(Cwd)" })
map("n", "<Leader>fo", "<cmd>Pick oldfiles<cr>", { desc = "Find Oldfiles(All)" })
map("n", "<Leader>fD", "<cmd>Pick diagnostic<cr>", { desc = "Find Diagnostics" })
map("n", "<Leader>gc", "<cmd>Pick git_commits<cr>", { desc = "Commits" })
map("n", "<Leader>gC", "<cmd>Pick git_commits path='%'<cr>", { desc = "Commits(Buffers)" })
map("n", "<Leader>gb", "<cmd>Pick git_branches<cr>", { desc = "Branches" })
map("n", "<Leader>gf", "<cmd>Pick git_files<cr>", { desc = "Files" })
map("n", "<Leader>gh", "<cmd>Pick git_hunks<cr>", { desc = "Hunks" })
--- }}}
