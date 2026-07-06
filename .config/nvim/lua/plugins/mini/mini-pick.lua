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

local fzy_config = require "config.fzy"

MiniPick.registry.files = function(local_opts)
  local_opts = local_opts or {}

  local cwd = local_opts.cwd
  local hidden = local_opts.hidden == true or local_opts.hidden == "true"
  local tool = local_opts.tool or "rg"

  local opts = fzy_config.source_opts({ cwd = cwd })

  if hidden then
    local command = {
      "rg",
      "--files",
      "--color=never",
      "--hidden",
      "--sort=path",
      "--glob",
      "!.git/",
    }
    if tool == "fd" then
      command = {
        "fd",
        "--type=f",
        "--color=never",
        "--hidden",
        "--exclude",
        ".git",
      }
    end

    return MiniPick.builtin.cli(
      { command = command },
      vim.tbl_deep_extend("force", {
        source = { name = "Files(hidden)" },
      }, opts)
    )
  end

  local_opts.cwd = nil
  return MiniPick.builtin.files(local_opts, opts)
end
MiniPick.registry.git_files = function(local_opts)
  return require("mini.extra").pickers.git_files(local_opts, fzy_config.source_opts())
end
MiniPick.registry.oldfiles = function(local_opts)
  return require("mini.extra").pickers.oldfiles(local_opts, fzy_config.source_opts())
end
MiniPick.registry.buffers = function(local_opts)
  return MiniPick.builtin.buffers(local_opts, fzy_config.source_opts())
end
MiniPick.registry.grep_live = function(local_opts)
  local_opts = local_opts or {}

  local cwd = local_opts.cwd
  if cwd ~= nil then
    cwd = vim.fn.fnamemodify(vim.fn.expand(cwd), ":p")
  end

  local opts = { source = { cwd = cwd } }
  local_opts.cwd = nil

  return MiniPick.builtin.grep_live(local_opts, opts)
end

MiniPick.registry.grep = function(local_opts)
  local_opts = local_opts or {}

  local cwd = local_opts.cwd
  if cwd ~= nil then
    cwd = vim.fn.fnamemodify(vim.fn.expand(cwd), ":p")
  end

  local opts = { source = { cwd = cwd } }
  local_opts.cwd = nil

  return MiniPick.builtin.grep(local_opts, opts)
end

local find_nvim_config = string.format('<Cmd>Pick files cwd="%s"<cr>', vim.fn.stdpath "config")
local find_dot_config = string.format('<Cmd>Pick files cwd="%s" hidden=true<cr>', vim.fn.expand "~/Documents/dotfiles")

map("n", "<C-p>", '<cmd>Pick files tool="rg"<cr>', { desc = "Pick File" })
map("n", "<Leader>ff", '<cmd>Pick files tool="rg"<cr>', { desc = "Find Files" })

map("n", "<Leader>fs", '<cmd>Pick grep_live tool="rg"<cr>', { desc = "Live Grep" })
map("n", "<Leader>fS", function()
  MiniPick.registry.grep_live({
    cwd = vim.fn.expand "%:p:h",
  })
end, { desc = "Live Grep (File Dir)" })

map("n", "<Leader>fw", function()
  MiniPick.registry.grep({
    pattern = vim.fn.expand "<cword>",
  })
end, { desc = "Grep cword" })
map("n", "<Leader>fW", function()
  MiniPick.registry.grep({
    cwd = vim.fn.expand "%:p:h",
    pattern = vim.fn.expand "<cword>",
  })
end, { desc = "Grep cword (File Dir)" })

map("n", "<Leader>fb", "<cmd>Pick buffers<cr>", { desc = "Find Buffers" })
map("n", "<Leader>fB", '<cmd>Pick buf_lines scope="current"<cr>', { desc = "Search line Buffers" })
map("n", "<Leader>fn", find_nvim_config, { desc = "Find Neovim" })
map("n", "<Leader>fd", find_dot_config, { desc = "Find Neovim" })
map("n", "<Leader>fh", "<cmd>Pick help<cr>", { desc = "Find Help" })
map("n", "<Leader>fk", "<cmd>Pick keymaps<cr>", { desc = "Find Keymaps" })
map("n", "<Leader>fm", "<cmd>Pick manpages<cr>", { desc = "Find Manpages" })
map("n", "<Leader>fr", "<cmd>Pick resume<cr>", { desc = "Find Resume" })
map(
  "n",
  "<Leader>ft",
  '<cmd>Pick hipatterns highlighters={"todo","fixme","note"}<cr>',
  { desc = "Find TODOs/FIXMEs/etc" }
)
map(
  "n",
  "<Leader>fT",
  '<cmd>Pick hipatterns scope="current" highlighters={"todo","fixme","note"}<cr>',
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
