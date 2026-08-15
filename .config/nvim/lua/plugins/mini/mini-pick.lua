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

local function lsp_workspace_roots()
  local roots, seen = {}, {}

  local function add_root(root)
    if type(root) ~= "string" or root == "" then return end
    root = vim.fn.fnamemodify(root, ":p")
    if root ~= "/" then root = root:gsub("/$", "") end
    if not seen[root] then
      seen[root] = true
      table.insert(roots, root)
    end
  end

  for _, client in ipairs(vim.lsp.get_clients()) do
    local folders = client.workspace_folders or {}
    if #folders > 0 then
      for _, folder in ipairs(folders) do
        if type(folder.uri) == "string" then add_root(vim.uri_to_fname(folder.uri)) end
      end
    elseif client.root_dir ~= nil then
      add_root(client.root_dir)
    end
  end
  return roots
end

local function workspace_symbol_location(item, max_width, roots)
  local path = item.path or item.filename or ""
  if vim.startswith(path, "file://") then path = vim.uri_to_fname(path) end
  path = vim.fn.fnamemodify(path, ":p")

  local best_root
  for _, root in ipairs(roots) do
    if path == root or vim.startswith(path, root .. "/") then
      if best_root == nil or #root > #best_root then best_root = root end
    end
  end

  local display_path = best_root and path:sub(#best_root + 2) or vim.fn.fnamemodify(path, ":~:.")
  local location = string.format("%s:%s:%s", display_path, item.lnum or 1, item.col or 1)
  if vim.fn.strdisplaywidth(location) > max_width then
    display_path = vim.fn.pathshorten(display_path)
    location = string.format("%s:%s:%s", display_path, item.lnum or 1, item.col or 1)
  end

  if vim.fn.strdisplaywidth(location) <= max_width then return location end
  return "…" .. location:sub(-math.max(max_width - 1, 1))
end

local function show_workspace_symbols(buf_id, items, query)
  local win_id = vim.fn.bufwinid(buf_id)
  local win_width = win_id == -1 and vim.o.columns or vim.api.nvim_win_get_width(win_id)
  local display_items = {}
  local decorations = {}
  local roots = lsp_workspace_roots()
  local location_width = math.min(48, math.max(20, math.floor(win_width * 0.38)))

  for i, item in ipairs(items) do
    local symbol_text = (item.text or ""):match ".*│%s*(.*)$" or item.text or ""
    local parsed_kind, parsed_name = symbol_text:match "^%[([^%]]+)%]%s*(.*)$"
    local kind = parsed_kind or item.kind or "Symbol"
    local name = parsed_name or symbol_text
    local icon, icon_hl = "", nil
    if _G.MiniIcons ~= nil and type(_G.MiniIcons.get) == "function" then
      icon, icon_hl = _G.MiniIcons.get("lsp", kind)
      icon = icon or ""
    end

    local kind_text = string.format("[%s]", kind)
    local location = workspace_symbol_location(item, location_width, roots)
    local prefix = icon == "" and "" or icon .. " "
    local text = string.format("%s%s  %s  %s", prefix, name, kind_text, location)
    local display_item = vim.deepcopy(item)
    display_item.text = text
    table.insert(display_items, display_item)

    table.insert(decorations, {
      row = i - 1,
      icon_hl = icon_hl or item.hl,
      icon_end = #prefix,
      kind_start = #prefix + #name + 2,
      kind_end = #prefix + #name + 2 + #kind_text,
    })
  end

  MiniPick.default_show(buf_id, display_items, query)

  local namespace = vim.api.nvim_create_namespace "ConfigMiniPickWorkspaceSymbols"
  vim.api.nvim_buf_clear_namespace(buf_id, namespace, 0, -1)
  for _, decoration in ipairs(decorations) do
    if decoration.icon_hl ~= nil and decoration.icon_end > 0 then
      vim.api.nvim_buf_set_extmark(buf_id, namespace, decoration.row, 0, {
        end_col = decoration.icon_end,
        hl_group = decoration.icon_hl,
        hl_mode = "combine",
        priority = 190,
      })
    end
    vim.api.nvim_buf_set_extmark(buf_id, namespace, decoration.row, decoration.kind_start, {
      end_col = decoration.kind_end,
      hl_group = "Comment",
      hl_mode = "combine",
      priority = 190,
    })
  end
end

MiniPick.registry.workspace_symbol = function()
  return require("mini.extra").pickers.lsp({ scope = "workspace_symbol_live" }, {
    source = { name = "LSP (workspace symbols)", show = show_workspace_symbols },
  })
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
