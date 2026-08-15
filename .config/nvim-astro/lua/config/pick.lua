local M = {}

local ns_id = vim.api.nvim_create_namespace("ConfigPick")

local function split_path(path)
  local from, to = path:find(".*[/\\\\]")
  if from == nil then return path, "" end

  local dirname = path:sub(from, to - 1)
  local basename = path:sub(to + 1)
  return basename ~= "" and basename or path, dirname
end
M.split_path = split_path

local function path_text(path)
  if type(path) == "table" then return path.path or path.text or "" end
  return tostring(path or "")
end
M.path_text = path_text

local function display_parts(path)
  local basename, dirname = split_path(path)
  if dirname == "" then return basename, nil end
  return basename .. "  " .. dirname, #basename + 2
end
M.display_parts = display_parts

function M.filename_first(path) return display_parts(path) end

local function display_item(item)
  if type(item) == "string" then
    local path, lnum, col, text = item:match "^(.-)%z(%d+)%z?(%d*)%z?(.*)$"
    if path ~= nil then
      return {
        text = M.filename_first(path) .. (text == "" and "" or "  " .. text),
        path = path,
        lnum = tonumber(lnum),
        col = tonumber(col),
      }
    end
    return { text = M.filename_first(item), path = item }
  end

  if type(item) ~= "table" or type(item.text) ~= "string" then return item end

  local result = vim.deepcopy(item)
  -- Help tags have a `filename` for previewing, but their visible item is the
  -- tag name. They are not path-oriented picker entries.
  if result.path == nil and result.filename ~= nil and (result.name ~= nil or result.cmd ~= nil) then
    return result
  end

  local path = result.path or result.filename
  if type(path) ~= "string" or path == "" then return result end

  -- MiniExtra formats LSP locations as path│line│column│text. Keep the
  -- location data intact while changing only the visible path order.
  local lsp_path, lsp_rest = result.text:match "^([^│]+)│(.*)$"
  local normalized_path = vim.fn.fnamemodify(path, ":p:.")
  if lsp_path ~= nil and result.lnum ~= nil and (lsp_path == path or lsp_path == normalized_path) then
    result.text = M.filename_first(lsp_path) .. "│" .. lsp_rest
  elseif result.text == path then
    result.text = M.filename_first(path)
  else
    result.text = M.filename_first(path) .. "  " .. result.text
  end
  result.path = path
  return result
end
M.display_item = display_item

function M.show_filename_first(buf_id, items, query, opts)
  opts = opts or {}
  local display_items, dim_from = {}, {}

  for i, item in ipairs(items) do
    display_items[i] = display_item(item)
    local path = type(display_items[i]) == "table" and display_items[i].path or path_text(item)
    local text = type(display_items[i]) == "table" and display_items[i].text or nil
    local basename, dirname = split_path(path)
    if dirname ~= "" and type(text) == "string" and vim.startswith(text, M.filename_first(path)) then
      dim_from[i] = #basename + 2
    end
  end

  local MiniPick = require "mini.pick"
  local show_query = opts.match_ranges and {} or query
  MiniPick.default_show(buf_id, display_items, show_query, { show_icons = true })

  vim.api.nvim_buf_clear_namespace(buf_id, ns_id, 0, -1)
  for i, ranges in pairs(opts.match_ranges or {}) do
    local line = vim.api.nvim_buf_get_lines(buf_id, i - 1, i, false)[1] or ""
    local text = type(display_items[i]) == "table" and display_items[i].text
    local start = type(text) == "string" and line:find(text, 1, true) or nil
    if start ~= nil then
      for _, range in ipairs(ranges) do
        vim.api.nvim_buf_set_extmark(buf_id, ns_id, i - 1, start + range[1] - 1, {
          end_col = start + range[2] - 1,
          hl_group = "MiniPickMatchRanges",
          hl_mode = "combine",
          priority = 200,
        })
      end
    end
  end

  for i, col in ipairs(dim_from) do
    if col ~= nil then
      local line = vim.api.nvim_buf_get_lines(buf_id, i - 1, i, false)[1] or ""
      local text = type(display_items[i]) == "table" and display_items[i].text
      local start = type(text) == "string" and line:find(text, 1, true) or nil
      if start ~= nil then
        vim.api.nvim_buf_set_extmark(buf_id, ns_id, i - 1, start + col - 1, {
          end_col = #line,
          hl_group = "Comment",
          hl_mode = "combine",
          priority = 199,
        })
      end
    end
  end
end

function M.source_opts(source)
  return { source = vim.tbl_extend("force", { show = M.show_filename_first }, source or {}) }
end

local function normalize_path(path)
  if type(path) ~= "string" or path == "" then return "" end
  if vim.startswith(path, "file://") then path = vim.uri_to_fname(path) end
  return vim.fn.fnamemodify(path, ":p")
end

local function truncate_text(text, max_width)
  if max_width <= 0 then return "" end
  if vim.fn.strdisplaywidth(text) <= max_width then return text end
  if max_width == 1 then return "…" end
  return vim.fn.strcharpart(text, 0, max_width - 1) .. "…"
end

function M.workspace_roots()
  local roots, seen = {}, {}
  local function add_root(root)
    root = normalize_path(root)
    if root == "" then return end
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

local function relative_path(path, roots)
  path = normalize_path(path)
  local best_root
  for _, root in ipairs(roots or {}) do
    if path == root or vim.startswith(path, root .. "/") then
      if best_root == nil or #root > #best_root then best_root = root end
    end
  end
  return best_root and path:sub(#best_root + 2) or vim.fn.fnamemodify(path, ":~:.")
end

function M.workspace_symbol_location(item, max_width, roots)
  local path = relative_path(item.path or item.filename, roots)
  local location = string.format("%s:%s:%s", path, item.lnum or 1, item.col or 1)
  if vim.fn.strdisplaywidth(location) > max_width then
    path = vim.fn.pathshorten(path)
    location = string.format("%s:%s:%s", path, item.lnum or 1, item.col or 1)
  end
  if vim.fn.strdisplaywidth(location) <= max_width then return location end
  return "…" .. location:sub(-math.max(max_width - 1, 1))
end

function M.lsp_source_text(item)
  local text = type(item.text) == "string" and item.text or ""
  return text:match ".*│%s*(.*)$" or text
end

function M.lsp_location_text(item, max_width, roots)
  local path = relative_path(item.path or item.filename, roots)
  local basename, dirname = split_path(path)
  local location = string.format("%s:%s:%s", basename, item.lnum or 1, item.col or 1)
  local source = M.lsp_source_text(item)
  local directory = dirname:gsub("[/\\]$", "")
  local location_width = vim.fn.strdisplaywidth(location)
  if location_width >= max_width then return truncate_text(location, max_width), nil end

  -- Keep the directory as a low-priority disambiguator. It should never take
  -- space away from the location or a useful source-line preview.
  local directory_width = directory == "" and 0 or math.min(32, math.max(12, math.floor(max_width * 0.28)))
  local directory_display = directory
  if directory_width > 0 and vim.fn.strdisplaywidth(directory_display) > directory_width then
    directory_display = vim.fn.pathshorten(directory_display)
  end
  local directory_text = directory_width > 0 and truncate_text(directory_display, directory_width) or ""
  local source_width = max_width - location_width - 2
  if directory_text ~= "" then source_width = source_width - vim.fn.strdisplaywidth(directory_text) - 2 end

  if source ~= "" and source_width < 12 then
    directory_text, source_width = "", max_width - location_width - 2
  end

  local text = location
  if source ~= "" and source_width >= 12 then text = text .. "  " .. truncate_text(source, source_width) end

  local directory_offset
  if directory_text ~= "" then
    local remaining = max_width - vim.fn.strdisplaywidth(text) - 2
    if remaining > 0 then
      directory_text = truncate_text(directory_text, remaining)
      directory_offset = #text + 2
      text = text .. "  " .. directory_text
    end
  end
  return text, directory_offset
end

function M.show_lsp_locations(buf_id, items, query)
  local MiniPick = require "mini.pick"
  local win_id = vim.fn.bufwinid(buf_id)
  local win_width = win_id == -1 and vim.o.columns or vim.api.nvim_win_get_width(win_id)
  local max_width = math.max(24, win_width - 4)
  local roots = M.workspace_roots()
  local display_items, directory_offsets = {}, {}

  for i, item in ipairs(items) do
    local text, directory_offset = M.lsp_location_text(item, max_width, roots)
    display_items[i] = vim.tbl_extend("force", vim.deepcopy(item), { text = text })
    directory_offsets[i] = directory_offset
  end

  MiniPick.default_show(buf_id, display_items, query, { show_icons = true })

  vim.api.nvim_buf_clear_namespace(buf_id, ns_id, 0, -1)
  for i, directory_offset in ipairs(directory_offsets) do
    if directory_offset ~= nil then
      local line = vim.api.nvim_buf_get_lines(buf_id, i - 1, i, false)[1] or ""
      local start = line:find(display_items[i].text, 1, true)
      if start ~= nil then
        local directory_start = start - 1 + directory_offset
        if directory_start < #line then
          vim.api.nvim_buf_set_extmark(buf_id, ns_id, i - 1, directory_start, {
            end_col = #line,
            hl_group = "Comment",
            hl_mode = "combine",
            priority = 199,
          })
        end
      end
    end
  end
end

function M.todo_comment_text(item, max_width, cwd)
  local tag = item.tag or "TODO"
  local message = vim.trim(item.message or "")
  if message == "" then
    message = vim.trim(item.text or ""):gsub("^" .. vim.pesc(tag) .. ":%s*", "")
  end

  local icon = ""
  local ok, todo_config = pcall(require, "todo-comments.config")
  if ok then icon = vim.tbl_get(todo_config, "options", "keywords", tag, "icon") or "" end

  local label = icon .. tag
  local path = relative_path(item.path or item.filename, { cwd })
  local basename, dirname = split_path(path)
  local location = string.format("%s:%s:%s", basename, item.lnum or 1, item.col or 1)
  local directory = dirname:gsub("[/\\]$", "")
  local location_width = vim.fn.strdisplaywidth(location)
  local label_width = vim.fn.strdisplaywidth(label)
  local directory_width = directory == "" and 0 or math.min(28, math.max(12, math.floor(max_width * 0.24)))
  local directory_display = directory
  if directory_width > 0 and vim.fn.strdisplaywidth(directory_display) > directory_width then
    directory_display = vim.fn.pathshorten(directory_display)
  end
  local directory_text = directory_width > 0 and truncate_text(directory_display, directory_width) or ""
  local message_width = max_width - label_width - location_width - 4
  if directory_text ~= "" then message_width = message_width - vim.fn.strdisplaywidth(directory_text) - 2 end

  if message ~= "" and message_width < 12 then
    directory_text, message_width = "", max_width - label_width - location_width - 4
  end

  local text = label
  if message ~= "" and message_width >= 8 then text = text .. "  " .. truncate_text(message, message_width) end
  text = text .. "  " .. location

  local directory_offset
  if directory_text ~= "" then
    local remaining = max_width - vim.fn.strdisplaywidth(text) - 2
    if remaining > 0 then
      directory_text = truncate_text(directory_text, remaining)
      directory_offset = #text + 2
      text = text .. "  " .. directory_text
    end
  end

  return text, #label, directory_offset, tag
end

function M.show_todo_comments(buf_id, items, query)
  local MiniPick = require "mini.pick"
  local win_id = vim.fn.bufwinid(buf_id)
  local win_width = win_id == -1 and vim.o.columns or vim.api.nvim_win_get_width(win_id)
  local max_width = math.max(24, win_width - 4)
  local display_items, decorations = {}, {}
  local cwd = vim.fn.getcwd()

  for i, item in ipairs(items) do
    local text, label_end, directory_offset, tag = M.todo_comment_text(item, max_width, cwd)
    display_items[i] = vim.tbl_extend("force", vim.deepcopy(item), { text = text })
    decorations[i] = { label_end = label_end, directory_offset = directory_offset, tag = tag }
  end

  MiniPick.default_show(buf_id, display_items, query, { show_icons = false })

  vim.api.nvim_buf_clear_namespace(buf_id, ns_id, 0, -1)
  for i, decoration in ipairs(decorations) do
    local line = vim.api.nvim_buf_get_lines(buf_id, i - 1, i, false)[1] or ""
    if vim.fn.hlexists("TodoFg" .. decoration.tag) == 1 then
      vim.api.nvim_buf_set_extmark(buf_id, ns_id, i - 1, 0, {
        end_col = decoration.label_end,
        hl_group = "TodoFg" .. decoration.tag,
        hl_mode = "combine",
        priority = 190,
      })
    end
    if decoration.directory_offset ~= nil then
      local directory_start = decoration.directory_offset
      if directory_start < #line then
        vim.api.nvim_buf_set_extmark(buf_id, ns_id, i - 1, directory_start, {
          end_col = #line,
          hl_group = "Comment",
          hl_mode = "combine",
          priority = 199,
        })
      end
    end
  end
end

function M.show_workspace_symbols(buf_id, items, query)
  local MiniPick = require "mini.pick"
  local win_id = vim.fn.bufwinid(buf_id)
  local win_width = win_id == -1 and vim.o.columns or vim.api.nvim_win_get_width(win_id)
  local display_items, decorations = {}, {}
  local roots = M.workspace_roots()
  local location_width = math.min(48, math.max(20, math.floor(win_width * 0.38)))

  for i, item in ipairs(items) do
    local symbol_text = M.lsp_source_text(item)
    local parsed_kind, parsed_name = symbol_text:match "^%[([^%]]+)%]%s*(.*)$"
    local kind = parsed_kind or item.kind or "Symbol"
    local name = parsed_name or symbol_text
    local icon, icon_hl = "", nil
    if _G.MiniIcons ~= nil and type(_G.MiniIcons.get) == "function" then
      icon, icon_hl = _G.MiniIcons.get("lsp", kind)
      icon = icon or ""
    end

    local kind_text = string.format("[%s]", kind)
    local location = M.workspace_symbol_location(item, location_width, roots)
    local prefix = icon == "" and "" or icon .. " "
    local display_item = vim.deepcopy(item)
    display_item.text = string.format("%s%s  %s  %s", prefix, name, kind_text, location)
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

  local namespace = vim.api.nvim_create_namespace "ConfigPickWorkspaceSymbols"
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

function M.filter_current_lsp_items(items)
  local current_path = normalize_path(vim.api.nvim_buf_get_name(0))
  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  if current_path == "" then return items end

  return vim.tbl_filter(function(item)
    local item_path = normalize_path(item.filename or item.path)
    if item_path ~= current_path then return true end

    local range = item.user_data and (item.user_data.range or item.user_data.targetRange)
    local start_line = range and range.start and range.start.line + 1 or item.lnum or 1
    local end_line = range and range["end"] and range["end"].line + 1 or item.end_lnum or start_line
    return cursor_line < start_line or cursor_line > end_line
  end, items)
end

function M.window_config()
  local height = math.floor(vim.o.lines * 0.618)
  local width = math.floor(vim.o.columns * 0.618)
  return {
    anchor = "NW",
    height = height,
    width = width,
    row = math.floor((vim.o.lines - height) / 2),
    col = math.floor((vim.o.columns - width) / 2),
    border = "solid",
  }
end

function M.lsp_window_config()
  local width = math.min(110, math.max(40, math.floor(vim.o.columns * 0.72)))
  local height = math.min(12, math.max(4, math.floor(vim.o.lines * 0.28)))
  width = math.min(width, math.max(vim.o.columns - 2, 20))
  height = math.min(height, math.max(vim.o.lines - 2, 4))

  local win_row, win_col = unpack(vim.api.nvim_win_get_position(0))
  local cursor_row = win_row + vim.api.nvim_win_get_cursor(0)[1]
  local cursor_col = win_col + vim.fn.virtcol "." - 1
  local row = cursor_row
  if row + height + 1 > vim.o.lines then row = cursor_row - height - 1 end

  return {
    relative = "editor",
    anchor = "NW",
    row = math.max(0, math.min(row, vim.o.lines - height - 1)),
    col = math.max(0, math.min(cursor_col, vim.o.columns - width - 1)),
    width = width,
    height = height,
    border = "solid",
  }
end

return M
