local fzf = require("fzf-lua")
local utils = require("fzf-lua.utils")

local function get_readme(path)
  if path == "" then
    return ""
  end
  for _, file in ipairs({ "README.md", "README" }) do
    local full = path .. "/" .. file
    if vim.fn.filereadable(full) == 1 then
      return full
    end
  end
  return path
end

local function short_src(url)
  if not url then
    return ""
  end
  return url:gsub("^https?://github%.com/", "")
end

local function extract_plugin_name(entry)
  if not entry then
    return nil
  end
  return entry:match("^(.-)" .. utils.nbsp)
end

local function show_details(plugin_name, p_data)
  local lines = {
    "Plugin:  " .. plugin_name,
    "Source:  " .. (p_data.spec.src or "N/A"),
    "Version: " .. tostring(p_data.spec.version or "latest"),
    "Rev:     " .. (p_data.rev or "N/A"),
    "Path:    " .. (p_data.path or "N/A"),
    "Active:  " .. tostring(p_data.active),
  }

  if p_data.path and vim.fn.isdirectory(p_data.path) == 1 then
    table.insert(lines, "")
    table.insert(lines, "--- Recent commits ---")
    local log = vim.fn.systemlist(
      string.format("git -C %s log --oneline -10", vim.fn.shellescape(p_data.path))
    )
    if vim.v.shell_error == 0 then
      for _, line in ipairs(log) do
        table.insert(lines, "  " .. line)
      end
    else
      table.insert(lines, "  (git log unavailable)")
    end
  end

  vim.cmd("tabnew")
  local buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_set_option_value("buftype", "nofile", { buf = buf })
  vim.api.nvim_set_option_value("bufhidden", "wipe", { buf = buf })
  vim.api.nvim_set_option_value("swapfile", false, { buf = buf })
  vim.api.nvim_set_option_value("buflisted", false, { buf = buf })
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_buf_set_name(buf, "pack://" .. plugin_name)
end

local function run_pack_manager(only_non_active)
  local pack_plugins = vim.pack.get(nil, { info = false })

  if not pack_plugins or #pack_plugins == 0 then
    vim.notify("No plugins found", vim.log.levels.WARN)
    return
  end

  local entries = {}
  local pack_data_map = {}

  for _, p_data in ipairs(pack_plugins) do
    if not only_non_active or not p_data.active then
      local plugin_name = p_data.spec.name
      local plugin_path = p_data.path or ""
      local preview_file = get_readme(plugin_path)

      local active_indicator = p_data.active
          and utils.ansi_codes.green("*")
          or utils.ansi_codes.dark_grey("-")

      local display_name = utils.ansi_codes.bold(plugin_name)
      local display_rev = utils.ansi_codes.green(string.sub(p_data.rev or "", 1, 7))
      local display_src = utils.ansi_codes.dark_grey(short_src(p_data.spec.src))

      local display_text = string.format(
        "[%s] %s %s %s",
        display_rev, active_indicator, display_name, display_src
      )

      -- Format: plugin_name<nbsp>preview_file:1:1:display_text
      -- The nbsp-embedded name is extracted in action callbacks via extract_plugin_name()
      -- The builtin previewer splits on nbsp and finds the file:line:col part for preview
      local entry_str = string.format(
        "%s%s%s:1:1:%s",
        plugin_name, utils.nbsp, preview_file, display_text
      )
      table.insert(entries, entry_str)
      pack_data_map[plugin_name] = p_data
    end
  end

  if #entries == 0 then
    vim.notify("No plugins to display", vim.log.levels.INFO)
    return
  end

  fzf.fzf_exec(entries, {
    prompt = "vim.pack> ",
    previewer = "builtin",
    keymap = {
      fzf = {
        ["ctrl-u"] = false,
      },
    },
    fzf_opts = {
      ["--delimiter"] = ":",
      ["--with-nth"] = "4..",
      ["--tiebreak"] = "begin",
      ["--header"] = "enter=Open | ctrl-u=Update | ctrl-x=Delete | ctrl-d=Details",
    },
    actions = {
      ["default"] = function(selected)
        local plugin_name = extract_plugin_name(selected[1])
        local p_data = pack_data_map[plugin_name]
        if p_data and p_data.path then
          vim.cmd("edit " .. p_data.path)
        end
      end,
      ["ctrl-u"] = function(selected)
        local plugin_name = extract_plugin_name(selected[1])
        if not plugin_name then
          return
        end
        vim.pack.update({ plugin_name })
        vim.schedule(function()
          run_pack_manager(only_non_active)
        end)
      end,
      ["ctrl-x"] = function(selected)
        local plugin_name = extract_plugin_name(selected[1])
        if not plugin_name then
          return
        end

        local ok, err = pcall(vim.pack.del, { plugin_name }, { force = true })
        if ok then
          vim.notify("Deleted: " .. plugin_name .. " (remove from config to prevent reinstall)", vim.log.levels.INFO)
          vim.schedule(function()
            run_pack_manager(only_non_active)
          end)
        else
          vim.notify("Failed to delete " .. plugin_name .. ": " .. tostring(err), vim.log.levels.ERROR)
        end
      end,
      ["ctrl-d"] = function(selected)
        local plugin_name = extract_plugin_name(selected[1])
        if not plugin_name then
          return
        end
        local p_data = pack_data_map[plugin_name]
        if p_data then
          show_details(plugin_name, p_data)
        end
      end,
    },
  })
end

vim.api.nvim_create_user_command("PackManage", function()
  run_pack_manager(false)
end, {})

vim.api.nvim_create_user_command("PackManageNonActive", function()
  run_pack_manager(true)
end, {})
