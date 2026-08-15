---@type LazySpec

local pick_utils = require "config.pick"

local function todo_comments()
  if not package.loaded["todo-comments"] then require("lazy").load { plugins = { "todo-comments.nvim" } } end
  local search = require "todo-comments.search"
  search.search(function(results)
    local items = {}
    for _, item in ipairs(results) do
      table.insert(items, {
        path = item.filename,
        lnum = item.lnum,
        col = item.col,
        tag = item.tag,
        message = item.message,
        text = item.text,
      })
    end
    require("mini.pick").start {
      source = { name = "Todo Comments", items = items, show = pick_utils.show_todo_comments },
    }
  end, {})
end

local function git_cwd()
  local cwd = vim.fn.expand "%:p:h"
  return cwd == "" and vim.fn.getcwd() or cwd
end

local function visual_text()
  local ok, region = pcall(vim.fn.getregion, vim.fn.getpos "v", vim.fn.getpos ".", { type = vim.fn.mode() })
  if ok and type(region) == "table" then return table.concat(region, "\n") end
  return vim.fn.expand "<cword>"
end

local function map_picker(maps, mode, lhs, callback, desc)
  maps[mode][lhs] = { callback, desc = desc }
end

local function configure_mappings(_, opts)
  local astrocore = require "astrocore"
  if astrocore.is_available "fzf-lua" then return end

  local MiniExtra = require "mini.extra"
  local MiniPick = require "mini.pick"

  opts.mappings = opts.mappings or {}
  opts.mappings.n = opts.mappings.n or {}
  opts.mappings.x = opts.mappings.x or {}
  local maps = opts.mappings

  local function pick(name, local_opts)
    return function() MiniPick.registry[name](local_opts or {}) end
  end

  map_picker(maps, "n", "<Leader>f<CR>", function() MiniPick.builtin.resume() end, "Resume")
  map_picker(maps, "n", "<Leader>f'", pick "marks", "Marks")
  map_picker(maps, "n", "<Leader>f/", function() MiniExtra.pickers.buf_lines { scope = "current" } end, "Buffer lines")
  map_picker(maps, "n", "<Leader>fa", pick "autocmds", "Autocmds")
  map_picker(maps, "n", "<Leader>fb", pick "buffers", "Buffers")
  map_picker(maps, "n", "<Leader>fB", pick "buf_lines", "Buffer lines")
  map_picker(maps, "n", "<Leader>fc", function() MiniPick.registry.grep { pattern = vim.fn.expand "<cword>" } end, "Grep word")
  map_picker(maps, "n", "<Leader>fC", pick "commands", "Commands")
  map_picker(maps, "n", "<Leader>fd", function()
    MiniPick.registry.git_files { cwd = vim.fn.expand "~/Documents/dotfiles" }
  end, "Dotfiles")
  map_picker(maps, "n", "<Leader>ff", pick "files", "Files")
  map_picker(maps, "n", "<Leader>fg", pick "git_files", "Git files")
  map_picker(maps, "n", "<Leader>fh", pick "help", "Help")
  map_picker(maps, "n", "<Leader>fk", pick "keymaps", "Keymaps")
  map_picker(maps, "n", "<Leader>fm", pick "manpages", "Manpages")
  map_picker(maps, "n", "<Leader>fn", function()
    MiniPick.registry.files { cwd = vim.fn.stdpath "config" }
  end, "Neovim config")
  map_picker(maps, "n", "<Leader>fo", function() MiniPick.registry.oldfiles { current_dir = true } end, "Old files (cwd)")
  map_picker(maps, "n", "<Leader>fO", pick "oldfiles", "Old files")
  map_picker(maps, "n", "<Leader>fr", pick "resume", "Resume")
  map_picker(maps, "n", "<Leader>fR", pick "registers", "Registers")
  map_picker(maps, "n", "<Leader>fs", pick "grep_live", "Search")
  map_picker(maps, "n", "<Leader>fS", function()
    MiniPick.builtin.grep_live({}, { source = { cwd = git_cwd() } })
  end, "Search (current dir)")
  map_picker(maps, "n", "<Leader>ft", pick "colorschemes", "Colorschemes")
  map_picker(maps, "n", "<Leader>fu", pick "undo", "Undo history")
  map_picker(maps, "n", "<Leader>fz", pick "zoxide", "Zoxide")
  map_picker(maps, "n", "<Leader>fw", function() MiniPick.registry.grep { pattern = vim.fn.expand "<cword>" } end, "Grep word")
  map_picker(maps, "n", "<Leader>fW", function()
    MiniPick.registry.grep { pattern = vim.fn.expand "<cword>", cwd = git_cwd() }
  end, "Grep word (current dir)")
  map_picker(maps, "n", "<Leader>fG", function() MiniPick.registry.grep_live { tool = "git" } end, "Git grep")
  map_picker(maps, "n", "<Leader>fT", todo_comments, "Todo comments")
  map_picker(maps, "n", "<C-p>", pick "files", "Files")
  map_picker(maps, "n", "<Leader>gC", function() MiniExtra.pickers.git_commits { path = "%" } end, "File commits")
  map_picker(maps, "n", "<Leader>gD", function() MiniExtra.pickers.git_hunks {} end, "Git diff hunks")
  map_picker(maps, "n", "<Leader>gb", pick "git_branches", "Branches")
  map_picker(maps, "n", "<Leader>gc", function() MiniExtra.pickers.git_commits {} end, "Commits")
  map_picker(maps, "n", "<Leader>gh", function() MiniExtra.pickers.git_hunks {} end, "Git hunks")
  map_picker(maps, "n", "<Leader>gt", pick "git_status", "Git status")
  map_picker(maps, "n", "<Leader>gw", pick "worktrees", "Worktrees")
  map_picker(maps, "x", "<Leader>fs", function() MiniPick.registry.grep { pattern = visual_text() } end, "Grep selection")
  map_picker(maps, "x", "<Leader>fS", function()
    MiniPick.registry.grep { pattern = visual_text(), cwd = git_cwd() }
  end, "Grep selection (current dir)")
end

local function jump_to_lsp_item(item)
  local path = item.filename
  if type(path) ~= "string" or path == "" then
    if type(item.bufnr) == "number" and vim.api.nvim_buf_is_valid(item.bufnr) then
      vim.cmd "normal! m'"
      vim.api.nvim_set_current_buf(item.bufnr)
      vim.api.nvim_win_set_cursor(0, { item.lnum or 1, math.max((item.col or 1) - 1, 0) })
      return true
    end
    return false
  end

  if vim.startswith(path, "file://") then path = vim.uri_to_fname(path) end
  local buf = vim.fn.bufadd(path)
  if buf == 0 then return false end
  vim.fn.bufload(buf)
  vim.cmd "normal! m'"
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { item.lnum or 1, math.max((item.col or 1) - 1, 0) })
  return true
end

local function start_mini_extra_lsp(scope, opts)
  local MiniPick = require "mini.pick"
  local MiniExtra = require "mini.extra"
  local previous_show = MiniPick.config.source.show
  local previous_method = vim.lsp.buf[scope]
  local jump_single = scope ~= "document_symbol" and scope ~= "workspace_symbol_live"

  -- MiniExtra owns the LSP on_list callback. Wrap it so a single result can
  -- jump directly without issuing a second request for the picker.
  if jump_single and type(previous_method) == "function" then
    vim.lsp.buf[scope] = function(...)
      local args = { ... }
      local opts_index = (scope == "references" or scope == "workspace_symbol") and 2 or 1
      local request_opts = args[opts_index]
      if type(request_opts) ~= "table" or type(request_opts.on_list) ~= "function" then
        return previous_method(unpack(args))
      end

      local on_list = request_opts.on_list
      request_opts = vim.tbl_extend("force", {}, request_opts)
      request_opts.on_list = function(data)
        local items = data and data.items or {}
        if scope == "references" then items = pick_utils.filter_current_lsp_items(items) end
        if #items == 1 and jump_to_lsp_item(items[1]) then return end
        if #items == 0 then
          vim.notify("No LSP results", vim.log.levels.INFO)
          return
        end
        local filtered_data = vim.tbl_extend("force", {}, data or {})
        filtered_data.items = items
        return on_list(filtered_data)
      end
      args[opts_index] = request_opts
      return previous_method(unpack(args))
    end
  end

  -- MiniExtra uses its own LSP renderer when no global source.show is set.
  -- Temporarily hide the filename-first renderer so symbols retain their
  -- kind icons, highlighting, positions, and previews.
  MiniPick.config.source.show = nil
  local ok, result = pcall(MiniExtra.pickers.lsp, { scope = scope }, opts)
  MiniPick.config.source.show = previous_show
  if jump_single and type(previous_method) == "function" then vim.lsp.buf[scope] = previous_method end
  if not ok then vim.notify(result, vim.log.levels.ERROR) end
  return ok and result or nil
end

local function lsp_picker(scope)
  return function()
    if scope == "workspace_symbol" then
      return start_mini_extra_lsp("workspace_symbol_live", {
        source = { name = "LSP (workspace symbols)", show = pick_utils.show_workspace_symbols },
      })
    end
    if type(vim.lsp.buf[scope]) ~= "function" then return end

    local opts = {}
    local location_scopes = {
      declaration = true,
      definition = true,
      implementation = true,
      references = true,
      type_definition = true,
    }
    if location_scopes[scope] then
      opts.source = { show = pick_utils.show_lsp_locations }
      if scope ~= "references" then opts.window = { config = pick_utils.lsp_window_config } end
    end
    return start_mini_extra_lsp(scope, opts)
  end
end

local function configure_lsp(_, opts)
  if require("astrocore").is_available "fzf-lua" then return end
  opts.mappings = opts.mappings or {}
  opts.mappings.n = opts.mappings.n or {}
  local maps = opts.mappings
  local lsp = lsp_picker

  maps.n.gd = { lsp "definition", desc = "Definition", cond = "textDocument/definition" }
  maps.n.gy = { lsp "type_definition", desc = "Type definition", cond = "textDocument/typeDefinition" }
  maps.n.grr = { lsp "references", desc = "References", cond = "textDocument/references" }
  maps.n.gri = { lsp "implementation", desc = "Implementation", cond = "textDocument/implementation" }
  maps.n.grs = { lsp "document_symbol", desc = "Document symbols", cond = "textDocument/documentSymbol" }
  maps.n.grS = { lsp "workspace_symbol", desc = "Live workspace symbols", cond = "workspace/symbol" }
  maps.n["<Leader>ls"] = { lsp "document_symbol", desc = "Document symbols", cond = "textDocument/documentSymbol" }
  maps.n["<Leader>lS"] = { lsp "workspace_symbol", desc = "Live workspace symbols", cond = "workspace/symbol" }
  maps.n["<Leader>lR"] = { lsp "references", desc = "References", cond = "textDocument/references" }
  maps.n["<Leader>lD"] = {
    function()
      local pick = require "mini.pick"
      require("mini.extra").pickers.diagnostic({ scope = "all" }, { source = { show = pick.config.source.show } })
    end,
    desc = "Workspace diagnostics",
  }
end

return {
  {
    "nvim-mini/mini.pick",
    version = false,
    lazy = true,
    dependencies = {
      { "nvim-mini/mini.extra", version = false },
      { "nvim-mini/mini.icons", optional = true },
    },
    config = function()
      local MiniExtra = require "mini.extra"
      local MiniPick = require "mini.pick"
      local fzy_config = require "config.fzy"

      local function choose_all()
        return MiniPick.default_choose_marked(MiniPick.get_picker_matches().all)
      end

      MiniPick.setup {
        mappings = {
          -- choose_in_split = "<C-x>",
          choose_in_tabpage = "<C-t>",
          choose_marked = "<M-CR>",
          -- mark = "<C-m>",
          mark_all = "<C-a>",
          move_down = "<C-j>",
          move_up = "<C-k>",
          refine = "<C-Space>",
          refine_marked = "<M-Space>",
          choose_all = { char = "<C-q>", func = choose_all },
        },
        source = { show = pick_utils.show_filename_first },
        window = {
          config = pick_utils.window_config,
          prompt_caret = "׀",
          prompt_prefix = "󰄾 ",
        },
      }

      MiniExtra.setup()

      local function picker_cwd(local_opts)
        local opts = vim.deepcopy(local_opts or {})
        local cwd = opts.cwd
        opts.cwd = nil
        return opts, cwd
      end

      local function fzy_source_opts(source)
        local opts = fzy_config.source_opts()
        opts.source = vim.tbl_extend("force", opts.source, source or {})
        return opts
      end

      MiniPick.registry.files = function(local_opts)
        local opts, cwd = picker_cwd(local_opts)
        return MiniPick.builtin.files(opts, fzy_source_opts { cwd = cwd })
      end

      MiniPick.registry.git_files = function(local_opts)
        local _, cwd = picker_cwd(local_opts)
        cwd = cwd or vim.fn.getcwd()
        return MiniPick.builtin.cli({
          command = { "git", "-C", cwd, "ls-files", "--cached", "--others", "--exclude-standard" },
        }, fzy_source_opts { cwd = cwd, name = "Git Files" })
      end

      MiniPick.registry.oldfiles = function(local_opts)
        return MiniExtra.pickers.oldfiles(local_opts, fzy_source_opts())
      end

      MiniPick.registry.buffers = function(local_opts)
        return MiniPick.builtin.buffers(local_opts, fzy_source_opts())
      end

      MiniPick.registry.grep = function(local_opts)
        local opts, cwd = picker_cwd(local_opts)
        return MiniPick.builtin.grep(opts, { source = { cwd = cwd } })
      end

      MiniPick.registry.autocmds = function()
        local items = {}
        for _, autocmd in ipairs(vim.api.nvim_get_autocmds {}) do
          local command = autocmd.command or (autocmd.callback and "<callback>") or ""
          table.insert(items, {
            text = string.format("%s  %s  %s", autocmd.event, autocmd.pattern or "*", command),
            autocmd = autocmd,
          })
        end
        table.sort(items, function(a, b) return a.text < b.text end)
        return MiniPick.start {
          source = {
            name = "Autocmds",
            items = items,
            preview = function(buf_id, item)
              vim.api.nvim_buf_set_lines(buf_id, 0, -1, false, vim.split(vim.inspect(item.autocmd), "\n"))
            end,
          },
        }
      end

      MiniPick.registry.zoxide = function()
        if vim.fn.executable "zoxide" ~= 1 then
          vim.notify("zoxide is not executable", vim.log.levels.WARN)
          return
        end

        local postprocess = function(lines)
          local items = {}
          for _, line in ipairs(lines) do
            local path = line:match "^%s*[%d%.]+%s+(.+)$"
            if path ~= nil then table.insert(items, { text = path, path = path }) end
          end
          return items
        end
        local choose = function(item)
          if item == nil or item.path == nil then return end
          vim.fn.chdir(item.path)
          vim.fn.jobstart({ "zoxide", "add", "--", item.path }, { detach = true })
        end
        return MiniPick.builtin.cli({
          command = { "zoxide", "query", "--list", "--score" },
          postprocess = postprocess,
        }, { source = { name = "Zoxide", choose = choose } })
      end

      MiniPick.registry.undo = function()
        local undo = vim.fn.undotree()
        local items = { { seq = 0, text = "origin" } }

        local function add_entries(entries, prefix)
          for _, entry in ipairs(entries or {}) do
            local current = entry.seq == undo.seq_cur and "*" or " "
            local timestamp = entry.time and os.date("%Y-%m-%d %H:%M:%S", entry.time) or ""
            table.insert(items, {
              seq = entry.seq,
              text = string.format("%s %s %s%s", current, entry.seq, timestamp, prefix),
            })
            add_entries(entry.alt, prefix .. "  ")
          end
        end
        add_entries(undo.entries, "")

        return MiniPick.start {
          source = {
            name = "Undo history",
            items = items,
            choose = function(item)
              if item ~= nil then vim.cmd("silent undo " .. item.seq) end
            end,
          },
        }
      end

      local function branch_items()
        local items = {}
        for _, line in ipairs(vim.fn.systemlist { "git", "branch", "--all", "--no-color", "-vv" }) do
          local marker, branch = line:match "^%s*([*+]?)%s*([^%s]+)"
          if branch ~= nil and not line:find " -> " then
            local remote, remote_branch = branch:match "^remotes/([^/]+)/(.+)$"
            table.insert(items, {
              text = line,
              branch = branch,
              marker = marker,
              remote = remote,
              remote_branch = remote_branch,
            })
          end
        end
        return items
      end

      MiniPick.registry.git_branches = function()
        local refresh = function() MiniPick.set_picker_items(branch_items()) end
        local switch = function(item)
          if item == nil or item.marker == "*" then return end
          local command = { "git", "switch", item.branch }
          if item.remote ~= nil then
            command = { "git", "switch", "--track", "-c", item.remote_branch, item.branch }
          end
          local result = vim.system(command):wait()
          if result.code ~= 0 then vim.notify(result.stderr, vim.log.levels.ERROR) end
          vim.cmd "checktime"
        end
        local add = function()
          local branch = vim.fn.input("New branch: ", table.concat(MiniPick.get_picker_query()))
          if branch == "" then return end
          local result = vim.system({ "git", "switch", "-c", branch }):wait()
          if result.code ~= 0 then vim.notify(result.stderr, vim.log.levels.ERROR) end
          refresh()
        end
        local remove = function()
          local item = MiniPick.get_picker_matches().current
          if item == nil or item.marker == "*" then return end
          if item.remote ~= nil then
            vim.notify("Remote branches cannot be deleted from this picker", vim.log.levels.WARN)
            return
          end
          local branch = item.branch
          if vim.fn.confirm("Delete branch " .. branch .. "?", "&Yes\n&No", 2) ~= 1 then return end
          local result = vim.system({ "git", "branch", "--delete", branch }):wait()
          if result.code ~= 0 then vim.notify(result.stderr, vim.log.levels.ERROR) end
          refresh()
        end
        return MiniPick.start {
          mappings = {
            add_branch = { char = "<C-a>", func = add },
            delete_branch = { char = "<C-x>", func = remove },
            mark = "",
            mark_all = "",
          },
          source = { name = "Git Branches", items = branch_items(), choose = switch },
        }
      end

      local function worktree_items()
        local items = {}
        for _, line in ipairs(vim.fn.systemlist { "git", "worktree", "list" }) do
          local path, hash, branch = line:match "^(.-)%s+(%S+)%s+%[(.-)%]$"
          if path ~= nil then
            table.insert(items, { path = path, text = path .. "  " .. branch, hash = hash, branch = branch })
          end
        end
        return items
      end

      MiniPick.registry.worktrees = function()
        local refresh = function() MiniPick.set_picker_items(worktree_items()) end
        local add = function()
          local branch = vim.fn.input "New worktree branch: "
          if branch == "" then return end
          local path = vim.fn.fnamemodify(vim.fn.getcwd(), ":h") .. "/" .. branch
          local result = vim.system({ "git", "worktree", "add", path, "-b", branch }):wait()
          if result.code ~= 0 then vim.notify(result.stderr, vim.log.levels.ERROR) end
          refresh()
        end
        local remove = function()
          local item = MiniPick.get_picker_matches().current
          if item == nil or item.path == vim.fn.getcwd() then return end
          if vim.fn.confirm("Remove worktree " .. item.path .. "?", "&Yes\n&No", 2) ~= 1 then return end
          local result = vim.system({ "git", "worktree", "remove", item.path }):wait()
          if result.code ~= 0 then vim.notify(result.stderr, vim.log.levels.ERROR) end
          refresh()
        end
        local choose = function(item)
          if item == nil then return end
          vim.fn.chdir(item.path)
          vim.cmd "checktime"
        end
        return MiniPick.start {
          mappings = {
            add_worktree = { char = "<C-a>", func = add },
            delete_worktree = { char = "<C-x>", func = remove },
            mark = "",
            mark_all = "",
          },
          source = { name = "Git Worktrees", items = worktree_items(), choose = choose },
        }
      end

      MiniPick.registry.git_status = function()
        local items = {}
        for _, line in ipairs(vim.fn.systemlist { "git", "status", "--porcelain=v1", "-u" }) do
          local path = line:sub(4)
          path = path:match(" -> (.*)$") or path
          if path ~= "" then
            table.insert(items, { path = path, text = line })
          end
        end
        return MiniPick.start {
          source = {
            name = "Git Status",
            items = items,
            choose = MiniPick.default_choose,
          },
        }
      end

      MiniPick.registry.registry = MiniPick.registry.registry or function()
        local names = vim.tbl_keys(MiniPick.registry)
        table.sort(names)
        local name = MiniPick.start { source = { name = "Pickers", items = names } }
        if name ~= nil and MiniPick.registry[name] ~= nil then return MiniPick.registry[name]() end
      end

    end,
    specs = {
      {
        "AstroNvim/astrocore",
        opts = function(_, opts)
          configure_mappings(_, opts)
        end,
      },
      {
        "AstroNvim/astrolsp",
        optional = true,
        opts = function(_, opts)
          configure_lsp(_, opts)
        end,
      },
    },
  },
}
