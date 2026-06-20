function _G.Fd(file_pattern, _)
  -- if first char is * then fuzzy search
  if file_pattern:sub(1, 1) == "*" then file_pattern = file_pattern:gsub(".", ".*%0") .. ".*" end
  local cmd = 'fd  --color=never --full-path --type file "' .. file_pattern .. '"'
  local result = vim.fn.systemlist(cmd)
  return result
end

local rtps = vim.api.nvim_list_runtime_paths()
local all_comps = {}
for _, p in ipairs(rtps) do
  for _, f in ipairs(vim.fn.globpath(p, "compiler/*.vim", 0, 1)) do
    table.insert(all_comps, vim.fn.fnamemodify(f, ":t:r"))
  end
end

local function is_large_buffer(bufnr)
  if not vim.api.nvim_buf_is_valid(bufnr) or not vim.api.nvim_buf_is_loaded(bufnr) then return false end

  local astrocore = require "astrocore"

  if require("astrocore.buffer").is_large(bufnr) then return true end

  local large_buf = vim.tbl_get(astrocore.config, "features", "large_buf")

  if not large_buf then return false end

  local enabled = large_buf.enabled

  if type(enabled) == "function" then
    large_buf = vim.deepcopy(large_buf)
    local ok, result = pcall(enabled, bufnr, large_buf)
    if not ok or result == false then return false end

    if type(result) == "table" then large_buf = result end
  elseif enabled == false then
    return false
  end

  local line_count = vim.api.nvim_buf_line_count(bufnr)
  if large_buf.lines and line_count > large_buf.lines then return true end

  local byte_count = vim.api.nvim_buf_get_offset(bufnr, line_count)
  if large_buf.size and byte_count > large_buf.size then return true end

  return large_buf.line_length and line_count > 0 and (byte_count / line_count) > large_buf.line_length or false
end

local function apply_large_buffer_guard(bufnr)
  if not vim.api.nvim_buf_is_valid(bufnr) then return end

  vim.b[bufnr].large_buf = true
  vim.b[bufnr].autoformat = false
  vim.b[bufnr].completion = false
  vim.b[bufnr].minianimate_disable = true
  vim.b[bufnr].minihipatterns_disable = true
  vim.b[bufnr].ts_highlight = false

  pcall(vim.diagnostic.enable, false, { bufnr = bufnr })

  for _, client in ipairs(vim.lsp.get_clients { bufnr = bufnr }) do
    pcall(vim.lsp.buf_detach_client, bufnr, client.id)
  end

  vim.api.nvim_buf_call(bufnr, function()
    if vim.fn.exists ":NoMatchParen" ~= 0 then vim.cmd "NoMatchParen" end
    if vim.fn.exists ":UfoDetach" ~= 0 then vim.cmd "UfoDetach" end
    if vim.fn.exists ":TSContext" ~= 0 then vim.cmd "TSContext disable" end

    pcall(vim.treesitter.stop, bufnr)

    vim.opt_local.foldmethod = "indent"
    vim.opt_local.swapfile = false
    vim.opt_local.undolevels = -1
    vim.opt_local.statuscolumn = ""
    vim.opt_local.conceallevel = 0
    vim.opt_local.list = false
  end)
end

-- apply_large_buffer_guard(0)

---@type LazySpec
return {
  {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      -- Configure core features of AstroNvim
      features = {
        large_buf = { enabled = true, size = 1024 * 256, lines = 6000, line_length = 1000 }, -- set global limits for large files for disabling features like treesitter
      },
      -- Diagnostics configuration (for vim.diagnostics.config({...})) when diagnostics are on
      diagnostics = {
        virtual_text = true,
        underline = true,
      },
      -- passed to `vim.filetype.add`
      filetypes = {
        filename = {
          dwm_sxhkdrc = "sxhkdrc",
        },
        pattern = {
          [".env*"] = "conf",
          ["tsconfig*.json"] = "jsonc",
          [".*/kitty/.+%.conf"] = "kitty",
        },
      },
      -- vim options can be configured here
      options = {
        opt = { -- vim.opt.<key>
          showbreak = "󰄾 ",
          undolevels = 10000,
          exrc = true, -- allows to create project specific settings
          sessionoptions = { "blank", "buffers", "curdir", "globals", "help", "tabpages", "winsize", "skiprtp" },
          smoothscroll = true,
          wrap = true, -- sets vim.opt.wrap
          grepprg = vim.fn.executable "rg" == 1 and "rg --vimgrep --smart-case --no-heading --sort=path"
            or vim.opt.grepprg,
          scrolloff = 8,
          splitkeep = "topline",
        },
        g = { -- vim.g.<key>
          -- unblevable/quick-scope
          qs_lazy_highlight = 1,
          qs_buftype_blacklist = { "terminal", "nofile", "dashboard", "startify" },
          -- end: unblevable/quick-scope
          markdown_recommended_style = 0,
          tsc_makeprg = "npx tsc",
        },
      },
      mappings = {
        n = {
          [","] = false,
          ["\\"] = false,
          ["<Leader>c"] = false,
          dl = { '"_dl' },
          c = { '"_c' },
          C = { '"_C' },
          ["0"] = { "^", desc = "Goto Beginning" },
          [",w"] = { "<cmd>w!<cr>", desc = "Save" },
          [",W"] = { "<cmd>noautocmd w!<cr>", desc = "Save(noautocmd)" },
          ["<localleader>e"] = {
            ':e <C-R>=expand("%:p:h") . "/" <CR>',
            silent = false,
            desc = "Edit in same dir",
          },
          ["<localleader>t"] = {
            ':tabe <C-R>=expand("%:p:h") . "/" <CR>',
            silent = false,
            desc = "Edit in same dir(Tab)",
          },
          ["<localleader>v"] = {
            ':vs <C-R>=expand("%:p:h") . "/" <CR>',
            silent = false,
            desc = "Edit in same dir(vsplit)",
          },
          ["<Leader>="] = {
            function() vim.lsp.buf.format(require("astrolsp").format_opts) end,
            desc = "Format buffer",
          },
          ["<Leader>bn"] = { "<cmd>tabnew<cr>", desc = "New tab" },
        },
        v = {
          ["0"] = { "^", desc = "Goto Beginning" },
          ["<Leader>="] = {
            function() vim.lsp.buf.format(require("astrolsp").format_opts) end,
            desc = "Format buffer",
          },
        },
        x = {
          c = { '"_c' },
          p = {
            [[ 'pgv"'.v:register.'y' ]],
            expr = true,
            desc = "Paste without overriding clipboard",
          },
        },
        t = {
          ["<C-]>"] = { "<C-\\><C-n>", desc = "Goto Normal Mode" },
        },
      },
      autocmds = {
        dynamic_large_buf_settings = {
          {
            event = { "TextChanged", "TextChangedI", "BufEnter" },
            desc = "Detect buffers that become large after creation",
            callback = function(args)
              if vim.b[args.buf].large_buf or not is_large_buffer(args.buf) then return end
              apply_large_buffer_guard(args.buf)
              require("astrocore").event("LargeBuf", true)
            end,
          },
          {
            event = "User",
            pattern = "AstroLargeBuf",
            desc = "Apply local large buffer guardrails",
            callback = function(args) apply_large_buffer_guard(args.buf) end,
          },
        },
        fix_comment_continuation = {
          {
            event = "FileType",
            desc = "Fix Comment Continuation",
            callback = function() vim.opt_local.formatoptions = "jcrqlnt" end,
          },
        },
        -- first key is the augroup name
        terminal_settings = {
          -- the value is a list of autocommands to create
          {
            -- event is added here as a string or a list-like table of events
            event = "TermOpen",
            -- the rest of the autocmd options (:h nvim_create_autocmd)
            desc = "Disable line number/fold column/sign column for terminals",
            callback = function(ev)
              local bufnr = ev.buf
              vim.opt_local.number = false
              vim.opt_local.relativenumber = false
              vim.opt_local.foldcolumn = "0"
              vim.opt_local.signcolumn = "no"
              vim.opt_local.foldmethod = "manual"

              vim.keymap.set("t", "<C-]>", "<C-\\><C-n>", { buffer = bufnr, desc = "Goto normal mode" })
              -- vim.keymap.set("n", "A", "A<C-k>", { buffer = bufnr })
              -- vim.keymap.set("n", "D", "A<C-k><C-\\><C-n>", { buffer = bufnr })
              -- vim.keymap.set("n", "cc", "A<C-e><C-u>", { buffer = bufnr })
              -- vim.keymap.set("n", "dd", "A<C-e><C-u><C-\\><C-n>", { buffer = bufnr })
            end,
          },
        },
      },
      treesitter = {
        auto_install = true,
        textobjects = {
          swap = {
            swap_next = {
              ["<LocalLeader>k"] = { query = "@block.outer", desc = "Swap next block" },
              ["<LocalLeader>f"] = { query = "@function.outer", desc = "Swap next function" },
              ["<LocalLeader>a"] = { query = "@parameter.inner", desc = "Swap next argument" },
            },
            swap_previous = {
              ["<LocalLeader>K"] = { query = "@block.outer", desc = "Swap previous block" },
              ["<LocalLeader>F"] = { query = "@function.outer", desc = "Swap previous function" },
              ["<LocalLeader>A"] = { query = "@parameter.inner", desc = "Swap previous argument" },
            },
          },
        },
      },
      sessions = {
        -- Configure auto saving
        autosave = {
          last = true, -- auto save last session
          cwd = true, -- auto save session for each working directory
        },
        -- Patterns to ignore when saving sessions
        ignore = {
          dirs = {}, -- working directories to ignore sessions in
          filetypes = { "gitcommit", "gitrebase" }, -- filetypes to ignore sessions
          buftypes = {}, -- buffer types to ignore sessions
        },
      },
      commands = {
        Redir = {
          function(opts)
            local cmd = opts.args
            local output

            if cmd:match "^!" then
              -- Run shell command (strip !)
              local shell_cmd = cmd:sub(2)
              output = vim.split(vim.fn.system(shell_cmd), "\n", { trimempty = true })
            else
              -- Redirect built-in/ex command output
              local ok, result = pcall(vim.api.nvim_exec2, cmd, { output = true })
              if not result.output then return end
              output = ok and vim.split(result.output, "\n", { trimempty = false }) or { result }
            end

            -- Open new tab with scratch buffer
            vim.cmd "$tabnew"
            local buf = vim.api.nvim_get_current_buf()
            vim.api.nvim_set_option_value("buftype", "nofile", { buf = buf })
            vim.api.nvim_set_option_value("bufhidden", "wipe", { buf = buf })
            vim.api.nvim_set_option_value("swapfile", false, { buf = buf })
            vim.api.nvim_set_option_value("buflisted", false, { buf = buf })

            -- Show the command and separator
            vim.api.nvim_buf_set_lines(buf, 0, -1, false, { string.format("Command [[ %s ]] ----- Output ---->", cmd) })
            -- Populate lines
            vim.api.nvim_buf_set_lines(buf, 1, -1, false, output)
          end,
          nargs = 1,
          desc = "Redirect output of a command to scratch tab",
          complete = function(query) return vim.fn.getcompletion(query, "command") end,
        },
        RunMake = {
          function(opts)
            vim.cmd "update"
            local compiler = opts.fargs[1]
            vim.cmd("compiler " .. compiler)
            -- If there are more arguments, pass them to Make
            if #opts.fargs > 1 then
              -- Join remaining args and append to Make
              local make_args = table.concat(vim.list_slice(opts.fargs, 2), " ")
              vim.cmd("make " .. make_args)
            else
              vim.cmd "Make"
            end
          end,
          nargs = "+",
          complete = function(arg_lead, cmd_line)
            local parts = vim.split(cmd_line, "%s+")
            if #parts == 1 or (#parts == 2 and arg_lead == parts[2]) then
              return vim.tbl_filter(function(c) return vim.startswith(c, arg_lead) end, all_comps)
            else
              return vim.fn.getcompletion(arg_lead, "file")
            end
          end,
        },
        PrintConfig = {
          function(opts)
            local plugins = vim.tbl_keys(require("lazy.core.config").plugins)
            local args = opts.args
            local function callback(plugin_name)
              local cmd = "Redir lua =require('lazy.core.config').plugins['" .. plugin_name .. "']"
              vim.notify(cmd, vim.log.levels.INFO, { title = "Command" })
              vim.fn.execute(cmd)
            end
            if args ~= "" then
              callback(args)
              return
            end

            vim.ui.select(plugins, { prompt = "Select Config to print" }, function(item)
              if not item then return end
              callback(item)
            end)
          end,
          desc = "Print final lazy config",
          nargs = "?",
          complete = function(prefix)
            local plugins = vim.tbl_keys(require("lazy.core.config").plugins)
            return vim
              .iter(plugins)
              :filter(function(t)
                if string.len(prefix:gsub("%s+", "")) > 0 then return t:match(prefix) end
                return true
              end)
              :totable()
          end,
        },
      },
    },
  },
  {
    "AstroNvim/astrocore",
    ---@param _ any
    ---@param opts AstroCoreOpts
    opts = function(_, opts)
      for i = 1, 9 do
        local k = "<leader>" .. i
        opts.mappings.n[k] = { "<cmd>" .. i .. "tabnext<cr>", desc = "Goto Tab " .. i }
      end

      for _, key in ipairs { "h", "j", "k", "l" } do
        opts.mappings.n["<C-W>" .. key] = {
          function() require("better_window_navigation").navigate(key) end,
          desc = "Smart window navigation: " .. key,
        }
        opts.mappings.n["<C-" .. string.upper(key) .. ">"] = {
          function() require("better_window_navigation").navigate(key) end,
          desc = "Smart window navigation: " .. key,
        }
      end

      vim.opt.path:append "**"
      vim.opt.iskeyword:append "-"

      if vim.fn.has "nvim-0.11" == 1 and vim.fn.executable "fd" then vim.opt.findfunc = "v:lua.Fd" end
    end,
  },
}
