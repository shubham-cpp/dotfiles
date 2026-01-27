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
---@type LazySpec
return {
  {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      -- Configure core features of AstroNvim
      features = {
        large_buf = { size = 1024 * 25, lines = 5000 }, -- set global limits for large files for disabling features like treesitter
        autopairs = true, -- enable autopairs at start
        cmp = true, -- enable completion at start
        diagnostics = { virtual_text = true, virtual_lines = false }, -- diagnostic settings on startup
        highlighturl = true, -- highlight URLs at start
        notifications = true, -- enable notifications at start
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
          [".env*"] = "conf",
        },
        pattern = {
          ["tsconfig*.json"] = "jsonc",
          [".*/kitty/.+%.conf"] = "kitty",
        },
      },
      -- vim options can be configured here
      options = {
        opt = {
          undolevels = 10000,
          wrap = true, -- sets vim.opt.wrap
          exrc = true, -- allows to create project specific settings
          sessionoptions = { "blank", "buffers", "curdir", "globals", "help", "tabpages", "winsize", "terminal" },
          smoothscroll = true,
          grepprg = vim.fn.executable "rg" == 1 and "rg --vimgrep --smart-case --no-heading --sort=path"
            or vim.opt.grepprg,
          scrolloff = 8,
          splitkeep = "topline",
          jumpoptions = "stack",
        },
        g = { -- vim.g.<key>
          markdown_recommended_style = 0,
          tsc_makeprg = "npx tsc",
        },
      },
      mappings = {
        -- first key is the mode
        n = {
          [","] = false,
          ["\\"] = false,
          ["<leader>c"] = false,
          ["0"] = { "^", desc = "Goto Beginning" },
          dl = { '"_dl' },
          c = { '"_c' },
          C = { '"_C' },
          [",w"] = { "<cmd>w!<cr>", desc = "Save File" },
          [",W"] = { "<cmd>noautocmd w!<cr>", desc = "Save File(autocmd)" },
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
      },
      autocmds = {
        fix_comment_continuation = {
          {
            event = "FileType",
            desc = "Fix Comment Continuation",
            callback = function() vim.opt_local.formatoptions = "jcrqlnt" end,
          },
        },
      },
      commands = {
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
              vim.cmd("Make " .. make_args)
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
        BetterWinNavClearHistory = {
          require("config.better_window_navigation").clear_history,
          nargs = 0,
          desc = "Clear the window navigation history for the current tab",
        },
        Make = {
          function(params)
            -- Insert args at the '$*' in the makeprg
            local cmd, num_subs = vim.o.makeprg:gsub("%$%*", params.args)
            if num_subs == 0 then cmd = cmd .. " " .. params.args end
            local task = require("overseer").new_task {
              cmd = vim.fn.expandcmd(cmd),
              components = {
                { "on_output_quickfix", open = not params.bang, open_height = 8 },
                "default",
              },
            }
            task:start()
          end,
          desc = "Run your makeprg as an Overseer task",
          nargs = "*",
          bang = true,
        },
      },
    },
  },
  {
    "AstroNvim/astrocore",
    optional = true,
    ---@param _ any
    ---@param opts AstroCoreOpts
    opts = function(_, opts)
      for i = 1, 9 do
        local k = "<leader>" .. i
        opts.mappings.n[k] = { "<cmd>" .. i .. "tabnext<cr>", desc = "Goto Tab " .. i }
      end

      for _, key in ipairs { "h", "j", "k", "l" } do
        opts.mappings.n["<C-W>" .. key] = {
          function() require("config.better_window_navigation").navigate(key) end,
          desc = "Smart window navigation: " .. key,
        }
        opts.mappings.n["<C-" .. string.upper(key) .. ">"] = {
          function() require("config.better_window_navigation").navigate(key) end,
          desc = "Smart window navigation: " .. key,
        }
      end

      vim.opt.path:append "**"
      vim.opt.iskeyword:append "-"

      if vim.fn.has "nvim-0.11" == 1 and vim.fn.executable "fd" then vim.opt.findfunc = "v:lua.Fd" end
    end,
  },
}
