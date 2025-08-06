function _G.Fd(file_pattern, _)
  -- if first char is * then fuzzy search
  if file_pattern:sub(1, 1) == "*" then file_pattern = file_pattern:gsub(".", ".*%0") .. ".*" end
  local cmd = 'fd  --color=never --full-path --type file "' .. file_pattern .. '"'
  local result = vim.fn.systemlist(cmd)
  return result
end

---@type LazySpec
return {
  {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      -- Configure core features of AstroNvim
      features = {
        large_buf = { size = false, lines = false }, -- set global limits for large files for disabling features like treesitter
        --   autopairs = true, -- enable autopairs at start
        --   cmp = true, -- enable completion at start
        --   diagnostics_mode = 3, -- diagnostic mode on start (0 = off, 1 = no signs/virtual text, 2 = no virtual text, 3 = on)
        --   highlighturl = true, -- highlight URLs at start
        --   notifications = true, -- enable notifications at start
      },
      -- Diagnostics configuration (for vim.diagnostics.config({...})) when diagnostics are on
      diagnostics = {
        virtual_text = true,
        underline = true,
      },
      -- vim options can be configured here
      options = {
        opt = { -- vim.opt.<key>
          relativenumber = true, -- sets vim.opt.relativenumber
          number = true, -- sets vim.opt.number
          spell = false, -- sets vim.opt.spell
          signcolumn = "yes", -- sets vim.opt.signcolumn to yes
          wrap = true, -- sets vim.opt.wrap
          undolevels = 10000,
          exrc = true, -- allows to create project specific settings
          wildmode = "longest:full,full",
          sessionoptions = { "blank", "buffers", "curdir", "globals", "help", "tabpages", "winsize", "terminal" },
          smoothscroll = true,
          grepprg = "rg --vimgrep --smart-case",
          scrolloff = 8,
        },
        g = { -- vim.g.<key>
          markdown_recommended_style = 0,
        },
      },
      -- Mappings can be configured through AstroCore as well.
      -- NOTE: keycodes follow the casing in the vimdocs. For example, `<Leader>` must be capitalized
      mappings = {
        n = {
          ["\\"] = false,
          ["<leader>c"] = false,
          [",w"] = { "<cmd>w!<cr>", desc = "Save File" },
          [",W"] = { "<cmd>noautocmd w!<cr>", desc = "Save File(noautocmd)" },
          ["0"] = { "^", desc = "Goto Beginning" },
          dl = { '"_dl' },
          c = { '"_c' },
          C = { '"_C' },
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
          ["<leader>R"] = {
            function()
              vim.cmd "source %"
              local file = vim.fn.substitute(vim.fn.expand "%:r", "lua/", "", "")
              local ok, mod = pcall(require, file)
              if ok and type(mod) ~= "boolean" and next(mod or {}) ~= nil and mod.config then mod.config() end
            end,
            silent = false,
            desc = "Reload module",
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
        o = {
          ie = { ':exec "normal! ggVG"<cr>', desc = "Entire File" },
          iv = { ':exec "normal! HVL"<cr>', desc = "Entire Visible screen" },
        },
      },
      filetypes = {
        filename = {
          vimfrc = "vim",
          dwm_sxhkdrc = "sxhkdrc",
          [".env"] = "conf",
          [".env.*"] = "conf",
          ["package.json"] = "jsonc",
        },
        pattern = {
          ["*profile"] = "sh",
          ["*.postcss"] = "css",
          ["*.kbd"] = "lisp",
          [".eslintrc"] = "jsonc",
          ["tsconfig.*.json"] = "jsonc",
          [".*/waybar/config"] = "jsonc",
          [".*/kitty/.+%.conf"] = "kitty",
          [".*/hypr/.+%.conf"] = "hyprlang",
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
            vim.cmd("compiler " .. opts.args)
            vim.cmd "Make"
          end,
          nargs = 1,
          complete = function(arg_lead)
            local rtps = vim.api.nvim_list_runtime_paths()
            local comps = {}
            for _, p in ipairs(rtps) do
              for _, f in ipairs(vim.fn.globpath(p, "compiler/*.vim", 0, 1)) do
                table.insert(comps, vim.fn.fnamemodify(f, ":t:r"))
              end
            end
            return vim.tbl_filter(function(c) return vim.startswith(c, arg_lead) end, comps)
          end,
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

      vim.opt.path:append "**"
      vim.opt.iskeyword:append "-"

      if vim.fn.has "nvim-0.11" == 1 and vim.fn.executable "fd" then vim.opt.findfunc = "v:lua.Fd" end
    end,
  },
}
