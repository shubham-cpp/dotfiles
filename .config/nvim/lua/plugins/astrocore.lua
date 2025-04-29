---@type LazySpec
return {
  "AstroNvim/astrocore",
  ---@type AstroCoreOpts
  opts = {
    -- Configure core features of AstroNvim
    features = {
      large_buf = { size = 1024 * 256, lines = 5000 }, -- set global limits for large files for disabling features like treesitter
      highlighturl = true, -- highlight URLs at start
    },
    -- Diagnostics configuration (for vim.diagnostics.config({...})) when diagnostics are on
    diagnostics = {
      virtual_text = { current_line = true },
      underline = true,
    },
    -- passed to `vim.filetype.add`
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
      -- first key is the mode
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
  },
}
