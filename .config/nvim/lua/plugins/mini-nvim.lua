---@type LazySpec
return {
  {
    "echasnovski/mini.operators",
    keys = {
      { "g=", mode = { "n", "x" }, desc = "Evalute" },
      { "ge", mode = { "n", "x" }, desc = "Exchange" },
      { "gm", mode = { "n", "x" }, desc = "Duplicate" },
      { "x", mode = { "n", "x" }, desc = "Replace with register" },
      { "gS", mode = { "n", "x" }, desc = "Sort" },
      "X",
    },
    opts = {
      -- Exchange text regions
      exchange = { prefix = "ge" },
      replace = { prefix = "x" },
      sort = { prefix = "gS" },
    },
    config = function(_, opts)
      require("mini.operators").setup(opts)
      vim.keymap.set("n", "X", "x$", { desc = "Replace to end of line", remap = true })
    end,
  },
  {
    "echasnovski/mini.move",
    keys = {
      { "<", mode = { "n", "v" } },
      { ">", mode = { "n", "v" } },
      { "J", mode = "v" },
      { "K", mode = "v" },
    },
    opts = {
      mappings = {
        -- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
        left = "<",
        right = ">",
        down = "J",
        up = "K",

        -- Move current line in Normal mode
        line_left = "<",
        line_right = ">",
        line_down = "<M-j>",
        line_up = "<M-k>",
      },
    },
    -- config = function(_, opts) require("mini.move").setup(opts) end,
  },
  {
    "echasnovski/mini.align",
    keys = { { "ga", mode = { "n", "x" } }, { "gA", mode = { "n", "x" } } },
    opts = {},
  },
  { "echasnovski/mini.extra", opts = {}, lazy = true },
  {
    "echasnovski/mini.ai",
    dependencies = { "echasnovski/mini.extra" },
    event = "User AstroFile",
    opts = function()
      local ai = require "mini.ai"
      local gen_ai_spec = require("mini.extra").gen_ai_spec
      return {
        n_lines = 500,
        custom_textobjects = {
          o = ai.gen_spec.treesitter { -- code block
            a = { "@block.outer", "@conditional.outer", "@loop.outer" },
            i = { "@block.inner", "@conditional.inner", "@loop.inner" },
          },
          f = ai.gen_spec.treesitter { a = "@function.outer", i = "@function.inner" }, -- function
          c = ai.gen_spec.treesitter { a = "@class.outer", i = "@class.inner" }, -- class
          t = { "<([%p%w]-)%f[^<%w][^<>]->.-</%1>", "^<.->().*()</[^/]->$" }, -- tags
          d = gen_ai_spec.number(), -- digits
          e = { -- Word with case
            { "%u[%l%d]+%f[^%l%d]", "%f[%S][%l%d]+%f[^%l%d]", "%f[%P][%l%d]+%f[^%l%d]", "^[%l%d]+%f[^%l%d]" },
            "^().*()$",
          },
          g = gen_ai_spec.buffer(), -- buffer
          B = gen_ai_spec.buffer(),
          D = gen_ai_spec.diagnostic(),
          u = ai.gen_spec.function_call(), -- u for "Usage"
          U = ai.gen_spec.function_call { name_pattern = "[%w_]" }, -- without dot in function name
        },
      }
    end,
    specs = {
      {
        "catppuccin",
        optional = true,
        ---@type CatppuccinOptions
        opts = { integrations = { mini = true } },
      },
    },
  },
  { "JoosepAlviste/nvim-ts-context-commentstring", enabled = true },
  {
    "echasnovski/mini.files",
    optional = true,
    lazy = true,
    opts = {
      options = { permanent_delete = false },
      mappings = {
        go_in = "L",
        go_in_plus = "l",
      },
    },
    dependencies = {
      {
        "s1n7ax/nvim-window-picker",
        name = "window-picker",
        lazy = true,
        version = "2.*",
        opts = {
          -- hint = "floating-big-letter",
          -- selection_chars = 'FJDKSLA;CMRUEIWOQP',
          selection_chars = "1234567890",
          picker_config = {
            handle_mouse_click = true,
            statusline_winbar_picker = {
              selection_display = function(char) return "%=" .. "%#Underlined#" .. char .. "%*" .. string.rep(" ", 16) end,
            },
          },
          highlights = {
            enabled = true,
            winbar = {
              focused = {
                fg = "#fefefe",
                bg = "#252530",
                bold = true,
              },
              unfocused = {
                fg = "#fefefe",
                bg = "#252530",
                bold = true,
              },
            },
          },
        },
      },
    },
    specs = {
      {
        "AstroNvim/astrocore",
        ---@type AstroCoreOpts
        opts = {
          mappings = {
            n = {
              ["<Leader>e"] = {
                function()
                  if not require("mini.files").close() then require("mini.files").open(vim.api.nvim_buf_get_name(0)) end
                end,
                desc = "Explorer",
              },
              ["<Leader>E"] = {
                function()
                  if not require("mini.files").close() then require("mini.files").open() end
                end,
                desc = "Explorer(cwd)",
              },
            },
          },
          autocmds = {
            mini_files_custom_bindings = {
              {
                event = "User",
                pattern = "MiniFilesBufferCreate",
                desc = "Create mappings to select target window",
                callback = function(args)
                  local mini_files_opts = require("astrocore").plugin_opts "mini.files"
                  local mappings = mini_files_opts.mappings or {}
                  local buf_id = args.data.buf_id
                  local files = require "mini.files"

                  vim.keymap.set("n", mappings.pick_window_close or "W", function()
                    local win_id = require("window-picker").pick_window()
                    if win_id then
                      files.set_target_window(win_id)
                      files.go_in { close_on_file = true }
                    end
                  end, { desc = "Select window", buffer = buf_id })
                  vim.keymap.set("n", mappings.pick_window or "gw", function()
                    local win_id = require("window-picker").pick_window()
                    if win_id then
                      files.set_target_window(win_id)
                      files.go_in()
                    end
                  end, { desc = "Select window(no close)", buffer = buf_id })
                end,
              },
              {
                event = "User",
                pattern = "MiniFilesBufferUpdate",
                desc = "Integrate with picker",
                callback = function(args)
                  local buf_id = args.data.buf_id
                  local files = require "mini.files"
                  local mini_files_opts = require("astrocore").plugin_opts "mini.files"
                  local mappings = mini_files_opts.mappings or {}
                  local ok, picker = pcall(require, "snacks.picker")

                  if not ok then return end

                  vim.keymap.set("n", mappings.find_files_in_dir or "<leader>f", function()
                    local entry = files.get_fs_entry() or {}
                    files.close()

                    local cwd = entry.path
                    if not cwd then vim.notify("Invalid path", vim.log.levels.ERROR, { title = "MiniFiles" }) end
                    if entry.fs_type == "file" then cwd = vim.fs.dirname(entry.path) end

                    picker.files { cwd = cwd, layout = { preset = "vscode" } }
                  end, { desc = "Files Dir", buffer = buf_id })

                  vim.keymap.set("n", mappings.grep_in_dir or "<leader>s", function()
                    local entry = files.get_fs_entry() or {}
                    files.close()

                    local cwd = entry.path
                    if not cwd then vim.notify("Invalid path", vim.log.levels.ERROR, { title = "MiniFiles" }) end
                    if entry.fs_type == "file" then cwd = vim.fs.dirname(entry.path) end

                    picker.grep { cwd = cwd }
                  end, { desc = "Grep dir", buffer = buf_id })
                end,
              },
            },
          },
        },
      },
    },
  },
}
