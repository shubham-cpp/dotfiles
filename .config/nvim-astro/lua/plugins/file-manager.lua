---@type LazySpec
return {
  {
    "mikavilpas/yazi.nvim",
    enabled = true,
    dependencies = { "folke/snacks.nvim" },
    ---@type YaziConfig | {}
    opts = {
      open_for_directories = false,
    },
    keys = {
      {
        "<leader>-",
        mode = { "n", "v" },
        "<cmd>Yazi<cr>",
        desc = "Open yazi at the current file",
      },
      {
        "<leader>_",
        "<cmd>Yazi cwd<cr>",
        desc = "Resume the last yazi session",
      },
    },
  },
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
                  local buf_id = args.data.buf_id
                  local files = require "mini.files"

                  vim.keymap.set("n", "W", function()
                    local win_id = require("window-picker").pick_window()
                    if win_id then
                      files.set_target_window(win_id)
                      files.go_in { close_on_file = true }
                    end
                  end, { desc = "Select window", buffer = buf_id })
                  vim.keymap.set("n", "gw", function()
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
                  local ok, picker = pcall(require, "snacks.picker")

                  if not ok then return end

                  vim.keymap.set("n", "gf", function()
                    local entry = files.get_fs_entry() or {}
                    files.close()

                    local cwd = entry.path
                    if not cwd then vim.notify("Invalid path", vim.log.levels.ERROR, { title = "MiniFiles" }) end
                    if entry.fs_type == "file" then cwd = vim.fs.dirname(entry.path) end

                    picker.files { cwd = cwd, layout = { preset = "vscode" } }
                  end, { desc = "Files Dir", buffer = buf_id })

                  vim.keymap.set("n", "gs", function()
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
