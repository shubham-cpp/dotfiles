local mini_files = vim.api.nvim_create_augroup("sp_mini_files", {})

local minifiles_toggle = function(...)
  local f = require "mini.files"
  local arg_len = ... == nil and 0 or #...
  if not f.close() then
    if arg_len == 0 then
      f.open(f.get_latest_path())
    else
      f.open(...)
    end
  end
end

---@type LazySpec
return {
  {
    "echasnovski/mini.files",
    dependencies = {
      {
        "s1n7ax/nvim-window-picker",
        version = "2.*",
        config = function()
          require("window-picker").setup {
            filter_rules = {
              include_current_win = false,
              autoselect_one = true,
              -- filter using buffer options
              bo = {
                -- if the file type is one of following, the window will be ignored
                filetype = { "neo-tree", "neo-tree-popup", "notify" },
                -- if the buffer type is one of following, the window will be ignored
                buftype = { "terminal", "quickfix" },
              },
            },
            --- @type 'statusline-winbar' | 'floating-big-letter'
            hint = "floating-big-letter",
          }
        end,
      },
      {
        "AstroNvim/astrocore",
        opts = {
          mappings = {
            n = {
              ["<Leader>e"] = { minifiles_toggle, desc = "Explorer" },
              ["<Leader>E"] = {
                function() minifiles_toggle(vim.api.nvim_buf_get_name(0), false) end,
                desc = "Explorer(current file)",
              },
            },
          },
        },
      },
    },
    opts = {
      options = {
        permanent_delete = false,
        use_as_default_explorer = true,
      },
      windows = {
        preview = true,
        width_focus = 30,
        width_preview = 30,
      },
    },
    config = function(_, opts)
      require("mini.files").setup(opts)

      local map_split = function(buf_id, lhs, direction, close_on_file)
        local rhs = function()
          local new_target_window
          local cur_target_window = require("mini.files").get_explorer_state().target_window
          if cur_target_window ~= nil then
            vim.api.nvim_win_call(cur_target_window, function()
              vim.cmd("belowright " .. direction .. " split")
              new_target_window = vim.api.nvim_get_current_win()
            end)

            require("mini.files").set_target_window(new_target_window)
            require("mini.files").go_in { close_on_file = close_on_file }
          end
        end

        local desc = "Open in " .. direction .. " split"
        if close_on_file then desc = desc .. " and close" end
        vim.keymap.set("n", lhs, rhs, { buffer = buf_id, desc = desc })
      end

      local function open_in_window_picker()
        local f = require "mini.files"
        local fs_entry = f.get_fs_entry()
        if fs_entry ~= nil and fs_entry.fs_type == "file" then
          local picked_window_id = require("window-picker").pick_window()
          if picked_window_id == nil then return end
          f.set_target_window(picked_window_id)
        end
        f.go_in {
          close_on_file = true,
        }
      end

      vim.api.nvim_create_autocmd("User", {
        pattern = "MiniFilesBufferCreate",
        desc = "Setup mappings for MiniFiles",
        group = mini_files,
        callback = function(args)
          local buf_id = args.data.buf_id

          map_split(buf_id, "gs", "horizontal", false)
          map_split(buf_id, "gv", "vertical", false)

          map_split(buf_id, "<C-w>s", "horizontal", false)
          map_split(buf_id, "<C-w>v", "vertical", false)
          map_split(buf_id, "<C-w>S", "horizontal", true)
          map_split(buf_id, "<C-w>V", "vertical", true)

          vim.keymap.set("n", "gw", open_in_window_picker, { buffer = buf_id, desc = "Open in target window" })
        end,
      })
      vim.api.nvim_create_autocmd("User", {
        pattern = "MiniFilesActionRename",
        desc = "LSP renamed file",
        group = mini_files,
        callback = function(event)
          local ok, snacks = pcall(require, "snacks")
          if ok then snacks.rename.on_rename_file(event.data.from, event.data.to) end
        end,
      })
    end,
  },
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
    keys = { "ga", "gA" },
    opts = {},
    -- config = function()
    --   require('mini.align').setup({})
    -- end,
  },
}
