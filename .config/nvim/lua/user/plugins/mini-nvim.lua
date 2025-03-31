local mini_group = vim.api.nvim_create_augroup("sp_mini_group", { clear = true })
local function map_split(buf_id, lhs, direction)
  local files = require "mini.files"
  local rhs = function()
    -- Make new window and set it as target
    local cur_target = files.get_explorer_state().target_window
    local new_target = vim.api.nvim_win_call(cur_target, function()
      vim.cmd(direction .. " split")
      return vim.api.nvim_get_current_win()
    end)

    files.set_target_window(new_target)
    files.go_in({ close_on_file = true })
    -- This intentionally doesn't act on file under cursor in favor of
    -- explicit "go in" action (`l` / `L`). To immediately open file,
    -- add appropriate `MiniFiles.go_in()` call instead of this comment.
  end

  -- Adding `desc` will result into `show_help` entries
  local desc = "Split " .. direction
  vim.keymap.set("n", lhs, rhs, { buffer = buf_id, desc = desc })
end

---@type LazySpec
return {
  { "echasnovski/mini.extra", lazy = true, opts = {} },
  { "echasnovski/mini.align", keys = { "ga", "gA" }, opts = {} },
  {
    "echasnovski/mini.icons",
    opts = {},
    config = function(_, opts)
      local icons = require "mini.icons"
      icons.setup(opts)
      icons.mock_nvim_web_devicons()
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
    "echasnovski/mini.ai",
    event = "BufWinEnter",
    enabled = true,
    dependencies = { "echasnovski/mini.extra", "nvim-treesitter" },
    opts = function()
      local ai = require "mini.ai"
      local gen_ai_spec = require("mini.extra").gen_ai_spec
      return {
        n_lines = 500,
        custom_textobjects = {
          o = ai.gen_spec.treesitter({ -- code block
            a = { "@block.outer", "@conditional.outer", "@loop.outer" },
            i = { "@block.inner", "@conditional.inner", "@loop.inner" },
          }),
          f = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }), -- function
          c = ai.gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" }), -- class
          t = { "<([%p%w]-)%f[^<%w][^<>]->.-</%1>", "^<.->().*()</[^/]->$" }, -- tags
          e = gen_ai_spec.buffer(),
          D = gen_ai_spec.diagnostic(),
          L = gen_ai_spec.line(),
        },
      }
    end,
  },
  {
    "echasnovski/mini.files",
    enabled = true,
    dependencies = { "echasnovski/mini.icons", "AstroNvim/astrolsp" },
    opts = function()
      vim.api.nvim_create_autocmd("User", {
        group = mini_group,
        desc = "Create mappings to modify target window via split",
        pattern = "MiniFilesBufferCreate",
        callback = function(args)
          local buf_id = args.data.buf_id

          map_split(buf_id, "<C-w>s", "belowright horizontal")
          map_split(buf_id, "<C-w>v", "belowright vertical")
          map_split(buf_id, "<C-w>t", "tab")

          map_split(buf_id, "gs", "belowright horizontal")
          map_split(buf_id, "gv", "belowright vertical")
          map_split(buf_id, "gt", "tab")
        end,
      })
      vim.api.nvim_create_autocmd("User", {
        group = mini_group,
        desc = "execute `didCreateFiles` operation when creating files",
        pattern = "MiniFilesActionCreate",
        callback = function(args) require("astrolsp.file_operations").didCreateFiles(args.data.to) end,
      })
      vim.api.nvim_create_autocmd("User", {
        group = mini_group,
        desc = "execute `didDeleteFiles` operation when deleting files",
        pattern = "MiniFilesActionDelete",
        callback = function(args) require("astrolsp.file_operations").didDeleteFiles(args.data.from) end,
      })
      vim.api.nvim_create_autocmd("User", {
        group = mini_group,
        desc = "execute `didRenameFiles` operation when moving or renaming files",
        pattern = { "MiniFilesActionMove", "MiniFilesActionRename" },
        callback = function(args) require("astrolsp.file_operations").didRenameFiles(args.data) end,
      })
      return {
        options = { permanent_delete = false, use_as_default_explorer = false },
        windows = { preview = true },
      }
    end,
    keys = {
      {
        "<leader>E",
        function()
          if not require("mini.files").close() then require("mini.files").open(vim.api.nvim_buf_get_name(0)) end
        end,
        desc = "Explorer",
      },
      -- {
      --   "<leader>e",
      --   function()
      --     if not require("mini.files").close() then require("mini.files").open() end
      --   end,
      --   desc = "Explorer",
      -- },
    },
  },
}
