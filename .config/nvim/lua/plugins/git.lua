return {
  {
    "dlyongemallo/diffview.nvim",
    cmd = { "DiffviewOpen", "DiffviewFileHistory", "DiffviewClose" },
    keys = {
      { "<leader>gd", "<cmd>DiffviewOpen<cr>", desc = "Diff open" },
      { "<leader>gH", "<cmd>DiffviewFileHistory %<cr>", desc = "File history" },
      { "<leader>gF", "<cmd>DiffviewFileHistory<cr>", desc = "Repo history" },
    },
    opts = { enhanced_diff_hl = true },
    config = function(_, opts)
      local actions = require("diffview.actions")
      opts.view = {
        default = { layout = "diff2_horizontal" },
        merge_tool = { layout = "diff3_horizontal" },
        file_history = { layout = "diff2_horizontal" },
      }
      opts.keymaps = {
        file_panel = { { "n", "q", actions.close, { desc = "DiffviewClose" } } },
        file_history_panel = { { "n", "q", actions.close, { desc = "DiffviewClose" } } },
      }
      require("diffview").setup(opts)
    end,
  },
  {
    "NeogitOrg/neogit",
    dependencies = {"dlyongemallo/diffview.nvim"},
    keys = {
      { "<leader>gn", "<cmd>Neogit kind=tab<cr>", desc = "Neogit" },
    },
    opts = {
      graph_style = "unicode",
      kind = "floating",
      integrations = { diffview = true },
      commit_editor = { staged_diff_split = "split" },
    },
  },
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require("gitsigns").setup({
        signs = {
          add = { text = "│" }, change = { text = "│" }, delete = { text = "_" },
          topdelete = { text = "‾" }, changedelete = { text = "~" },
          untracked = { text = "┆" }, stash = { text = "$" },
        },
        signs_staged = {
          add = { text = "│" }, change = { text = "│" }, delete = { text = "_" },
          topdelete = { text = "‾" }, changedelete = { text = "~" },
        },
        current_line_blame = false,
        on_attach = function(bufnr)
          if vim.b[bufnr].bigfile then return false end
          local gs = require("gitsigns")
          local function map(mode, l, r, map_opts)
            map_opts = vim.tbl_extend("force", { buffer = bufnr, desc = map_opts and map_opts.desc }, map_opts or {})
            vim.keymap.set(mode, l, r, map_opts)
          end
          map("n", "]g", function()
            if vim.wo.diff then return "]g" end
            vim.schedule(function() gs.nav_hunk("next") end)
            return "<Ignore>"
          end, { expr = true, desc = "Next hunk" })
          map("n", "[g", function()
            if vim.wo.diff then return "[g" end
            vim.schedule(function() gs.nav_hunk("prev") end)
            return "<Ignore>"
          end, { expr = true, desc = "Prev hunk" })
          map("n", "]G", function() gs.nav_hunk("first") end, { desc = "First hunk" })
          map("n", "[G", function() gs.nav_hunk("last") end, { desc = "Last hunk" })
          map("n", "<leader>ghs", gs.stage_hunk, { desc = "Stage hunk" })
          map("v", "<leader>ghs", function() gs.stage_hunk({ vim.fn.line("."), vim.fn.virtcol(".") }) end, { desc = "Stage hunk" })
          map("n", "<leader>ghr", gs.reset_hunk, { desc = "Reset hunk" })
          map("v", "<leader>ghr", function() gs.reset_hunk({ vim.fn.line("."), vim.fn.virtcol(".") }) end, { desc = "Reset hunk" })
          map("n", "<leader>ghS", gs.stage_buffer, { desc = "Stage buffer" })
          map("n", "<leader>ghR", gs.reset_buffer, { desc = "Reset buffer" })
          map("n", "<leader>ghp", gs.preview_hunk, { desc = "Preview hunk" })
          map("n", "<leader>ghi", gs.preview_hunk_inline, { desc = "Preview hunk inline" })
          map("n", "<leader>ghd", gs.diffthis, { desc = "Diff this" })
          map("n", "<leader>ghD", function() gs.diffthis("~") end, { desc = "Diff this (~)" })
          map("n", "<leader>ghq", gs.setqflist, { desc = "Quickfix hunks" })
          map("n", "<leader>gl", function() gs.blame_line({ full = true }) end, { desc = "Blame line" })
          map("n", "<leader>gL", gs.toggle_current_line_blame, { desc = "Toggle line blame" })
          map("n", "<leader>gD", gs.toggle_deleted, { desc = "Toggle deleted" })
          map({ "o", "x" }, "ig", "<Cmd>Gitsigns select_hunk<CR>", { desc = "Select hunk" })
        end,
      })
    end,
  },
}
