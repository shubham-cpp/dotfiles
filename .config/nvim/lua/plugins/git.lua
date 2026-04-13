return {
  {
    url = "NeogitOrg/neogit",
    config = function()
      require("neogit").setup({
        graph_style = "unicode",
        kind = "floating",
        -- integrations = {
        --   diffview = true,
        -- },
        commit_editor = {
          staged_diff_split = "split",
        },
      })
    end,
    keys = {
      { "<leader>gn", "<cmd>Neogit kind=floating<cr>", desc = "Neogit" },
    },
  },

  {
    url = "lewis6991/gitsigns.nvim",
    config = function()
      require("gitsigns").setup({
        signs = {
          add = { text = "│" },
          change = { text = "│" },
          delete = { text = "_" },
          topdelete = { text = "‾" },
          changedelete = { text = "~" },
          untracked = { text = "┆" },
          stash = { text = "$" },
        },
        signs_staged = {
          add = { text = "│" },
          change = { text = "│" },
          delete = { text = "_" },
          topdelete = { text = "‾" },
          changedelete = { text = "~" },
        },
        current_line_blame = false,
        on_attach = function(bufnr)
          if vim.b[bufnr].bigfile then
            return false
          end

          local gs = require("gitsigns")

          local function map(mode, l, r, opts)
            opts = vim.tbl_extend("force", { buffer = bufnr, desc = opts and opts.desc }, opts or {})
            vim.keymap.set(mode, l, r, opts)
          end

          -- Navigation
          map("n", "]g", function()
            if vim.wo.diff then
              return "]g"
            end
            vim.schedule(function()
              gs.nav_hunk("next")
            end)
            return "<Ignore>"
          end, { expr = true, desc = "Next hunk" })

          map("n", "[g", function()
            if vim.wo.diff then
              return "[g"
            end
            vim.schedule(function()
              gs.nav_hunk("prev")
            end)
            return "<Ignore>"
          end, { expr = true, desc = "Prev hunk" })

          map("n", "]G", function()
            gs.nav_hunk("first")
          end, { desc = "First hunk" })

          map("n", "[G", function()
            gs.nav_hunk("last")
          end, { desc = "Last hunk" })

          -- Hunk actions (<leader>gh)
          map({ "n", "v" }, "<leader>ghs", function()
            gs.stage_hunk({ vim.fn.line("."), vim.fn.virtcol(".") })
          end, { desc = "Stage hunk" })

          map({ "n", "v" }, "<leader>ghr", function()
            gs.reset_hunk({ vim.fn.line("."), vim.fn.virtcol(".") })
          end, { desc = "Reset hunk" })

          map("n", "<leader>ghS", gs.stage_buffer, { desc = "Stage buffer" })
          map("n", "<leader>ghR", gs.reset_buffer, { desc = "Reset buffer" })
          map("n", "<leader>ghp", gs.preview_hunk, { desc = "Preview hunk" })
          map("n", "<leader>ghi", gs.preview_hunk_inline, { desc = "Preview hunk inline" })

          map("n", "<leader>ghd", gs.diffthis, { desc = "Diff this" })
          map("n", "<leader>ghD", function()
            gs.diffthis("~")
          end, { desc = "Diff this (~)" })

          map("n", "<leader>ghq", gs.setqflist, { desc = "Quickfix hunks" })

          -- Blame & toggles (<leader>g)
          map("n", "<leader>gl", function()
            gs.blame_line({ full = true })
          end, { desc = "Blame line" })

          map("n", "<leader>gL", gs.toggle_current_line_blame, { desc = "Toggle line blame" })
          map("n", "<leader>gD", gs.toggle_deleted, { desc = "Toggle deleted" })

          -- Text object
          map({ "o", "x" }, "ig", "<Cmd>Gitsigns select_hunk<CR>", { desc = "Select hunk" })
        end,
      })
    end,
  },

  {
    url = "spacedentist/resolve.nvim",
    config = function()
      require("resolve").setup({
        default_keymaps = false,
      })
    end,
    keys = {
      { "<leader>co", "<Plug>(resolve-ours)", desc = "Resolve: ours" },
      { "<leader>ct", "<Plug>(resolve-theirs)", desc = "Resolve: theirs" },
      { "<leader>cb", "<Plug>(resolve-both)", desc = "Resolve: both" },
      { "<leader>cn", "<Plug>(resolve-none)", desc = "Resolve: none" },
      { "]x", "<Plug>(resolve-next)", desc = "Next conflict" },
      { "[x", "<Plug>(resolve-prev)", desc = "Prev conflict" },
    },
  },
}
