---@type LazySpec
return {
  "lewis6991/gitsigns.nvim",
  event = "BufRead",
  opts = {
    on_attach = function(bufnr)
      local gs = package.loaded.gitsigns

      local function map(mode, l, r, opts)
        opts = opts or {}
        opts.buffer = bufnr
        vim.keymap.set(mode, l, r, opts)
      end

      -- Navigation
      map("n", "]c", function()
        if vim.wo.diff then return "]c" end
        vim.schedule(function() gs.next_hunk() end)
        return "<Ignore>"
      end, { expr = true, desc = "Next Hunk" })

      map("n", "[c", function()
        if vim.wo.diff then return "[c" end
        vim.schedule(function() gs.prev_hunk() end)
        return "<Ignore>"
      end, { expr = true, desc = "Prev Hunk" })

      map("n", "<leader>gr", gs.reset_hunk, { desc = "Reset Hunk" })
      map(
        "v",
        "<leader>gr",
        function() gs.reset_hunk({ vim.fn.line ".", vim.fn.line "v" }) end,
        { desc = "Reset Hunk" }
      )
      map("n", "<leader>gS", gs.stage_hunk, { desc = "Stage Hunk" })
      map(
        "v",
        "<leader>gS",
        function() gs.stage_hunk({ vim.fn.line ".", vim.fn.line "v" }) end,
        { desc = "Stage Hunk" }
      )
      map("n", "<leader>gR", gs.reset_buffer, { desc = "Reset Buffer" })
      map("n", "<leader>gp", gs.preview_hunk, { desc = "Preview Hunk" })
      map("n", "<leader>gL", gs.blame_line, { desc = "Blame" })
      map("n", "<leader>gd", gs.diffthis, { desc = "Diffthis" })

      -- Text object
      map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", { desc = "Inside hunk" })
    end,
  },
}
