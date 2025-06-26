---@type LazySpec
return {
  "lewis6991/gitsigns.nvim",
  event = "VeryLazy",
  opts = {
    on_attach = function(bufnr)
      local function map(lhs, rhs, desc, mode)
        vim.keymap.set(mode or "n", "<leader>g" .. lhs, rhs, {
          buffer = bufnr,
          desc = desc,
        })
      end
      map("l", function()
        require("gitsigns").blame_line()
      end, "View Git blame")
      map("L", function()
        require("gitsigns").blame_line { full = true }
      end, "View full Git blame")
      map("hp", function()
        require("gitsigns").preview_hunk_inline()
      end, "Preview Git hunk")

      map("hr", function()
        require("gitsigns").reset_hunk()
      end, "Reset Git hunk")
      map("hr", function()
        require("gitsigns").reset_hunk { vim.fn.line ".", vim.fn.line "v" }
      end, "Reset Git hunk", "v")
      map("hR", function()
        require("gitsigns").reset_buffer()
      end, "Reset Git buffer")

      map("hs", function()
        require("gitsigns").stage_hunk()
      end, "Stage Git hunk")
      map("hs", function()
        require("gitsigns").stage_hunk { vim.fn.line ".", vim.fn.line "v" }
      end, "Stage Git hunk", "v")
      map("hS", function()
        require("gitsigns").stage_buffer()
      end, "Stage Git buffer")

      map("d", function()
        require("gitsigns").diffthis()
      end, "View Git diff")

      vim.keymap.set("n", "[G", function()
        require("gitsigns").nav_hunk "first"
      end, { buffer = bufnr, desc = "First Git hunk" })
      vim.keymap.set("n", "]G", function()
        require("gitsigns").nav_hunk "last"
      end, { buffer = bufnr, desc = "Last Git hunk" })
      vim.keymap.set("n", "]g", function()
        require("gitsigns").nav_hunk "next"
      end, { buffer = bufnr, desc = "Next Git hunk" })
      vim.keymap.set("n", "[g", function()
        require("gitsigns").nav_hunk "prev"
      end, { buffer = bufnr, desc = "Previous Git hunk" })

      vim.keymap.set({ "o", "x" }, "ig", ":<C-U>Gitsigns select_hunk<CR>", { desc = "inside Git hunk", buffer = bufnr })
    end,
  },
}
