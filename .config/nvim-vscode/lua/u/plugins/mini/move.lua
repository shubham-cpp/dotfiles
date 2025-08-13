local later = MiniDeps.later
later(function()
  require("mini.move").setup({
    -- Move current line in Visual mode
    mappings = {
      left = "<",
      right = ">",
      down = "J",
      up = "K",

      -- Move current line in Normal mode
      line_left = "<M-h>",
      line_right = "<M-l>",
      line_down = "<M-j>",
      line_up = "<M-k>",
    },
  })
end)
