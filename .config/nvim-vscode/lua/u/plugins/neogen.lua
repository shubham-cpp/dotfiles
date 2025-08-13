local add, later = MiniDeps.add, MiniDeps.later

local prefix = "<Leader>n"

later(function()
  add({ source = "danymat/neogen" })
  local neogen = require "neogen"
  neogen.setup({
    languages = {
      lua = { template = { annotation_convention = "emmylua" } },
      typescript = { template = { annotation_convention = "tsdoc" } },
      typescriptreact = { template = { annotation_convention = "tsdoc" } },
    },
  })

  -- stylua: ignore start
  vim.keymap.set("n", prefix .. "<CR>", function() neogen.generate({ type = "any" })   end, { desc = "Current" })
  vim.keymap.set("n", prefix .. "c",    function() neogen.generate({ type = "class" }) end, { desc = "Class" })
  vim.keymap.set("n", prefix .. "f",    function() neogen.generate({ type = "func" })  end, { desc = "Function" })
  vim.keymap.set("n", prefix .. "t",    function() neogen.generate({ type = "type" })  end, { desc = "Type" })
  vim.keymap.set("n", prefix .. "F",    function() neogen.generate({ type = "file" })  end, { desc = "File" })
  vim.keymap.set("n", "<leader>ln",     function() neogen.generate()                   end, { desc = "Generate Annotations (Neogen)" })
  -- stylua: ignore end
end)
