---@type LazySpec
return {
  "danymat/neogen",
  opts = {
    languages = {
      lua = { template = { annotation_convention = "emmylua" } },
      typescript = { template = { annotation_convention = "tsdoc" } },
      typescriptreact = { template = { annotation_convention = "tsdoc" } },
    },
  },
  keys = function()
    local prefix = "<Leader>n"
    local neogen = require "neogen"
    -- stylua: ignore start
    return {
      { prefix .. "<CR>", function() neogen.generate({ type = "any" })   end, desc = "Current" },
      { prefix .. "c",    function() neogen.generate({ type = "class" }) end, desc = "Class" },
      { prefix .. "f",    function() neogen.generate({ type = "func" })  end, desc = "Function" },
      { prefix .. "t",    function() neogen.generate({ type = "type" })  end, desc = "Type" },
      { prefix .. "F",    function() neogen.generate({ type = "file" })  end, desc = "File" },
      { "<leader>ln",     function() neogen.generate()                   end, desc = "Generate Annotations (Neogen)" },
    }
    -- stylua: ignore end
  end,
}
