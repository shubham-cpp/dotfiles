local prefix = "<Leader>n"
---@type LazySpec
return {
  "danymat/neogen",
  optional = true,
  opts = {
    languages = {
      lua = { template = { annotation_convention = "emmylua" } },
      typescript = { template = { annotation_convention = "tsdoc" } },
      typescriptreact = { template = { annotation_convention = "tsdoc" } },
    },
  },
  keys = {
    { prefix, desc = "Annotation" },
    {
      prefix .. "<CR>",
      function()
        require("neogen").generate({ type = "any" })
      end,
      desc = "Current",
    },
    {
      prefix .. "c",
      function()
        require("neogen").generate({ type = "class" })
      end,
      desc = "Class",
    },
    {
      prefix .. "f",
      function()
        require("neogen").generate({ type = "func" })
      end,
      desc = "Function",
    },
    {
      prefix .. "t",
      function()
        require("neogen").generate({ type = "type" })
      end,
      desc = "Type",
    },
    {
      prefix .. "F",
      function()
        require("neogen").generate({ type = "file" })
      end,
      desc = "File",
    },
  },
}
