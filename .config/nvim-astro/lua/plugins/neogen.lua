---@type LazySpec
return {
  "danymat/neogen",
  cmd = "Neogen",
  opts = {
    snippet_engine = "luasnip",
    languages = {
      lua = { template = { annotation_convention = "emmylua" } },
      typescript = { template = { annotation_convention = "tsdoc" } },
      typescriptreact = { template = { annotation_convention = "tsdoc" } },
    },
  },
  dependencies = {
    { "AstroNvim/astroui", opts = { icons = { Neogen = "󰷉" } } },
    {
      "AstroNvim/astrocore",
      opts = function(_, opts)
        local maps = opts.mappings
        local prefix = "<Leader>n"
        maps.n[prefix] = { desc = require("astroui").get_icon("Neogen", 1, true) .. "Annotation" }
        maps.n[prefix .. "<CR>"] = { function() require("neogen").generate { type = "any" } end, desc = "Current" }
        maps.n[prefix .. "c"] = { function() require("neogen").generate { type = "class" } end, desc = "Class" }
        maps.n[prefix .. "f"] = { function() require("neogen").generate { type = "func" } end, desc = "Function" }
        maps.n[prefix .. "t"] = { function() require("neogen").generate { type = "type" } end, desc = "Type" }
        maps.n[prefix .. "F"] = { function() require("neogen").generate { type = "file" } end, desc = "File" }
      end,
    },
  },
}
