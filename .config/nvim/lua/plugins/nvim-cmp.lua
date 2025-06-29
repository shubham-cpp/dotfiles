---@type LazySpec
return {
  "hrsh7th/nvim-cmp",
  dependencies = { "lukas-reineke/cmp-rg" },
  optional = true,
  opts = function(_, opts)
    local cmp = require "cmp"
    opts.mapping["<C-x><C-x>"] = cmp.mapping.complete {
      config = { sources = { { name = "luasnip" } } },
    }
    opts.mapping["<C-y>"] = cmp.mapping {
      i = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
      c = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = false },
    }
    opts.mapping["<CR>"] = cmp.mapping {
      i = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
      c = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = false },
    }
    table.insert(opts.sources, {
      name = "rg",
      keyword_length = 3,
      max_item_count = 10,
      group_index = 2,
      priority = 200,
      option = { additional_arguments = "--smart-case" },
    })
  end,
}
