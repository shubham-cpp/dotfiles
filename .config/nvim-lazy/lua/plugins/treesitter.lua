---@type LazySpec
return {
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    keys = function()
      local ks = {}
      local textobjects = {
        swap = {
          swap_next = {
            ["<LocalLeader>k"] = { query = "@block.outer", desc = "Swap next block" },
            ["<LocalLeader>f"] = { query = "@function.outer", desc = "Swap next function" },
            ["<LocalLeader>a"] = { query = "@parameter.inner", desc = "Swap next argument" },
          },
          swap_previous = {
            ["<LocalLeader>K"] = { query = "@block.outer", desc = "Swap previous block" },
            ["<LocalLeader>F"] = { query = "@function.outer", desc = "Swap previous function" },
            ["<LocalLeader>A"] = { query = "@parameter.inner", desc = "Swap previous argument" },
          },
        },
      }
      for fn, tbl in pairs(textobjects.swap) do
        for key, arg in pairs(tbl) do
          table.insert(ks, {
            key,
            function()
              require("nvim-treesitter-textobjects.swap")[fn](arg.query)
            end,
            desc = arg.desc,
          })
        end
      end
      return ks
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter-context",
    keys = {
      {
        "<localleader>c",
        function()
          require("treesitter-context").go_to_context(vim.v.count1)
        end,
        desc = "Jumping to context (upwards)",
      },
      {
        "<localleader><localleader>",
        function()
          require("treesitter-context").go_to_context(vim.v.count1)
        end,
        desc = "Jumping to context (upwards)",
      },
    },
  },
}
