---@type LazySpec
return {
  "nvim-treesitter/nvim-treesitter-context",
  optional = true,
  opts = {
    max_lines = 3,
  },
  dependencies = {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      mappings = {
        n = {
          ["<localleader>\\"] = {
            function() require("treesitter-context").go_to_context(vim.v.count1) end,
            desc = "Jumping to context (upwards)",
            silent = true,
          },
          --- Swap the default bindings for ut & uT
          ["<Leader>uT"] = { function() require("astrocore.toggles").tabline() end, desc = "Toggle tabline" },
          ["<Leader>ut"] = { "<cmd>TSContext toggle<CR>", desc = "Toggle treesitter context" },
        },
      },
    },
  },
}
