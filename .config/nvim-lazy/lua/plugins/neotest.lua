return {
  "nvim-neotest/neotest",
  optional = true,
  dependencies = { { "nvim-neotest/neotest-jest", config = function() end } },
  opts = {
    adapters = {
      ["neotest-jest"] = {},
    },
  },
}
