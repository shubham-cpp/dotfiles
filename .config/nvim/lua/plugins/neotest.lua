return {
  {
    url = "nvim-neotest/neotest",
    config = function()
      require("neotest").setup({
        adapters = {
          require("neotest-jest")({}),
        },
      })
    end,
  },

  {
    url = "nvim-neotest/neotest-jest",
  },
}
