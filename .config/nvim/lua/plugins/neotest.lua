return {
  "nvim-neotest/neotest",
  dependencies = { "nvim-neotest/neotest-jest" },
  keys = {
    {
      "<leader>tt",
      function()
        require("neotest").run.run()
      end,
      desc = "Run nearest test",
    },
    {
      "<leader>tf",
      function()
        require("neotest").run.run(vim.fn.expand("%"))
      end,
      desc = "Run file tests",
    },
    {
      "<leader>to",
      function()
        require("neotest").output.open({ enter = true })
      end,
      desc = "Test output",
    },
    {
      "<leader>ts",
      function()
        require("neotest").summary.toggle()
      end,
      desc = "Test summary",
    },
  },
  -- opts = function()
  --   return {
  --     adapters = {
  --       require("neotest-jest")({}),
  --       require("rustaceanvim.neotest")()
  --     },
  --   }
  -- end,
  config = function()
    require("neotest").setup({
      diagnostic = {
        enabled = true,
        severity = vim.diagnostic.severity.ERROR, -- Only show errors
      },
      config = function()
        local neotest_ns = vim.api.nvim_create_namespace("neotest")
        vim.diagnostic.config({
          virtual_text = {
            format = function(diagnostic)
              return diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " ")
            end,
          },
        }, neotest_ns)
      end,
      adapters = {
        require("neotest-jest")({}),
        -- require("neotest-plenary"),
        require("rustaceanvim.neotest"),
      },
    })
  end,
}
