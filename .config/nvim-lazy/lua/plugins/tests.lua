local group = vim.api.nvim_create_augroup("sp_tests", { clear = true })

vim.api.nvim_create_autocmd("BufReadPost", {
  pattern = {
    "*.test.js",
    "*.spec.js",
    "*.test.ts",
    "*.spec.ts",
    "*.test.jsx",
    "*.spec.jsx",
    "*.test.tsx",
    "*.spec.tsx",
  },
  description = "Neotest binds",
  callback = function(args)
    local bufnr = args.buf
    vim.keymap.set("n", "<leader>tw", function()
      require("neotest").run.run({ vim.fn.expand("%"), jestCommand = "jest --watch " })
    end, {
      buffer = bufnr,
      description = "Run test in watch mode(current file)",
    })
    vim.keymap.set("n", "<leader>tW", function()
      require("neotest").run.run({ jestCommand = "jest --watch " })
    end, {
      buffer = bufnr,
      description = "Run test in watch mode(project)",
    })
  end,
})

---@type LazySpec
return {
  "nvim-neotest/neotest",
  dependencies = {
    "marilari88/neotest-vitest",
    "nvim-neotest/neotest-jest",
  },
  opts = {
    adapters = {
      ["neotest-jest"] = {},
      ["neotest-vitest"] = {
        -- Filter directories when searching for test files. Useful in large projects (see Filter directories notes).
        filter_dir = function(name, rel_path, root)
          return name ~= "node_modules"
        end,
      },
    },
  },
}
