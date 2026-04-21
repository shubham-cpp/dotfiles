return {
  "mfussenegger/nvim-lint",
  config = function()
    local lint = require("lint")
    lint.linters_by_ft = {
      go = { "golangcilint" },
      fish = { "fish" },
      python = { "ruff" },
      zsh = { "zsh" },
      systemd = { "systemd-analyze" },
      css = { "stylelint" },
      scss = { "stylelint" },
      less = { "stylelint" },
    }

    local lint_augroup = vim.api.nvim_create_augroup("nvim_lint", { clear = true })
    vim.api.nvim_create_autocmd({ "BufWritePost", "InsertLeave" }, {
      group = lint_augroup,
      callback = function(args)
        if vim.b[args.buf].bigfile then return end
        lint.try_lint()
      end,
    })
  end,
}
