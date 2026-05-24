return {
  "mfussenegger/nvim-lint",
  event = { "BufWritePost", "InsertLeave" },
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

    local function should_lint(buf)
      return vim.api.nvim_buf_is_valid(buf) and vim.bo[buf].buftype == "" and not vim.b[buf].bigfile
    end

    local lint_augroup = vim.api.nvim_create_augroup("nvim_lint", { clear = true })
    vim.api.nvim_create_autocmd("BufWritePost", {
      group = lint_augroup,
      callback = function(args)
        if not should_lint(args.buf) then
          return
        end
        lint.try_lint(nil, { ignore_errors = true })
      end,
    })
    vim.api.nvim_create_autocmd("InsertLeave", {
      group = lint_augroup,
      callback = function(args)
        if not should_lint(args.buf) then
          return
        end
        lint.try_lint(nil, { filter = "stdin", ignore_errors = true })
      end,
    })
  end,
}
