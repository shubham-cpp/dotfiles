---@type LazySpec
return {
  "nvimtools/none-ls.nvim",
  opts = function(_, opts)
    local null_ls = require "null-ls"
    local methods = require "null-ls.methods"
    local h = require "null-ls.helpers"

    local FORMATTING = methods.internal.FORMATTING
    local ruff_organize_imports = h.make_builtin {
      name = "ruff_organize_imports",
      meta = {
        url = "https://docs.astral.sh/ruff/",
        description = "An extremely fast Python linter, written in Rust. Organize imports.",
      },
      method = FORMATTING,
      filetypes = { "python" },
      generator_opts = {
        command = "ruff",
        args = {
          "check",
          "--fix",
          "--force-exclude",
          "--select=I001",
          "--exit-zero",
          "--no-cache",
          "--stdin-filename",
          "$FILENAME",
          "-",
        },
        to_stdin = true,
      },
      factory = h.formatter_factory,
    }
    local ruff_format = h.make_builtin {
      name = "ruff_format",
      meta = {
        url = "https://docs.astral.sh/ruff/",
        description = "An extremely fast Python linter, written in Rust. Organize imports.",
      },
      method = FORMATTING,
      filetypes = { "python" },
      generator_opts = {
        command = "ruff",
        args = { "format", "-n", "--force-exclude", "--stdin-filename", "$FILENAME", "-" },
        to_stdin = true,
      },
      factory = h.formatter_factory,
    }
    -- Check supported formatters and linters
    -- https://github.com/nvimtools/none-ls.nvim/tree/main/lua/null-ls/builtins/formatting
    -- https://github.com/nvimtools/none-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
    opts.sources = require("astrocore").list_insert_unique(opts.sources, {
      -- Set a formatter
      -- null_ls.builtins.formatting.stylua,
      -- null_ls.builtins.formatting.prettier,
      ruff_organize_imports,
      ruff_format,
    })
  end,
}
