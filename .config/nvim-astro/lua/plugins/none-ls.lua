---@type LazySpec
return {
  {
    "nvimtools/none-ls.nvim",
    opts = function(_, opts)
      local null_ls = require "null-ls"
      local helpers = require "null-ls.helpers"
      local astrocore = require "astrocore"

      local lint_filetypes = {
        "astro",
        "javascript",
        "javascriptreact",
        "svelte",
        "typescript",
        "typescriptreact",
        "vue",
      }

      local format_filetypes = {
        "astro",
        "css",
        "graphql",
        "handlebars",
        "html",
        "htmlangular",
        "javascript",
        "javascriptreact",
        "json",
        "jsonc",
        "less",
        "markdown",
        "markdown.mdx",
        "scss",
        "svelte",
        "typescript",
        "typescriptreact",
        "vue",
        "yaml",
      }

      local severity = {
        error = vim.diagnostic.severity.ERROR,
        warning = vim.diagnostic.severity.WARN,
        information = vim.diagnostic.severity.INFO,
        hint = vim.diagnostic.severity.HINT,
      }

      local oxlint = {
        name = "oxlint",
        method = null_ls.methods.DIAGNOSTICS,
        filetypes = lint_filetypes,
        generator = null_ls.generator {
          command = "oxlint",
          args = { "--format", "json", "$FILENAME" },
          format = "json",
          check_exit_code = function(code) return code <= 1 end,
          on_output = function(params)
            local diagnostics = {}
            for _, item in ipairs(params.output and params.output.diagnostics or {}) do
              local label = item.labels and item.labels[1]
              local span = label and label.span or {}
              local row = span.line or 1
              local col = math.max((span.column or 1) - 1, 0)

              diagnostics[#diagnostics + 1] = {
                row = row,
                col = col,
                end_col = col + math.max(span.length or 1, 1),
                source = "oxlint",
                code = item.code,
                severity = severity[item.severity] or vim.diagnostic.severity.WARN,
                message = item.message,
              }
            end
            return diagnostics
          end,
        },
      }

      local oxfmt = {
        name = "oxfmt",
        method = null_ls.methods.FORMATTING,
        filetypes = format_filetypes,
        generator = helpers.formatter_factory {
          command = "oxfmt",
          args = { "--stdin-filepath", "$FILENAME" },
          to_stdin = true,
        },
      }

      opts.sources = vim.tbl_filter(
        function(source) return type(source) ~= "table" or source.name ~= "prettierd" end,
        opts.sources or {}
      )
      opts.sources = astrocore.list_insert_unique(opts.sources, {
        oxlint,
        oxfmt,
      })
    end,
  },
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    optional = true,
    opts = function(_, opts)
      opts.ensure_installed = require("astrocore").list_insert_unique(opts.ensure_installed or {}, { "oxfmt" })
      opts.ensure_installed = vim.tbl_filter(function(tool) return tool ~= "prettierd" end, opts.ensure_installed)
    end,
  },
}
