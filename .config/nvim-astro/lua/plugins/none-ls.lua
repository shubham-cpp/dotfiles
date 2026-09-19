---@type LazySpec
return {
  {
    "nvimtools/none-ls.nvim",
    opts = function(_, opts)
      local null_ls = require "null-ls"
      local helpers = require "null-ls.helpers"
      local astrocore = require "astrocore"

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
