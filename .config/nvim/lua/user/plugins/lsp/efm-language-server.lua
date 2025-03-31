local function format()
  if vim.b.disable_format ~= true or vim.g.disable_format ~= true then
    vim.lsp.buf.format({ name = "efm", timeout = 2500 })
  end
end
---@type LazySpec
return {
  {
    "creativenull/efmls-configs-nvim",
    version = "v1.x.x",
    lazy = true,
  },
  {
    "mason.nvim",
    optional = true,
    opts = {
      ensure_installed = {
        "efm",
        "clang-format",
        "shellcheck",
        "shfmt",
      },
    },
  },
  {
    "AstroNvim/astrolsp",
    optional = true,
    ---@type AstroLSPOpts
    opts = {
      config = {
        efm = {
          on_new_config = function(config)
            local stylua = require "efmls-configs.formatters.stylua"
            local prettier = require "efmls-configs.formatters.prettier_d"
            local fish_lint = require "efmls-configs.linters.fish"
            local fish_indent = require "efmls-configs.formatters.fish_indent"
            local gcc = require "efmls-configs.linters.gcc"
            local clangd_format = require "efmls-configs.formatters.clang_format"
            local golangci_lint = require "efmls-configs.linters.golangci_lint"
            local goimport = require "efmls-configs.formatters.goimports"
            local gofumpt = require "efmls-configs.formatters.gofumpt"
            local ruff_lint = require "efmls-configs.linters.ruff"
            local ruff_format = require "efmls-configs.formatters.ruff"
            local ruff_sort = require "efmls-configs.formatters.ruff_sort"
            local shfmt = require "efmls-configs.formatters.shfmt"
            local shellcheck = require "efmls-configs.linters.shellcheck"
            local hadolint = require "efmls-configs.linters.hadolint"

            local languages = {
              typescript = { prettier },
              javascript = { prettier },
              typescriptreact = { prettier },
              javascriptreact = { prettier },
              astro = { prettier },
              vue = { prettier },
              svelte = { prettier },

              markdown = { prettier },
              css = { prettier },
              scss = { prettier },
              sass = { prettier },
              less = { prettier },
              html = { prettier },
              json = { prettier },
              jsonc = { prettier },
              yaml = { prettier },

              lua = { stylua },

              c = { gcc, clangd_format },
              cpp = { gcc, clangd_format },

              go = { golangci_lint, goimport, gofumpt },
              python = { ruff_lint, ruff_format, ruff_sort },

              sh = { shfmt, shellcheck },
              zsh = { shfmt, shellcheck },
              bash = { shfmt, shellcheck },
              fish = { fish_lint, fish_indent },
              dockerfile = { hadolint },
            }
            config.filetypes = vim.tbl_keys(vim.tbl_extend("force", config.filetypes or {}, languages))
            if not config.settings then config.settings = {} end
            config.settings.rootMarkers = { ".git/", "package.json", ".venv", "requirements.txt", "go.mod" }
            config.settings.languages = vim.tbl_extend("force", config.settings.languages or {}, languages)

            config.init_options = {
              documentFormatting = true,
              documentRangeFormatting = true,
            }
          end,
        },
      },
      on_attach = function(client, bufnr)
        local ok_navic, navic = pcall(require, "nvim-navic")
        if ok_navic and client.supports_method "textDocument/documentSymbol" then navic.attach(client, bufnr) end
        if client.supports_method "textDocument/documentFormatting" then
          vim.keymap.set(
            "n",
            "<leader>=",
            function() vim.lsp.buf.format({ async = true, name = "efm" }) end,
            { desc = "Format Buffer" }
          )
          vim.api.nvim_create_user_command("Format", format, {
            desc = "Format buffer using efm",
          })
          vim.api.nvim_create_user_command("FormatDisable", function(args)
            if args.bang == true then
              vim.g.disable_format = true
            else
              vim.b.disable_format = true
            end
          end, {
            bang = true,
            desc = "Disable auto-formatting for current buffer. Use `FormatDisable!`(bang) to disable globally",
          })
          vim.api.nvim_create_autocmd("BufWritePre", {
            desc = "Autoformat buffer",
            buffer = bufnr,
            callback = format,
          })
        end
        if client.supports_method "textDocument/documentRangeFormatting" then
          vim.keymap.set(
            "v",
            "<leader>=",
            function() vim.lsp.buf.format({ async = true, name = "efm" }) end,
            { desc = "Format Range" }
          )
        end
      end,
    },
  },
}
