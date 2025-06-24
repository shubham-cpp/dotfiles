local group = vim.api.nvim_create_augroup("sp_formatters", { clear = true })
---@type LazySpec
return {
  {
    "mason-org/mason-lspconfig.nvim",
    opts = { ensure_installed = { "efm" } },
  },
  {
    "mason-org/mason.nvim",
    opts = { ensure_installed = { "eslint_d", "prettier" } },
  },
  {
    "creativenull/efmls-configs-nvim",
    -- dir="~/Documents/Programming/Contributions/efmls-configs-nvim",
    version = "v1.x.x", -- version is optional, but recommended
  },
  {
    "neovim/nvim-lspconfig",
    opts_extend = { "servers" },
    opts = function(_, opts)
      vim.env.ESLINT_D_PPID = vim.fn.getpid()

      local eslint_lint = require "efmls-configs.linters.eslint_d"
      local eslint_format = require "efmls-configs.formatters.eslint_d"
      local prettier = require "efmls-configs.formatters.prettier_d"
      local stylua = require "efmls-configs.formatters.stylua"
      local shellcheck = require "efmls-configs.linters.shellcheck"
      local shfmt = require "efmls-configs.formatters.shfmt"
      local fish = require "efmls-configs.linters.fish"
      local fish_indent = require "efmls-configs.formatters.fish_indent"
      local hadolint = require "efmls-configs.linters.hadolint"
      local golangci_lint = require "efmls-configs.linters.golangci_lint"
      local gofumpt = require "efmls-configs.formatters.gofumpt"
      local goimports = require "efmls-configs.formatters.goimports"
      local ruff = require "efmls-configs.linters.ruff"
      local ruff_format = require "efmls-configs.formatters.ruff"
      local ruff_sort = require "efmls-configs.formatters.ruff_sort"

      -- Add newer eslint config files
      table.insert(eslint_lint.rootMarkers, "eslint.config.mjs")
      table.insert(eslint_lint.rootMarkers, "eslint.config.js")
      table.insert(eslint_lint.rootMarkers, "eslint.config.js")
      if eslint_format.rootMarkers == nil then
        eslint_format.rootMarkers = eslint_lint.rootMarkers
        eslint_format.requireMarker = true
      end

      local languages = {
        html = { prettier },
        css = { prettier },
        scss = { prettier },
        sass = { prettier },
        less = { prettier },
        json = { prettier },
        jsonc = { prettier },
        yaml = { prettier },
        ["yaml.docker-compose"] = { prettier },
        markdown = { prettier },
        vue = { prettier, eslint_format, eslint_lint },
        astro = { prettier, eslint_lint, eslint_format },
        svelte = { prettier, eslint_lint, eslint_format },
        javascript = { prettier, eslint_lint, eslint_format },
        typescript = { prettier, eslint_lint, eslint_format },
        javascriptreact = { prettier, eslint_lint, eslint_format },
        typescriptreact = { prettier, eslint_lint, eslint_format },
        lua = { stylua },
        bash = { shellcheck, shfmt },
        sh = { shellcheck, shfmt },
        zsh = { shellcheck, shfmt },
        fish = { fish, fish_indent },
        dockerfile = { prettier, hadolint },
        go = { golangci_lint, gofumpt, goimports },
        python = { ruff, ruff_format, ruff_sort },
      }

      opts.servers.efm = {
        filetypes = vim.tbl_keys(languages),
        settings = {
          -- languages = languages
          rootMarkers = {
            ".git/",
            "package.json",
            "lazy-lock.json",
            "go.mod",
            "pyproject.toml",
            "cargo.toml",
            "requirements.txt",
            ".venv/",
          },
        },
        init_options = {
          hover = true,
          codeAction = true,
          documentFormatting = true,
          documentRangeFormatting = true,
        },
        on_attach = function(_, buffer, map)
          vim.api.nvim_create_autocmd("BufWritePre", {
            -- group = group,
            desc = "Format on Save",
            buffer = buffer,
            command = 'lua vim.lsp.buf.format({ name = "efm", timeout = 2000, async = false })',
          })
          map("<leader>=", function()
            vim.lsp.buf.format { name = "efm", async = true }
          end, "Format", "formatting")

          map("<leader>=", function()
            vim.lsp.buf.format { name = "efm", async = true }
          end, "Format", "rangeFormatting", { "x" })
        end,
      }
    end,
  },
}
