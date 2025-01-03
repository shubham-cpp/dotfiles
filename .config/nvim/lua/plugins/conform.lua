return {
  'stevearc/conform.nvim',
  event = { 'BufWritePre' },
  cmd = { 'ConformInfo' },
  keys = {
    {
      -- Customize or remove this keymap to your liking
      '<leader>=',
      function()
        require('conform').format({ async = true, lsp_fallback = true })
      end,
      mode = { 'n', 'v' },
      desc = 'Format buffer(c)',
    },
  },
  init = function()
    -- If you want the formatexpr, here is the place to set it
    vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
  end,
  config = function()
    vim.env.ESLINT_D_PPID = vim.fn.getpid()
    local prettier = { 'prettierd', 'prettier', stop_after_first = true }
    local slow_format_filetypes = {}

    ---@param bufnr integer
    ---@param ... string
    ---@return string
    local function first(bufnr, ...)
      local conform = require 'conform'
      for i = 1, select('#', ...) do
        local formatter = select(i, ...)
        if conform.get_formatter_info(formatter, bufnr).available then
          return formatter
        end
      end
      return select(1, ...)
    end

    local function prettier_eslint(bufnr)
      return { first(bufnr, 'prettierd', 'prettier'), 'eslint_d' }
    end

    require('conform').setup({
      -- Define your formatters
      formatters_by_ft = {
        sh = { 'shfmt' },
        lua = { 'stylua' },
        bash = { 'shfmt' },
        c = { 'clang-format' },
        cpp = { 'clang-format' },
        go = function(bufnr)
          return { 'goimports', first(bufnr, 'gofumpt', 'gofmt') }
        end,
        fish = { 'fish_indent' },
        css = prettier,
        html = prettier,
        jsonc = prettier,
        json = prettier,
        json5 = prettier,
        less = prettier,
        markdown = prettier,
        sass = prettier,
        scss = prettier,
        astro = prettier_eslint,
        javascript = prettier_eslint,
        javascriptreact = prettier_eslint,
        typescript = prettier_eslint,
        typescriptreact = prettier_eslint,
        svelte = prettier_eslint,
        vue = prettier_eslint,
        nim = { 'nimpretty' },
        zig = { 'zigfmt' },
        php = function(bufnr)
          return { first(bufnr, 'pint', 'php-cs-fixer', 'phpcbf') }
        end,
        blade = { 'blade-formatter' },
        gleam = { 'gleam' },
        -- You can use a function here to determine the formatters dynamically
        python = function(bufnr)
          if require('conform').get_formatter_info('ruff_format', bufnr).available then
            return { 'ruff_format', 'ruff_fix', 'ruff_organize_imports' }
          else
            return { 'isort', 'black' }
          end
        end,
        -- Use the "_" filetype to run formatters on filetypes that don't
        -- have other formatters configured.
        ['_'] = { 'trim_whitespace', 'trim_newlines' },
      },
      default_format_opts = {
        lsp_format = 'fallback',
      },
      -- Set up format-on-save
      -- format_on_save = { timeout_ms = 500, lsp_fallback = true },
      format_on_save = function(bufnr)
        -- Disable autoformat for files in a certain path
        local bufname = vim.api.nvim_buf_get_name(bufnr)
        if bufname:match '/node_modules/' then
          return
        end
        if slow_format_filetypes[vim.bo[bufnr].filetype] then
          return
        end
        local function on_format(err)
          if err and err:match 'timeout$' then
            slow_format_filetypes[vim.bo[bufnr].filetype] = true
          end
        end

        return { timeout_ms = 200, lsp_fallback = true }, on_format
      end,

      format_after_save = function(bufnr)
        if not slow_format_filetypes[vim.bo[bufnr].filetype] then
          return
        end
        return { lsp_fallback = true }
      end,
      -- Customize formatters
      formatters = {
        shfmt = {
          prepend_args = { '-i', '2' },
        },
      },
    })
  end,
}
