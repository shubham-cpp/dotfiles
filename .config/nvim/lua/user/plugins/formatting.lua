local prettier = { 'prettierd', 'prettier', stop_after_first = true }
local function prettier_eslint(bufnr)
  return { require('user.config.util').first_formatter(bufnr, 'prettierd', 'prettier'), 'eslint_d' }
end
---@type LazySpec
return {
  'stevearc/conform.nvim',
  dependencies = { 'mason.nvim' },
  cmd = 'ConformInfo',
  keys = {
    {
      '<leader>=',
      function()
        require('conform').format({ async = true })
      end,
      mode = { 'n', 'v' },
      desc = 'Format Injected Langs',
    },
  },
  opts_extend = { 'formatters_by_ft' },
  -- This will provide type hinting with LuaLS
  ---@module "conform"
  ---@type conform.setupOpts
  opts = {
    default_format_opts = {
      timeout_ms = 3000,
      async = false, -- not recommended to change
      quiet = false, -- not recommended to change
      lsp_format = 'fallback', -- not recommended to change
    },
    formatters_by_ft = {
      lua = { 'stylua' },
      fish = { 'fish_indent' },
      sh = { 'shfmt' },
      bash = { 'shfmt' },
      astro = prettier_eslint,
      javascript = prettier_eslint,
      javascriptreact = prettier_eslint,
      typescript = prettier_eslint,
      typescriptreact = prettier_eslint,
      svelte = prettier_eslint,
      vue = prettier_eslint,
      jsonc = prettier,
      json = prettier,
      json5 = prettier,
      css = prettier,
      html = prettier,
      less = prettier,
      markdown = prettier,
      sass = prettier,
      scss = prettier,
      sql = { 'sqlfluff' },
      go = function(bufnr)
        return { 'goimports', require('user.config.util').first_formatter(bufnr, 'gofumpt', 'gofmt') }
      end,
      python = function(bufnr)
        if require('conform').get_formatter_info('ruff_format', bufnr).available then
          return { 'ruff_format', 'ruff_fix', 'ruff_organize_imports' }
        else
          return { 'isort', 'black' }
        end
      end,
      -- ['_'] = { 'trim_whitespace', 'trim_newlines' },
    },
    -- formatters = {
    --   sqlfluff = { args = { 'format', '--dialect=ansi', '-' } },
    -- },
    -- Set up format-on-save
    -- format_on_save = { timeout_ms = 500, lsp_fallback = true },
    format_on_save = function(bufnr)
      -- Disable autoformat for files in a certain path
      local bufname = vim.api.nvim_buf_get_name(bufnr)
      if bufname:match '/node_modules/' then
        return
      end
      -- Disable with a global or buffer-local variable
      if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
        return
      end
      return { timeout_ms = 300, lsp_format = 'fallback' }
    end,
  },
  config = function(_, opts)
    -- If you want the formatexpr, here is the place to set it
    vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
    vim.env.ESLINT_D_PPID = vim.fn.getpid()
    require('conform').setup(opts)

    vim.api.nvim_create_user_command('FormatDisable', function(args)
      if args.bang then
        -- FormatDisable! will disable formatting just for this buffer
        vim.b.disable_autoformat = true
      else
        vim.g.disable_autoformat = true
      end
    end, { desc = 'Disable autoformat-on-save', bang = true })
    vim.api.nvim_create_user_command('FormatEnable', function()
      vim.b.disable_autoformat = false
      vim.g.disable_autoformat = false
    end, { desc = 'Re-enable autoformat-on-save' })
  end,
}
