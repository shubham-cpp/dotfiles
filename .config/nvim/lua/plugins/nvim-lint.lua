local au_group = vim.api.nvim_create_augroup('sp_nvim_lint', {})
local languages = {
  sh = { 'shellcheck', 'cspell' },
  bash = { 'shellcheck', 'cspell' },
  fish = { 'fish' },
  zsh = { 'shellcheck', 'zsh', 'cspell' },
  -- c = { 'clangtidy' },
  -- cpp = { 'clangtidy' },
  go = { 'golangcilint', 'cspell' },
  -- markdown = { 'vale' },
  html = { 'htmlhint', 'cspell' },
  css = { 'stylelint', 'cspell' },
  scss = { 'stylelint', 'cspell' },
  sass = { 'stylelint', 'cspell' },
  less = { 'stylelint', 'cspell' },
  python = { 'ruff', 'cspell' },
  astro = { 'cspell' },
  svelte = { 'cspell' },
  javascript = { 'cspell' },
  javascriptreact = { 'cspell' },
  typescript = { 'cspell' },
  typescriptreact = { 'cspell' },
  vue = { 'cspell' },
  yaml = { 'yamllint', 'cspell' },
  php = { 'php', 'phpstan', 'phpcs', 'cspell' },
}
local linters = {
  eslint_d = {
    condition = function(ctx)
      return vim.fs.find({
        'eslint.config.js',
        'eslint.config.mjs',
        'eslint.config.cjs',
        '.eslintrc.json',
        '.eslintrc',
        '.eslintrc.js',
        '.eslintrc.yaml',
        '.eslintrc.yml',
      }, { path = ctx.filename, upward = true })[1]
    end,
  },
}

local function debounce(ms, fn)
  local timer = vim.uv.new_timer()
  return function(...)
    local argv = { ... }
    timer:start(ms, 0, function()
      timer:stop()
      vim.schedule_wrap(fn)(unpack(argv))
    end)
  end
end

---@type LazySpec
return {
  'mfussenegger/nvim-lint',
  enabled = true,
  ft = vim.tbl_keys(languages),
  config = function(_, opts)
    local lint = require 'lint'
    vim.env.ESLINT_D_PPID = vim.fn.getpid()
    languages = vim.tbl_deep_extend('force', languages, opts.languages or {})
    linters = vim.tbl_deep_extend('force', linters, opts.linters or {})

    lint.linters_by_ft = languages

    local function run()
      -- Use nvim-lint's logic first:
      -- * checks if linters exist for the full filetype first
      -- * otherwise will split filetype by "." and add all those linters
      -- * this differs from conform.nvim which only uses the first filetype that has a formatter
      local names = lint._resolve_linter_by_ft(vim.bo.filetype)

      -- Create a copy of the names table to avoid modifying the original.
      names = vim.list_extend({}, names)

      -- Add fallback linters.
      if #names == 0 then
        vim.list_extend(names, lint.linters_by_ft['_'] or {})
      end

      -- Add global linters.
      vim.list_extend(names, lint.linters_by_ft['*'] or {})

      -- Filter out linters that don't exist or don't match the condition.
      local ctx = { filename = vim.api.nvim_buf_get_name(0) }
      ctx.dirname = vim.fn.fnamemodify(ctx.filename, ':h')
      names = vim.tbl_filter(function(name)
        local linter = lint.linters[name]
        return linter and not (type(linter) == 'table' and linter.condition and not linter.condition(ctx))
      end, names)

      -- Run linters.
      if #names > 0 then
        lint.try_lint(names)
      end
    end
    for name, linter in pairs(linters) do
      if type(linter) == 'table' and type(lint.linters[name]) == 'table' then
        lint.linters[name] = vim.tbl_deep_extend('force', lint.linters[name], linter)
      else
        lint.linters[name] = linter
      end
    end
    vim.api.nvim_create_autocmd({ 'BufWritePost', 'BufRead', 'InsertLeave', 'FocusGained' }, {
      group = au_group,
      desc = 'Lint on save',
      pattern = '*',
      callback = debounce(100, run),
    })
  end,
}
