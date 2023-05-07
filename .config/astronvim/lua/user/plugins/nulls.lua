return {
  'jose-elias-alvarez/null-ls.nvim',
  opts = function(_, opts)
    local null_ls = require 'null-ls'
    local my_sources = {
      null_ls.builtins.diagnostics.mypy,
      null_ls.builtins.diagnostics.fish,
      null_ls.builtins.formatting.fish_indent,
      null_ls.builtins.completion.spell.with({
        filetypes = { 'markdown', 'html' },
      }),
    }
    -- table.insert(opts.sources, null_ls.builtins.diagnostics.mypy)
    print(vim.inspect(opts))
    opts.sources = vim.tbl_extend('keep', opts.sources or {}, my_sources)
    return opts
  end,
}
