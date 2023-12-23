return {
  capabilities = {
    offsetEncoding = 'utf-8',
  },
  cmd = {
    'clangd',
    '--background-index',
    '--clang-tidy',
    '--suggest-missing-includes',
    '--header-insertion=iwyu',
    '--completion-style=detailed',
    '--function-arg-placeholders',
    '--pch-storage=memory',
    '--fallback-style=llvm',
  },
}
