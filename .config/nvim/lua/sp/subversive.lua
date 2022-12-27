local function map(mode, lhs, rhs)
  vim.api.nvim_set_keymap(mode, lhs, rhs, {})
end

map('n', 'x', '<plug>(SubversiveSubstitute)')
map('n', 'xx', '<plug>(SubversiveSubstituteLine)')
map('n', 'X', '<plug>(SubversiveSubstituteToEndOfLine)')

-- map('n', '<Leader>xb', '<plug>(SubversiveSubstituteRange)')
-- map('x', '<Leader>xb', '<plug>(SubversiveSubstituteRange)')
-- map('n', '<Leader>xbw', '<plug>(SubversiveSubstituteWordRange)')

-- map('n', '<Leader>cx', '<plug>(SubversiveSubstituteRangeConfirm)')
-- map('x', '<Leader>cx', '<plug>(SubversiveSubstituteRangeConfirm)')
-- map('n', '<Leader>cxw', '<plug>(SubversiveSubstituteWordRangeConfirm)')
