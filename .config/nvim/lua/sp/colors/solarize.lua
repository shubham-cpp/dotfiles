local ok_status, NeoSolarized = pcall(require, 'NeoSolarized')
if not ok_status then
  return
end

NeoSolarized.setup({})
vim.cmd 'colorscheme NeoSolarized'
vim.cmd 'hi QuickScopePrimary gui=bold,underline,undercurl guifg=#112c35 guibg=#cb4b16'
vim.cmd 'hi QuickScopeSecondary gui=bold,underline,undercurl guifg=#072d2b guibg=#859000'
