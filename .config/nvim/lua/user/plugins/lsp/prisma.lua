---@type LazySpec
return {
  { 'williamboman/mason.nvim', opts = { ensure_installed = { 'prisma-language-server' } } },
  { 'nvim-treesitter', opts = { ensure_installed = { 'prisma' } } },
}
