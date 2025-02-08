---@type LazySpec
return {
  {
    "mason.nvim",
    opts = { ensure_installed = { "goimports", "gofumpt", "golangci-lint" } },
  },
  {
    "mfussenegger/nvim-lint",
    opts = {
      linters_by_ft = {
        go = { "golangcilint" },
      },
    },
  },
}
