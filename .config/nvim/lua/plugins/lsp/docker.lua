---@type LazySpec
return {
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = { ensure_installed = { "dockerfile" } },
  },
  {
    "mason-org/mason.nvim",
    opts = { ensure_installed = { "hadolint"} },
  },
  {
    "mason-org/mason-lspconfig.nvim",
    opts = { ensure_installed = { "docker_compose_language_service", "dockerls" } },
  },
  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      opts.servers.docker_compose_language_service = {}
      opts.servers.dockerls = {}
    end,
  },
}
