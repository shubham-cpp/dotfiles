---@type LazySpec
return {
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = {
      ensure_installed = {
        "bash",
        "fish",
        "sxhkdrc",
        "hyprlang",
        "ssh_config",
        "gitcommit",
        "git_config",
        "gitattributes",
        "gitignore",
        "git_rebase",
      },
    },
  },
  {
    "mason-org/mason-lspconfig.nvim",
    opts = { ensure_installed = { "bashls" } },
  },
  {
    "mason-org/mason.nvim",
    opts = { ensure_installed = { "shellcheck", "shfmt" } },
  },
  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      opts.servers["bashls"] = {
        filetypes = { "sh", "bash", "zsh" },
      }
    end,
  },
}
