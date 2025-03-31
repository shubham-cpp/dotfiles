---@type LazySpec
return {
  {
    "b0o/SchemaStore.nvim",
    lazy = true,
    dependencies = {
      {
        "AstroNvim/astrolsp",
        optional = true,
        ---@type AstroLSPOpts
        opts = {
          ---@diagnostic disable: missing-fields
          config = {
            yamlls = {
              on_new_config = function(config)
                config.settings.yaml.schemas = vim.tbl_deep_extend(
                  "force",
                  config.settings.yaml.schemas or {},
                  require("schemastore").yaml.schemas()
                )
              end,
              settings = { yaml = { schemaStore = { enable = false, url = "" } } },
            },
          },
        },
      },
    },
  },
  {
    "nvim-treesitter",
    optional = true,
    opts = { ensure_installed = { "yaml", "dockerfile" } },
  },
  {
    "mason.nvim",
    optional = true,
    opts = { ensure_installed = { "yaml-language-server" } },
  },
}
