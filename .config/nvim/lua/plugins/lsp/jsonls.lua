---@type LazySpec
return {
  {
    "b0o/SchemaStore.nvim",
    lazy = true,
    version = false, -- last release is way too old
  },
  {
    "mason-org/mason-lspconfig.nvim",
    opts = { ensure_installed = { "jsonls", "yamlls" }, },
  },
  {
    'neovim/nvim-lspconfig',
    opts_extend = { 'servers' },
    opts = function(_,opts)
      opts.servers['jsonls'] = {
        on_new_config = function(new_config)
          new_config.settings.json.schemas = new_config.settings.json.schemas or {}
          vim.list_extend(new_config.settings.json.schemas, require("schemastore").json.schemas())
        end,
        settings = {
          json = {
            format = { enable = true },
            validate = { enable = true },
          },
        },
      }
      opts.servers['yamlls'] = {
        -- lazy-load schemastore when needed
        on_new_config = function(new_config)
          new_config.settings.yaml.schemas = vim.tbl_deep_extend(
            "force",
            new_config.settings.yaml.schemas or {},
            require("schemastore").yaml.schemas()
          )
        end,
        settings = {
          redhat = { telemetry = { enabled = false } },
          yaml = {
            keyOrdering = false,
            validate = true,
            format = { enable = true, },
            schemaStore = { enable = false, url = "", },
          },
        },
      }
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = { ensure_installed = { "json", "json5", "jsonc", "yaml" } },
  }
}
