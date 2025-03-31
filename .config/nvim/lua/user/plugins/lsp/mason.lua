---@type LazySpec
return {
  "williamboman/mason.nvim",
  cmd = "Mason",
  build = ":MasonUpdate",
  opts_extend = { "ensure_installed" },
  opts = {
    ---@type '"prepend"' | '"append"' | '"skip"'
    PATH = "append",
    ensure_installed = { "stylua", "docker-compose-language-service", "dockerfile-language-server", "hadolint" },
    ui = {
      icons = { package_pending = " ", package_installed = " ", package_uninstalled = " " },
    },
  },
  ---@param opts MasonSettings | {ensure_installed: string[]}
  config = function(_, opts)
    vim.diagnostic.config({
      severity_sort = true,
      float = { border = "rounded", source = "if_many" },
      underline = { severity = vim.diagnostic.severity.ERROR },
      signs = {
        text = {
          [vim.diagnostic.severity.ERROR] = "󰅚 ",
          [vim.diagnostic.severity.WARN] = "󰀪 ",
          [vim.diagnostic.severity.INFO] = "󰋽 ",
          [vim.diagnostic.severity.HINT] = "󰌶 ",
        },
      },
      virtual_text = {
        source = "if_many",
        spacing = 2,
        format = function(diagnostic)
          local diagnostic_message = {
            [vim.diagnostic.severity.ERROR] = diagnostic.message,
            [vim.diagnostic.severity.WARN] = diagnostic.message,
            [vim.diagnostic.severity.INFO] = diagnostic.message,
            [vim.diagnostic.severity.HINT] = diagnostic.message,
          }
          return diagnostic_message[diagnostic.severity]
        end,
      },
    })

    require("mason").setup(opts)

    local mr = require "mason-registry"

    mr.refresh(function()
      for _, tool in ipairs(opts.ensure_installed) do
        local p = mr.get_package(tool)
        if not p:is_installed() then p:install() end
      end
    end)
  end,
}
