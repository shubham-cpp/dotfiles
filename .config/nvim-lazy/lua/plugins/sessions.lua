---@type LazySpec
return {
  {
    "folke/persistence.nvim",
    init = function()
      local group = vim.api.nvim_create_augroup("ConfigScopePersistence", { clear = true })

      vim.api.nvim_create_autocmd("User", {
        group = group,
        pattern = "PersistenceSavePre",
        callback = function()
          require("config.tabscope").save_state()
        end,
      })

      vim.api.nvim_create_autocmd("User", {
        group = group,
        pattern = "PersistenceLoadPre",
        callback = function()
          require("config.tabscope").suspend()
        end,
      })

      vim.api.nvim_create_autocmd("User", {
        group = group,
        pattern = "PersistenceLoadPost",
        callback = function()
          require("config.tabscope").resume()
        end,
      })
    end,
    keys = {
      {
        "<leader>ql",
        function()
          require("persistence").load()
        end,
        desc = "Load Session",
      },
      {
        "<leader>qs",
        function()
          require("persistence").select()
        end,
        desc = "Select Session",
      },
      {
        "<leader>qL",
        function()
          require("persistence").load({ last = true })
        end,
        desc = "Restore Last Session",
      },
      {
        "<leader>qd",
        function()
          require("persistence").stop()
        end,
        desc = "Don't Save Current Session",
      },
    },
  },
}
