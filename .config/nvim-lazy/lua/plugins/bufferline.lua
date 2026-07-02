---@type LazySpec
return {
  {
    "akinsho/bufferline.nvim",
    optional = true,
    dependencies = { "tiagovla/scope.nvim" },
    opts = function(_, opts)
      local close_buffer = function(buf)
        require("config.tabscope").close_buffer(buf)
      end

      opts.options = opts.options or {}
      opts.options.mode = "buffers"
      opts.options.always_show_bufferline = true
      opts.options.close_command = close_buffer
      opts.options.right_mouse_command = close_buffer
    end,
  },
  {
    "tiagovla/scope.nvim",
    lazy = false,
    config = function()
      require("config.tabscope").setup()
    end,
  },
}
