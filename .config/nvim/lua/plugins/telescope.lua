---@type LazySpec
return {
  { "nvim-telescope/telescope-fzf-native.nvim", enabled = false },
  {
    "natecraddock/telescope-zf-native.nvim",
    lazy = true,
    enabled = function() 
	    local ok, util = pcall(require,"astrocore")
	    if not ok then
		    return false
	    end
	    return util.is_available "telescope.nvim" 
    end,
    specs = {
      {
        "nvim-telescope/telescope.nvim",
        dependencies = { "natecraddock/telescope-zf-native.nvim" },
        opts = function() require("telescope").load_extension "zf-native" end,
      },
    },
  },
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      { "nvim-mini/mini.fuzzy", opts = {}, lazy = true },
    },
    opts = function(_, opts)
      if not opts.defaults then opts.defaults = {} end
      opts.defaults.path_display = { "truncate", "filename_first" }
      opts.defaults.generic_sorter = require("mini.fuzzy").get_telescope_sorter

      if not opts.extensions then opts.extensions = {} end
      opts.extensions["zf-native"] = {
        generic = { enable = false },
      }
    end,
  },
}
