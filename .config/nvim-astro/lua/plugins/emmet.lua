---@type LazySpec
return {
  {
    "mattn/emmet-vim",
    cmd = "EmmetInstall",
    keys = { { "<c-y>", desc = "Emmet plugin", mode = { "i", "n" } } },
    init = function() vim.g.user_emmet_install_global = 0 end,
  },
  {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      autocmds = {
        enable_emmet_in_fts = {
          {
            event = "FileType",
            desc = "Enabled emmet plugin for specific filetypes",
            pattern = { "html", "css", "scss", "javascriptreact", "typescriptreact", "astro", "vue", "svelte" },
            command = "EmmetInstall",
          },
        },
      },
    },
  },
}
