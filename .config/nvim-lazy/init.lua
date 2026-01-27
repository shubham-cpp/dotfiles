-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")

vim.api.nvim_create_autocmd("BufWinEnter", {
  once = true,
  desc = "Run lspmux if its not running already",
  callback = function()
    local on_exit = function(obj)
      if obj.code == 1 then
        vim.system({ "lspmux", "server" }, {
          detach = true,
        }, function(out)
          if out.code == 0 then
            vim.notify("Started lspmux server", vim.log.levels.INFO)
          else
            vim.notify("Failed to start lspmux server", vim.log.levels.ERROR)
          end
        end)
      end
    end
    vim.system({ "pgrep", "-x", "lspmux" }, on_exit)
  end,
})
