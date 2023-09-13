return {
  'Shatur/neovim-session-manager',
  event = 'BufWinEnter',
  config = function()
    local sm = require 'session_manager'
    local conf = require 'session_manager.config'
    sm.setup({
      -- Define what to do when Neovim is started without arguments.
      -- Possible values: Disabled, CurrentDir, LastSession
      autoload_mode = conf.AutoloadMode.CurrentDir,
    })
  end,
}
