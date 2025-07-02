---@type LazySpec
return {
  "ibhagwan/fzf-lua",
  dependencies = "echasnovski/mini.icons",
  cmd = "FzfLua",
  opts = function()
    local actions = require "fzf-lua.actions"
    local vscode = {
      height = 0.55,
      width = 0.6,
      row = 0,
    }

    local action_keys = {
      ["ctrl-q"] = {
        fn = actions.file_edit_or_qf,
        prefix = "select-all+",
      },
    }

    return {
      { "border-fused", "hide" },
      defaults = { formatter = { "path.filename_first", 2 } },
      winopts = { preview = { default = "bat" } },
      files = {
        actions = action_keys,
        previewer = false,
        winopts = vscode,
      },
      git = {
        files = {
          actions = action_keys,
          previewer = false,
          winopts = vscode,
        },
      },
      grep = {
        actions = action_keys,
        rg_glob = true,
        glob_flag = "--iglob",
        glob_separator = "%s%-%-",
      },
    }
  end,
}
