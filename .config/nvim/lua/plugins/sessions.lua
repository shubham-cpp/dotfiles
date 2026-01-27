local get_session_name = function()
  local name = vim.fn.getcwd()
  -- local branch = vim.system({"git","branch","--show-current"},{text=true}):wait()
  -- if branch.code == 0 then
  local branch = vim.g.gitsigns_head --[[@as string|nil]]
  if branch ~= nil then
    -- return name .. vim.trim(branch.stdout --[[@as string]])
    return name .. vim.trim(branch)
  else
    return name
  end
end

local icon_name = "Persistence"
---@type LazySpec
return {
  -- { "stevearc/resession.nvim", enabled = false },
  {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      sessions = {
        autosave = {
          cwd = false,
        },
        ignore = {
          dirs = {},
          filetypes = { "gitcommit", "gitrebase" },
          buftypes = {},
        },
      },
      mappings = {
        n = {
          ["<Leader>q"] = { desc = "󰁯 Session" },
          -- update save dirsession mapping to get the correct session name
          ["<Leader>qs"] = {
            function() require("resession").save(get_session_name(), { dir = "dirsession" }) end,
            desc = "Save this dirsession",
          },
          -- update load dirsession mapping to get the correct session name
          ["<Leader>ql"] = {
            function() require("resession").load(get_session_name(), { dir = "dirsession" }) end,
            desc = "Load current dirsession",
          },
          -- Delete session
          ["<Leader>qd"] = {
            function() require("resession").delete(get_session_name(), { dir = "dirsession" }) end,
            desc = "Delete current dirsession",
          },
        },
      },
      autocmds = {
        git_branch_sessions = {
          -- auto save directory sessions on leaving
          {
            event = "VimLeavePre",
            desc = "Save git branch directory sessions on close",
            callback = vim.schedule_wrap(function()
              if require("astrocore.buffer").is_valid_session() then
                require("resession").save(get_session_name(), { dir = "dirsession", notify = false })
              end
            end),
          },
          -- auto restore previous previous directory session, remove if necessary
          {
            event = "VimEnter",
            desc = "Restore previous directory session if neovim opened with no arguments",
            nested = true, -- trigger other autocommands as buffers open
            callback = function()
              -- Only load the session if nvim was started with no args
              if vim.fn.argc(-1) == 0 then
                -- try to load a directory session using the current working directory
                require("resession").load(get_session_name(), { dir = "dirsession", silence_errors = true })
              end
            end,
          },
        },
      },
    },
  },
  {
    "folke/persistence.nvim",
    enabled = false,
    event = "BufReadPre", -- this will only start session saving when an actual file was opened
    opts = {},
    specs = {
      { "AstroNvim/astroui", opts = { icons = { [icon_name] = "󰁯" } } },
      {
        "AstroNvim/astrocore",
        opts = function(_, opts)
          local maps = opts.mappings
          local prefix = "<Leader>q"
          maps.n[prefix] = false
          maps.n[prefix] = { desc = require("astroui").get_icon(icon_name, 1, true) .. icon_name }
          maps.n[prefix .. "l"] = { function() require("persistence").load { last = true } end, desc = "Load Last" }
          maps.n[prefix .. "s"] = { function() require("persistence").select() end, desc = "Select" }
          maps.n[prefix .. "d"] = { function() require("persistence").stop() end, desc = "Stop" }
        end,
      },
    },
  },
}
