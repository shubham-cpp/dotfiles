-- function for calculating the current session name
local get_session_name = function()
  local name = vim.fn.getcwd()
  -- local branch = vim.fn.system "git branch --show-current"
  local branch = vim.b.gitsigns_head
  if branch == nil then branch = vim.fn.system "git branch --show-current" end
  if branch ~= nil or vim.v.shell_error == 0 then
    return name .. vim.trim(branch --[[@as string]])
  else
    return name
  end
end

---@type LazySpec
return {
  "AstroNvim/astrocore",
  ---@type AstroCoreOpts
  opts = {
    sessions = {
      -- disable the auto-saving of directory sessions
      autosave = { cwd = false },
    },
    mappings = {
      n = {
        -- update save dirsession mapping to get the correct session name
        ["<Leader>SS"] = {
          function() require("resession").save(get_session_name(), { dir = "dirsession" }) end,
          desc = "Save this dirsession",
        },
        -- update load dirsession mapping to get the correct session name
        ["<Leader>S."] = {
          function() require("resession").load(get_session_name(), { dir = "dirsession" }) end,
          desc = "Load current dirsession",
        },
      },
    },
    autocmds = {
      -- disable alpha autostart. I've disabled `alpha-nvim`
      -- alpha_autostart = false,
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
            -- Logic copied from https://github.com/AstroNvim/AstroNvim/blob/365aa6e083dcd25fa3d1c8a2515d7e71a03d51d3/lua/astronvim/plugins/alpha.lua#L49
            local should_skip
            local lines = vim.api.nvim_buf_get_lines(0, 0, 2, false)
            if
              vim.fn.argc() > 0 -- don't start when opening a file
              or #lines > 1 -- don't open if current buffer has more than 1 line
              or (#lines == 1 and lines[1]:len() > 0) -- don't open the current buffer if it has anything on the first line
              or #vim.tbl_filter(function(bufnr) return vim.bo[bufnr].buflisted end, vim.api.nvim_list_bufs()) > 1 -- don't open if any listed buffers
              or not vim.o.modifiable -- don't open if not modifiable
            then
              should_skip = true
            else
              for _, arg in pairs(vim.v.argv) do
                if arg == "-b" or arg == "-c" or vim.startswith(arg, "+") or arg == "-S" then
                  should_skip = true
                  break
                end
              end
            end
            if should_skip then return end
            -- if possible, load session
            if not pcall(function() require("resession").load(get_session_name(), { dir = "dirsession" }) end) then
              -- if session was not loaded, if possible, load alpha
              require("lazy").load { plugins = { "alpha-nvim" } }
              if pcall(function() require("alpha").start(true) end) then
                vim.schedule(function() vim.cmd.doautocmd "FileType" end)
              end
            end
          end,
        },
      },
    },
  },
}
