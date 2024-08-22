local au_group = vim.api.nvim_create_augroup("sp_sessions", { clear = true })
---@type LazySpec
return {
  {
    "Shatur/neovim-session-manager",
    event = "BufWinEnter",
    enabled = false,
    cmd = "SessionManager",
    keys = {
      { "<leader>Sl", "<cmd>SessionManager! load_session<cr>", desc = "[S]ession [L]oad" },
      { "<leader>SL", "<cmd>SessionManager! load_last_session<cr>", desc = "[S]ession [L]oad Last" },
      {
        "<leader>Sc",
        "<cmd>SessionManager! load_current_dir_session<cr>",
        desc = "[S]ession Load Current Directory",
      },
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    config = function()
      local session_manager = require("session_manager")
      session_manager.setup({
        -- Possible values: Disabled, CurrentDir, LastSession
        autoload_mode = require("session_manager.config").AutoloadMode.CurrentDir,
      })
    end,
  },
  {
    "stevearc/resession.nvim",
    enabled = true,
    lazy = true,
    init = function()
      local function get_session_name()
        local name = vim.fn.getcwd()
        local branch = vim.trim(vim.fn.system("git branch --show-current"))
        if vim.v.shell_error == 0 then
          return name .. branch
        else
          return name
        end
      end
      -- vim.api.nvim_create_autocmd("UIEnter", {
      --   desc = "Load session on VimEnter",
      --   group = au_group,
      --   callback = function()
      --     -- Only load the session if nvim was started with no args
      --     if vim.fn.argc(-1) == 0 then
      --       vim.schedule(function()
      --         require("resession").load(get_session_name(), { dir = "dirsession", silence_errors = true })
      --       end)
      --     end
      --   end,
      --   nested = true,
      -- })
      vim.api.nvim_create_autocmd("VimLeavePre", {
        desc = "Save session on VimLeavePre",
        group = au_group,
        callback = function()
          print("Saving session")
          require("resession").save(get_session_name(), { dir = "dirsession", notify = false })
        end,
      })
    end,
    opts = {},
    config = function(_, opts)
      require("resession").setup(opts)
    end,
    keys = {
      { "<leader>Ss", '<cmd>lua require("resession").save()<cr>', desc = "[S]ession [S]ave" },
      { "<leader>Sd", '<cmd>lua require("resession").delete()<cr>', desc = "[S]ession [D]elete" },
      {
        "<leader>Sl",
        function()
          vim.ui.select(
            require("resession").list({ dir = "dirsession" }),
            { prompt = "Select Session> " },
            function(selected_session)
              if selected_session == nil or not selected_session then
                return
              end
              require("resession").load(selected_session, { dir = "dirsession", silence_errors = true })
            end
          )
        end,
        desc = "[S]ession [L]ist",
      },
    },
  },
}
