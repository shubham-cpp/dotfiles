return {
  {
    "nvim-lualine/lualine.nvim",
    config = function()
      local diag_icons = require("core.icons")
      local tab_bufs = require("core.tab_buffers")
      local Buffers = require("lualine.components.buffers")
      function Buffers:buffers()
        local bufnrs = tab_bufs.get_bufnrs()
        local result = {}
        Buffers.bufpos2nr = {}
        for _, b in ipairs(bufnrs) do
          result[#result + 1] = self:new_buffer(b, #result + 1)
          Buffers.bufpos2nr[#result] = b
        end
        return result
      end
      tab_bufs.setup()
      local colors = {
        bg = "#141415",
        fg = "#cdcdcd",
        inactive_bg = "#1c1c24",
        comment = "#606079",
        error = "#d8647e",
        warning = "#f3be7c",
        hint = "#7e98e8",
        func = "#c48282",
        string = "#e8b589",
        property = "#c3c3d5",
        constant = "#aeaed1",
        keyword = "#6e94b2",
        type = "#9bb4bc",
        delta = "#f3be7c",
        plus = "#90a959",
      }

      local conditions = {
        buffer_not_empty = function()
          return vim.fn.empty(vim.fn.expand("%:t")) ~= 1
        end,
        hide_in_width = function()
          return vim.fn.winwidth(0) > 80
        end,
      }

      local config = {
        sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {},
          lualine_x = {},
          lualine_y = {},
          lualine_z = {},
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = { "filename" },
          lualine_x = {},
          lualine_y = {},
          lualine_z = {},
        },
        tabline = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {
            {
              "buffers",
              mode = 0,
              show_filename_only = true,
              show_modified_status = true,
              buffers_color = {
                active = { fg = "#141415", bg = "#b4d4cf", gui = "bold" },
                inactive = { fg = "#606079", bg = "#1c1c24" },
              },
              symbols = { modified = " ●", alternate_file = "" },
            },
          },
          lualine_x = {},
          lualine_y = {},
          lualine_z = {
            {
              "tabs",
              mode = 0,
              show_modified_status = true,
              tabs_color = {
                active = { fg = "#141415", bg = "#90a0b5", gui = "bold" },
                inactive = { fg = "#606079", bg = "#1c1c24" },
              },
              symbols = { modified = " ●" },
            },
          },
        },
      }

      config.options = {
        component_separators = "",
        section_separators = "",
        icons_enabled = true,
        theme = {
          normal = { c = { fg = colors.fg, bg = colors.bg } },
          inactive = { c = { fg = colors.comment, bg = colors.inactive_bg } },
        },
      }

      local function ins_left(component)
        table.insert(config.sections.lualine_c, component)
      end

      local function ins_right(component)
        table.insert(config.sections.lualine_x, component)
      end

      ins_left({
        function()
          return "▊"
        end,
        color = { fg = colors.keyword },
        padding = { left = 0, right = 1 },
      })

      ins_left({
        "filename",
        cond = conditions.buffer_not_empty,
        color = { fg = colors.property, gui = "bold" },
      })

      ins_left({
        "navic",
        color_correction = "static",
        cond = conditions.buffer_not_empty,
      })

      ins_left({
        "diagnostics",
        symbols = diag_icons,
        diagnostics_color = {
          error = { fg = colors.error },
          warn = { fg = colors.warning },
          info = { fg = colors.constant },
          hint = { fg = colors.hint },
        },
      })

      ins_left({
        function()
          return "%="
        end,
      })

      ins_right({
        "branch",
        icon = "",
        color = { fg = colors.constant, gui = "bold" },
      })

      ins_right({
        "diff",
        diff_color = {
          added = { fg = colors.plus },
          modified = { fg = colors.delta },
          removed = { fg = colors.error },
        },
        cond = conditions.hide_in_width,
      })

      ins_right({
        "lsp_status",
        color = { fg = colors.fg, gui = "bold" },
        ignore_lsp = { "copilot" },
      })

      ins_right({ "location", color = { fg = colors.type } })
      ins_right({ "progress", color = { fg = colors.fg, gui = "bold" } })

      ins_right({
        function()
          return "▊"
        end,
        color = { fg = colors.keyword },
        padding = { left = 1, right = 0 },
      })

      require("lualine").setup(config)
    end,
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    config = function()
      require("which-key").setup({
        preset = "helix",
        spec = {
          { mode = { "n", "v" }, { "<leader>l", group = "lsp" } },
          { "<leader>f", group = "find" },
          { "<leader>g", group = "git" },
          { "<leader>gh", group = "hunks" },
          { "<leader>n", group = "annotations" },
          { "<leader>q", group = "session" },
        },
      })
    end,
  },
  {
    "brenoprata10/nvim-highlight-colors",
    ft = {
      "css",
      "scss",
      "less",
      "html",
      "javascript",
      "javascriptreact",
      "typescript",
      "typescriptreact",
      "svelte",
      "vue",
    },
    opts = { render = "background", enable_tailwind = true },
  },
  {
    "folke/todo-comments.nvim",
    cmd = { "TodoQuickFix", "TodoLocList", "TodoTrouble", "TodoTelescope", "TodoFzfLua" },
    event = { "BufReadPost", "BufNewFile" },
    opts = {},
  },
}
