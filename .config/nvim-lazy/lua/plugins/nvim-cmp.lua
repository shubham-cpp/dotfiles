local function has_words_before()
  local line, col = (unpack or table.unpack)(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end
local function is_visible(cmp)
  return cmp.core.view:visible() or vim.fn.pumvisible() == 1
end

---@return cmp.ComparatorFunction
local function lower_emmet()
  return function(e1, e2)
    local is_e1_emmet = e1.source:get_debug_name() == "nvim_lsp:emmet_ls"
      or e1.source:get_debug_name() == "nvim_lsp:emmet_language_server"
    local is_e2_emmet = e2.source:get_debug_name() == "nvim_lsp:emmet_ls"
      or e2.source:get_debug_name() == "nvim_lsp:emmet_language_server"
    if is_e1_emmet then
      return false
    end
    if is_e2_emmet then
      return true
    end
    return nil
  end
end

---@type LazySpec
return {
  {
    "hrsh7th/nvim-cmp",
    optional = true,
    dependencies = {
      "https://codeberg.org/FelipeLema/cmp-async-path",
      "lukas-reineke/cmp-rg",
      "hrsh7th/cmp-cmdline",
    },
    event = { "InsertEnter", "CmdlineEnter" },
    opts = function(_, opts)
      local cmp = require("cmp")
      local defaults = require("cmp.config.default")()

      -- vim.opt.completeopt:append("noselect")
      -- opts.completion = { completeopt = "menu,menuone,noselect", }
      -- opts.completion = { completeopt = "menu,menuone,noinsert" }
      -- opts.preselect = cmp.PreselectMode.None
      -- opts.confirm_opts = {
      --   behavior = cmp.ConfirmBehavior.Replace,
      --   select = false,
      -- }

      opts.mapping["<C-x><C-x>"] = cmp.mapping.complete({
        config = { sources = { { name = "luasnip" } } },
      })

      opts.mapping["<CR>"] = cmp.mapping({
        i = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
        c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false }),
      })

      opts.mapping["<C-j>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "c" })
      opts.mapping["<C-k>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "c" })

      opts.mapping["<Tab>"] = cmp.mapping(function(fallback)
        local ok, luasnip = pcall(require, "luasnip")
        if is_visible(cmp) then
          cmp.select_next_item()
        elseif ok and vim.api.nvim_get_mode().mode ~= "c" and luasnip.expand_or_locally_jumpable() then
          luasnip.expand_or_jump()
        elseif vim.api.nvim_get_mode().mode ~= "c" and vim.snippet and vim.snippet.active({ direction = 1 }) then
          vim.schedule(function()
            vim.snippet.jump(1)
          end)
        elseif has_words_before() then
          cmp.complete()
        else
          fallback()
        end
      end)
      opts.mapping["<S-Tab>"] = cmp.mapping(function(fallback)
        local ok, luasnip = pcall(require, "luasnip")
        if is_visible(cmp) then
          cmp.select_prev_item()
        elseif ok and vim.api.nvim_get_mode().mode ~= "c" and luasnip.jumpable(-1) then
          luasnip.jump(-1)
        elseif vim.api.nvim_get_mode().mode ~= "c" and vim.snippet and vim.snippet.active({ direction = -1 }) then
          vim.schedule(function()
            vim.snippet.jump(-1)
          end)
        else
          fallback()
        end
      end, { "i", "s" })

      opts.experimental = { ghost_text = false }

      opts.sources = cmp.config.sources({
        { name = "lazydev", priority = 1000, group_index = 0 },
        { name = "nvim_lsp", priority = 1000, group_index = 1 },
        { name = "async_path", priority = 1000, group_index = 1 },
        { name = "luasnip", priority = 950, group_index = 1 },
        {
          name = "buffer",
          group_index = 2,
          priority = 350,
          option = {
            get_bufnrs = function()
              return vim.fn.tabpagebuflist()
            end,
          },
        },
        {
          name = "rg",
          keyword_length = 3,
          max_item_count = 10,
          group_index = 2,
          priority = 200,
          option = { additional_arguments = "--smart-case" },
        },
      })
      opts.sorting = {
        priority_weight = defaults.sorting.priority_weight,
        comparators = vim.tbl_extend("keep", {
          lower_emmet(),
        }, defaults.sorting.comparators),
      }
    end,
  },
  {
    "folke/noice.nvim",
    optional = true,
    -- enabled = false,
    opts = {
      cmdline = { enabled = false },
      messages = { enabled = false },
      popupmenu = {
        ---@type 'nui'|'cmp'
        backend = "cmp",
      },
      ---@type NoicePresets
      presets = {
        bottom_search = false,
        command_palette = true,
        lsp_doc_border = true,
      },
    },
  },
  {
    "hrsh7th/cmp-cmdline",
    keys = { ":", "/", "?" }, -- lazy load cmp on more keys along with insert mode
    dependencies = "hrsh7th/nvim-cmp",
    opts = function()
      local cmp = require("cmp")
      return {
        {
          type = "/",
          completion = { completeopt = "menu,menuone,noselect" },
          mapping = cmp.mapping.preset.cmdline(),
          sources = {
            { name = "buffer" },
          },
        },
        {
          type = ":",
          completion = { completeopt = "menu,menuone,noselect" },
          mapping = cmp.mapping.preset.cmdline({
            ["<Tab>"] = cmp.mapping({
              c = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
              i = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
            }),
            ["<C-j>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
            ["<CR>"] = cmp.mapping({
              i = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
              c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false }),
            }),
          }),
          sources = cmp.config.sources({
            { name = "path" },
          }, {
            {
              name = "cmdline",
              option = {
                ignore_cmds = { "Man", "!", "find", "grep", "vimgrep" },
              },
            },
          }),
          matching = { disallow_symbol_nonprefix_matching = false },
        },
      }
    end,
    config = function(_, opts)
      local cmp = require("cmp")
      vim.tbl_map(function(val)
        cmp.setup.cmdline(val.type, val)
      end, opts)
    end,
  },
}
