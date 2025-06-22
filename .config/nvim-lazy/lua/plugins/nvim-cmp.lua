local function has_words_before()
  local line, col = (unpack or table.unpack)(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end
local function is_visible(cmp)
  return cmp.core.view:visible() or vim.fn.pumvisible() == 1
end

---@param kind lsp.CompletionItemKind | number
---@return cmp.ComparatorFunction
local function deprio(kind)
  return function(e1, e2)
    if e1:get_kind() == kind then
      return false
    end
    if e2:get_kind() == kind then
      return true
    end
  end
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

local function format(_, item)
  local icons = LazyVim.config.icons.kinds

  if icons[item.kind] then
    item.kind = icons[item.kind] .. item.kind
  end

  local widths = {
    abbr = vim.g.cmp_widths and vim.g.cmp_widths.abbr or 40,
    menu = vim.g.cmp_widths and vim.g.cmp_widths.menu or 30,
  }

  for key, width in pairs(widths) do
    if item[key] and vim.fn.strdisplaywidth(item[key]) > width then
      item[key] = vim.fn.strcharpart(item[key], 0, width - 1) .. "…"
    end
  end

  return item
end
---@type LazySpec
return {
  {
    "hrsh7th/nvim-cmp",
    optional = true,
    dependencies = {
      "https://codeberg.org/FelipeLema/cmp-async-path",
      "lukas-reineke/cmp-rg",
    },
    event = { "InsertEnter", "CmdlineEnter" },
    -- opts = function()
    --   local luasnip, cmp = require("luasnip"), require("cmp")
    --   local types = require("cmp.types")
    --   local defaults = require("cmp.config.default")()
    --
    --   return {
    --     preselect = cmp.PreselectMode.None,
    --     formatting = { format = format },
    --     sources = cmp.config.sources({
    --       { name = "lazydev" },
    --       { name = "nvim_lsp" },
    --       { name = "luasnip" },
    --       { name = "async_path" },
    --       { name = "buffer" },
    --       {
    --         name = "rg",
    --         keyword_length = 3,
    --         max_item_count = 10,
    --         option = { additional_arguments = "--smart-case" },
    --       },
    --     }),
    --     sorting = {
    --       priority_weight = defaults.sorting.priority_weight,
    --       comparators = vim.tbl_extend("keep", {
    --         deprio(types.lsp.CompletionItemKind.Text),
    --         lower_emmet(),
    --       }, defaults.sorting.comparators),
    --     },
    --     mapping = {
    --       ["<Up>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
    --       ["<Down>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
    --
    --       ["<C-P>"] = cmp.mapping(function()
    --         if is_visible(cmp) then
    --           cmp.select_prev_item()
    --         else
    --           cmp.complete()
    --         end
    --       end),
    --       ["<C-N>"] = cmp.mapping(function()
    --         if is_visible(cmp) then
    --           cmp.select_next_item()
    --         else
    --           cmp.complete()
    --         end
    --       end),
    --       ["<C-K>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "c" }),
    --       ["<C-J>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "c" }),
    --
    --       ["<C-U>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
    --       ["<C-D>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
    --       ["<C-B>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
    --       ["<C-F>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
    --
    --       ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
    --       ["<C-E>"] = cmp.mapping(cmp.mapping.abort(), { "i", "c" }),
    --       ["<CR>"] = cmp.mapping(cmp.mapping.confirm({ select = false }), { "i", "c" }),
    --       ["<C-y>"] = LazyVim.cmp.confirm({ select = true }),
    --
    --       ["<Tab>"] = cmp.mapping(function(fallback)
    --         if is_visible(cmp) then
    --           cmp.select_next_item()
    --         elseif vim.api.nvim_get_mode().mode ~= "c" and luasnip.expand_or_locally_jumpable() then
    --           luasnip.expand_or_jump()
    --         elseif has_words_before() then
    --           cmp.complete()
    --         else
    --           fallback()
    --         end
    --       end, { "i", "s" }),
    --       ["<S-Tab>"] = cmp.mapping(function(fallback)
    --         if is_visible(cmp) then
    --           cmp.select_prev_item()
    --         elseif vim.api.nvim_get_mode().mode ~= "c" and luasnip.jumpable(-1) then
    --           luasnip.jump(-1)
    --         else
    --           fallback()
    --         end
    --       end, { "i", "s" }),
    --       ["<C-x><C-x>"] = cmp.mapping.complete({
    --         config = { sources = { { name = "luasnip" } } },
    --       }),
    --     },
    --     snippet = {
    --       expand = function(item)
    --         return LazyVim.cmp.expand(item.body)
    --       end,
    --     },
    --   }
    -- end,
    opts = function(_, opts)
      local cmp = require("cmp")
      local types = require("cmp.types")
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

      -- opts.mapping["<CR>"] =
      --   cmp.mapping(cmp.mapping.confirm({ select = true }), { "i", "c" })

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
        { name = "lazydev" },
        { name = "nvim_lsp" },
        { name = "luasnip" },
        { name = "async_path" },
        { name = "buffer" },
        {
          name = "rg",
          keyword_length = 3,
          -- max_item_count = 10,
          -- group_index = 2,
          -- priority = 200,
          option = { additional_arguments = "--smart-case" },
        },
      })
      opts.sorting = {
        priority_weight = defaults.sorting.priority_weight,
        comparators = vim.tbl_extend("keep", {
          deprio(types.lsp.CompletionItemKind.Text),
          lower_emmet(),
        }, defaults.sorting.comparators),
      }
    end,
  },
  {
    "hrsh7th/cmp-cmdline",
    optional = true,
    keys = { ":", "/", "?" }, -- lazy load cmp on more keys along with insert mode
    dependencies = { "hrsh7th/nvim-cmp" },
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
          mapping = cmp.mapping.preset.cmdline(),
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
