local gh = function(x)
  return "https://github.com/" .. x
end

vim.pack.add({
  { src = gh "saghen/blink.cmp", version = vim.version.range "1.*" },
  { src = gh "mikavilpas/blink-ripgrep.nvim", version = vim.version.range "2.*" },
})

local blink_icon = function(ctx)
  if ctx.source_name == "Path" then
    local data = ctx.item.data or {}
    local category = ctx.kind == "Folder" and "directory" or "file"
    local name = data.full_path or data.path or ctx.label
    local icon, hl = MiniIcons.get(category, name)
    return icon or ctx.kind_icon, hl or ctx.kind_hl
  end

  local icon, hl = MiniIcons.get("lsp", ctx.kind)
  return icon or ctx.kind_icon, hl or ctx.kind_hl
end

local blink_normal_buffers = function()
  return vim.tbl_filter(function(buf)
    return vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].buftype == ""
  end, vim.api.nvim_list_bufs())
end

local prefer_shorter_prefix = function(a, b)
  if a.label == b.label then
    return
  end

  local a_prefixes_b = b.label:sub(1, #a.label) == a.label
  local b_prefixes_a = a.label:sub(1, #b.label) == b.label
  if a_prefixes_b ~= b_prefixes_a then
    return a_prefixes_b
  end
end

require("blink.cmp").setup({
  snippets = { preset = "mini_snippets" },
  keymap = {
    preset = "enter",
    ["<C-j>"] = { "select_next", "fallback" },
    ["<C-k>"] = { "select_prev", "fallback" },

    ["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
    ["<S-Tab>"] = { "select_prev", "snippet_backward", "fallback" },
  },
  -- appearance = { nerd_font_variant = "mono" },
  completion = {
    accept = { auto_brackets = { enabled = true } },
    documentation = { auto_show = true, window = { border = "rounded" } },
    list = { selection = { preselect = true, auto_insert = true } },
    menu = {
      border = "rounded",
      draw = {
        columns = {
          { "kind_icon" },
          { "label", "label_description", gap = 1 },
          { "source_name" },
        },
        -- components = {
        --   kind_icon = {
        --     text = function(ctx)
        --       local icon = blink_icon(ctx)
        --       return icon .. ctx.icon_gap
        --     end,
        --     highlight = function(ctx)
        --       local _, hl = blink_icon(ctx)
        --       return hl
        --     end,
        --   },
        --   source_name = {
        --     width = { max = 12 },
        --     text = function(ctx)
        --       return ctx.source_name
        --     end,
        --     highlight = "BlinkCmpSource",
        --   },
        -- },
        treesitter = { "lsp" },
      },
    },
  },
  cmdline = {
    enabled = true,
    keymap = {
      preset = "cmdline",
      ["<Left>"] = false,
      ["<Right>"] = false,
      ["<C-j>"] = { "select_next", "fallback" },
      ["<C-k>"] = { "select_prev", "fallback" },
    },
    completion = {
      list = { selection = { preselect = false, auto_insert = true } },
      menu = {
        auto_show = function()
          return vim.tbl_contains({ ":", "/", "?" }, vim.fn.getcmdtype())
        end,
      },
    },
  },
  fuzzy = {
    implementation = "prefer_rust",
    sorts = {
      function(a, b)
        if (a.client_name == nil or b.client_name == nil) or (a.client_name == b.client_name) then
          return
        end
        return b.client_name == "emmet_ls" or b.client_name == "emmet_language_server"
      end,
      "score",
      "exact",
      "sort_text",
    },
  },
  signature = { enabled = true, window = { border = "rounded" } },
  sources = {
    default = { "lsp", "path", "snippets", "buffer", "ripgrep" },
    providers = {
      lsp = { fallbacks = {} },
      snippets = { score_offset = -1 },
      buffer = { opts = { get_bufnrs = blink_normal_buffers } },
      ripgrep = {
        module = "blink-ripgrep",
        name = "Ripgrep",
        score_offset = -10,
        async = true,
        timeout_ms = 100,
        min_keyword_length = 4,
        opts = {
          prefix_min_len = 4,
          project_root_marker = { ".git", "package.json", "pyproject.toml", "Cargo.toml", "go.mod" },
          fallback_to_regex_highlighting = true,
          backend = {
            customize_icon_highlight = true,
            ripgrep = {
              max_filesize = "1M",
              project_root_fallback = true,
              search_casing = "--smart-case",
            },
          },
        },
      },
    },
  },
})
