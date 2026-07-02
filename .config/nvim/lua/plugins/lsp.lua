local gh = require("config.utils").gh

local M = {}

vim.pack.add({
  gh "mason-org/mason.nvim",
  gh "neovim/nvim-lspconfig",
  gh "b0o/SchemaStore.nvim",
})

local mason_packages = {
  "tree-sitter-cli",
  "prettierd",
  "stylua",
  "ruff",
  "lua-language-server",
  "tailwindcss-language-server",
  "css-variables-language-server",
  "cssmodules-language-server",
  "docker-compose-language-service",
  "dockerfile-language-server",
  -- "tsgo",
  "vtsls",
  "pyrefly",
  "html-lsp",
  "css-lsp",
  "json-lsp",
  "eslint-lsp",
  "yaml-language-server",
  "taplo",
  "emmet-language-server",
  "gopls",
  "goimports",
  "gofumpt",
  "gomodifytags",
  "impl",
}

local servers = {
  "lua_ls",
  -- "emmylua_ls",
  -- "lua-lang-server",
  "eslint",
  "tsgo",
  -- "vtsls",
  "pyrefly",
  "emmet_language_server",
  "html",
  "cssls",
  "css_variables",
  "cssmodules_ls",
  "jsonls",
  "yamlls",
  "taplo",
  "gopls",
  "tailwindcss",
  "docker_compose_language_service",
  "docker_language_server",
}

local pick_fallbacks = {
  declaration = vim.lsp.buf.declaration,
  definition = vim.lsp.buf.definition,
  implementation = vim.lsp.buf.implementation,
  references = vim.lsp.buf.references,
  type_definition = vim.lsp.buf.type_definition,
  document_symbol = vim.lsp.buf.document_symbol,
  workspace_symbol = function()
    vim.lsp.buf.workspace_symbol ""
  end,
}

function M.ensure_mason_packages(packages)
  local ok, registry = pcall(require, "mason-registry")
  if not ok then
    return
  end

  registry.refresh(function()
    for _, name in ipairs(packages) do
      local pkg_ok, pkg = pcall(registry.get_package, name)
      if pkg_ok and not pkg:is_installed() and not pkg:is_installing() then
        pkg:install()
      end
    end
  end)
end

function M.pick_or_fallback(scope)
  return function()
    local ok, extra = pcall(require, "mini.extra")
    if ok then
      local picked = pcall(extra.pickers.lsp, { scope = scope })
      if picked then
        return
      end
    end

    local fallback = pick_fallbacks[scope]
    if fallback ~= nil then
      fallback()
    end
  end
end

---@param bufnr number
local function toggle_inlay_hints(bufnr)
  vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = bufnr }), { bufnr = bufnr })
end

---@param bufnr number
local function enable_lsp_folding(bufnr)
  for _, win in ipairs(vim.fn.win_findbuf(bufnr)) do
    vim.api.nvim_set_option_value("foldmethod", "expr", { win = win })
    vim.api.nvim_set_option_value("foldexpr", "v:lua.vim.lsp.foldexpr()", { win = win })
    -- vim.api.nvim_set_option_value("foldtext", "v:lua.vim.lsp.foldtext()", { win = win })
  end
end

function M.on_attach(client, bufnr)
  local map = function(mode, lhs, rhs, desc)
    vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc, silent = true })
  end

  -- local is_vtsls = client.name == "vtsls"

  local function organize_imports()
    vim.lsp.buf.code_action({
      apply = true,
      context = {
        only = { "source.organizeImports" },
        diagnostics = {},
      },
    })
  end

  map("n", "<leader>ld", M.pick_or_fallback "definition", "Definition")
  map("n", "<leader>lD", M.pick_or_fallback "declaration", "Declaration")
  map("n", "<leader>lR", M.pick_or_fallback "references", "References")
  map("n", "<leader>li", M.pick_or_fallback "implementation", "Implementation")
  map("n", "<leader>lt", M.pick_or_fallback "type_definition", "Type Definition")
  map("n", "<leader>ls", M.pick_or_fallback "document_symbol", "Document Symbols")
  map("n", "<leader>lS", M.pick_or_fallback "workspace_symbol", "Workspace Symbols")

  map("n", "gd", M.pick_or_fallback "definition", "Defination")
  map("n", "grd", M.pick_or_fallback "definition", "Defination")
  map("n", "grD", M.pick_or_fallback "declaration", "Declaration")
  map("n", "grr", M.pick_or_fallback "references", "References")
  map("n", "gri", M.pick_or_fallback "implementation", "Implementation")
  map("n", "grt", M.pick_or_fallback "type_definition", "Type Definition")
  map("n", "gro", M.pick_or_fallback "document_symbol", "Document Symbols")
  map("n", "grO", M.pick_or_fallback "workspace_symbol", "Workspace Symbols")
  map({ "n", "x" }, "grf", function()
    local ok, conform = pcall(require, "conform")
    if ok then
      conform.format({ async = true, lsp_format = "fallback" })
    else
      vim.lsp.buf.format({ bufnr = bufnr, async = true })
    end
  end, "Format")
  map("n", "grh", function()
    toggle_inlay_hints(bufnr)
  end, "Toggle Inlay Hints")

  map({ "n", "x" }, "<leader>la", vim.lsp.buf.code_action, "Code Action")
  map("n", "<leader>lr", vim.lsp.buf.rename, "Rename")
  map({ "n", "x" }, "<leader>lf", function()
    local ok, conform = pcall(require, "conform")
    if ok then
      conform.format({ async = true, lsp_format = "fallback" })
    else
      vim.lsp.buf.format({ bufnr = bufnr, async = true })
    end
  end, "Format")
  map("n", "<leader>lh", function()
    toggle_inlay_hints(bufnr)
  end, "Toggle Inlay Hints")
  map("n", "<leader>ll", "<cmd>checkhealth vim.lsp<cr>", "Info")
  map("n", "<leader>lL", "<cmd>lsp restart<cr>", "Restart")
  map("n", "<leader>lo", organize_imports, "Organize Imports")
  map("n", "grs", organize_imports, "Sort Imports")

  if client:supports_method "textDocument/foldingRange" then
    enable_lsp_folding(bufnr)
  end

  if client:supports_method "textDocument/semanticTokens/full" then
    vim.lsp.semantic_tokens.enable(true, { bufnr = bufnr })
  end
end

local function setup_capabilities()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = require("blink.cmp").get_lsp_capabilities(capabilities)
  -- capabilities = vim.tbl_deep_extend("force", capabilities, require("mini.completion").get_lsp_capabilities())

  vim.lsp.config("*", {
    capabilities = capabilities,
  })
end

local function setup_servers()
  local schemastore = require "schemastore"
  local library = vim.api.nvim_get_runtime_file("", true)

  vim.lsp.config("lua_ls", {
    settings = {
      Lua = { workspace = { library = library } },
    },
  })

  vim.lsp.config("tsgo", {
    settings = {
      ["js/ts"] = {
        preferGoToSourceDefinition = true,
        updateImportsOnFileMove = { enabled = "always" },
        preferences = {
          jsxAttributeCompletionStyle = "auto",
          preferTypeOnlyAutoImports = true,
        },
      },
    },
  })

  local typescript = {
    updateImportsOnFileMove = { enabled = "always" },
    preferGoToSourceDefinition = true,
    preferences = { preferTypeOnlyAutoImports = true },
    inlayHints = {
      enumMemberValues = { enabled = true },
      functionLikeReturnTypes = { enabled = true },
      parameterNames = { enabled = "literals" },
      parameterTypes = { enabled = true },
      propertyDeclarationTypes = { enabled = true },
      variableTypes = { enabled = false },
    },
  }
  vim.lsp.config("vtsls", {
    settings = {
      complete_function_calls = true,
      vtsls = {
        enableMoveToFileCodeAction = true,
        autoUseWorkspaceTsdk = true,
        experimental = { maxInlayHintLength = 30 },
      },
      javascript = typescript,
      typescript = typescript,
    },
  })

  vim.lsp.config("pyrefly", {
    init_options = {
      pyrefly = {
        typeCheckingMode = "auto",
        analysis = {
          diagnosticMode = "openFilesOnly",
          inlayHints = {
            callArgumentNames = "off",
            functionReturnTypes = true,
            variableTypes = true,
          },
          showHoverGoToLinks = true,
        },
        streamDiagnostics = true,
      },
      commentFoldingRanges = true,
    },
  })

  vim.lsp.config("html", {
    init_options = {
      provideFormatter = true,
      embeddedLanguages = { css = true, javascript = true },
      configurationSection = { "html", "css", "javascript" },
    },
    settings = {
      html = {
        hover = { documentation = true, references = true },
        validate = { scripts = true, styles = true },
      },
    },
  })

  vim.lsp.config("cssls", {
    settings = {
      css = { validate = true },
      less = { validate = true },
      scss = { validate = true },
    },
  })

  vim.lsp.config("jsonls", {
    settings = { json = { schemas = schemastore.json.schemas(), validate = { enable = true } } },
  })

  vim.lsp.config("eslint", {
    on_attach = function(_, bufnr)
      -- vim.api.nvim_create_autocmd({ "BufWritePre" }, {
      -- 	desc = "Run Eslint on Save",
      -- 	buf = bufnr,
      -- 	command = "LspEslintFixAll",
      -- })
      vim.keymap.set(
        "n",
        "<leader>le",
        "<cmd>LspEslintFixAll<cr>",
        { buffer = bufnr, desc = "Eslint Fix", silent = true }
      )
    end,
  })

  vim.lsp.config("yamlls", {
    settings = {
      redhat = { telemetry = { enabled = false } },
      yaml = {
        completion = true,
        format = { enable = true },
        hover = true,
        keyOrdering = false,
        schemaStore = { enable = false, url = "" },
        schemas = schemastore.yaml.schemas(),
        validate = true,
      },
    },
  })

  vim.lsp.config("taplo", {
    root_markers = { "starship.toml", ".taplo.toml", "taplo.toml", ".git" },
    settings = {
      evenBetterToml = {
        schema = {
          enabled = true,
          associations = {
            ["starship.toml"] = "https://starship.rs/config-schema.json",
          },
        },
        formatter = { alignEntries = true, columnWidth = 80, trailingNewline = true },
      },
    },
  })

  vim.lsp.enable(servers)
end

function M.setup()
  require("mason").setup({
    ui = {
      icons = {
        package_installed = "✓",
        package_pending = "⟳",
        package_uninstalled = "✗",
      },
    },
  })
  setup_capabilities()
  setup_servers()

  vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("ConfigLsp", { clear = true }),
    desc = "Configure LSP buffer UX",
    callback = function(event)
      local client = vim.lsp.get_client_by_id(event.data.client_id)
      if client ~= nil then
        M.on_attach(client, event.buf)
      end
    end,
  })

  vim.api.nvim_create_autocmd("UIEnter", {
    group = vim.api.nvim_create_augroup("ConfigMasonInstall", { clear = true }),
    desc = "Install missing Mason LSP packages",
    once = true,
    callback = function()
      M.ensure_mason_packages(mason_packages)
    end,
  })

  if #vim.api.nvim_list_uis() > 0 then
    vim.schedule(function()
      M.ensure_mason_packages(mason_packages)
    end)
  end
end

M.setup()

return M
