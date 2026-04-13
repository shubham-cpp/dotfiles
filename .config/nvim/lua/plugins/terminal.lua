return {
  url = "akinsho/toggleterm.nvim",
  config = function()
    require("toggleterm").setup({
      open_mapping = [[<c-\>]],
      direction = "float",
      shell = "fish",
      on_open = function(term)
        local bufnr = term.bufnr
        vim.opt_local.foldexpr = ""
        vim.opt_local.foldmethod = "manual"
        vim.keymap.set("t", "<C-]>", "<C-\\><C-n>", { buffer = bufnr, desc = "Normal mode" })
      end,
    })

    -- LazyGit integration
    local Terminal = require("toggleterm.terminal").Terminal

    local lazygit_config_path = vim.fs.normalize(vim.fn.stdpath("cache") .. "/lazygit-config.yml")

    local lazygit_config_lines = {
      "os:",
      [[  edit: >-]],
      [[    if [ -z "$NVIM" ]; then nvim -- {{filename}}; else nvim --server "$NVIM" --remote-expr 'execute("LazygitHide")' && nvim --server "$NVIM" --remote {{filename}}; fi]],
      [[  editAtLine: >-]],
      [[    if [ -z "$NVIM" ]; then nvim +{{line}} -- {{filename}}; else nvim --server "$NVIM" --remote-expr 'execute("LazygitHide")' && nvim --server "$NVIM" --remote {{filename}} && nvim --server "$NVIM" --remote-send ':{{line}}<CR>'; fi]],
      "gui:",
      [[  nerdFontsVersion: "3"]],
    }

    vim.fn.writefile(lazygit_config_lines, lazygit_config_path)

    -- Preserve user's existing lazygit config
    local default_config = vim.fn.expand("~/.config/lazygit/config.yml")
    local configs = {}
    if vim.fn.filereadable(default_config) == 1 then
      table.insert(configs, default_config)
    end
    table.insert(configs, lazygit_config_path)
    vim.env.LG_CONFIG_FILE = table.concat(configs, ",")

    local lazygit_term = Terminal:new({
      cmd = "lazygit",
      direction = "float",
      hidden = true,
      on_open = function(term)
        vim.opt_local.foldexpr = ""
        vim.opt_local.foldmethod = "manual"
        vim.keymap.set("t", "<C-]>", "<C-\\><C-n>", { buffer = term.bufnr, desc = "Normal mode" })
      end,
    })

    vim.api.nvim_create_user_command("LazygitHide", function()
      if lazygit_term:is_open() then
        lazygit_term:close()
      end
    end, {})

    vim.keymap.set("n", "<leader>gg", function()
      lazygit_term:toggle()
    end, { desc = "LazyGit" })
  end,
}
