---@type LazySpec
return {
  {
    "bullets-vim/bullets.vim",
    ft = { "markdown", "gitcommit" },
    init = function()
      vim.g.bullets_set_mappings = 0
      vim.g.bullets_custom_mappings = {
        { "imap", "<cr>", "<Plug>(bullets-newline)" },
        { "inoremap", "<C-cr>", "<cr>" },
        { "nmap", "o", "<Plug>(bullets-newline)" },
        { "vmap", "gN", "<Plug>(bullets-renumber)" },
        { "nmap", "gN", "<Plug>(bullets-renumber)" },
        { "nmap", "<leader>x", "<Plug>(bullets-toggle-checkbox)" },
        { "imap", "<C-t>", "<Plug>(bullets-demote)" },
        { "vmap", "-", "<Plug>(bullets-demote)" },
        { "imap", "<C-d>", "<Plug>(bullets-promote)" },
        { "vmap", "+", "<Plug>(bullets-promote)" },
      }
      vim.g.bullets_delete_last_bullet_if_empty = 2
      vim.g.bullets_enabled_file_types = { "markdown", "gitcommit", "text" }
    end,
  },
}
