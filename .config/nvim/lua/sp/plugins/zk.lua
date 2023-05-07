local function map(mode, lhs, rhs, opts) -- {{{
  opts.buffer = opts.buffer == nil and true or opts.buffer
  opts.noremap = opts.noremap == nil and true or opts.noremap
  opts.silent = opts.silent == nil and true or opts.silent
  vim.keymap.set(mode, lhs, rhs, opts)
end --- }}}

local config = {
  "mickael-menu/zk-nvim",
  ft = { "markdown" },
  config = function()
    require("zk").setup({
      lsp = {
        config = {
          on_attach = function(client, bufnr)
            map('n', '<cr>', vim.lsp.buf.definition, { buffer = bufnr })
            map('n', 'K', vim.lsp.buf.hover, { buffer = bufnr })
            map("v", "<leader>za", ":'<,'>lua vim.lsp.buf.range_code_action()<CR>",
              { buffer = bufnr, desc = "[Z]k Code [A]ctions" })
            map("n", "<leader>zb", "<Cmd>ZkBacklinks<CR>", { buffer = bufnr, desc = "[Z]k [B]acklinks" })
            -- Create a new note in the same directory as the current buffer, using the current selection for title.
            map("v", "<leader>znt", ":'<,'>ZkNewFromTitleSelection { dir = vim.fn.expand('%:p:h') }<CR>",
              { buffer = bufnr })
            -- Create a new note in the same directory as the current buffer, using the current selection for note content and asking for its title.
            map("v", "<leader>znc",
              ":'<,'>ZkNewFromContentSelection { dir = vim.fn.expand('%:p:h'), title = vim.fn.input('Title: ') }<CR>",
              { buffer = bufnr })
            map("n", "<leader>zi", "<cmd>ZkIndex<cr>", { desc = "[Z]k [I]ndex", buffer = bufnr })
            map("n", "<leader>zo", "<Cmd>ZkNew { title = vim.fn.input('Title: ') }<CR>",
              { desc = "[Z]k [N]new", buffer = bufnr, silent = false })
            map("n", "<leader>zo", "<Cmd>ZkNotes { sort = { 'modified' } }<CR>",
              { desc = "[Z]k [O]pen Notes", buffer = bufnr })
            map("n", "<leader>zf", "<Cmd>ZkNotes { sort = { 'modified' }, match = { vim.fn.input('Search: ') } }<CR>",
              { buffer = bufnr, desc = "[Z]k Notes [F]ind" })
            map("v", "<leader>zf", ":'<,'>ZkMatch<CR>", { buffer = bufnr, desc = "[Z]k Notes [F]ind" })
            map("n", "<leader>zc", "<cmd>ZkCd<cr>", { desc = "[Z]k [C]d into notes", buffer = bufnr })
            map("n", "<leader>zl", "<cmd>ZkLinks<cr>", { desc = "[Z]k [L]inks picker", buffer = bufnr })
            print("ZK server attached")
          end
        }
      }
    })
  end
}
return config
