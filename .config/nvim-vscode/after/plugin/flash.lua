local ok_flash, flash = pcall(require, "flash")
if ok_flash then
  flash.setup({
    modes = { char = { enabled = false, autohide = false } },
  })
  vim.keymap.set({ "n", "x" }, "s", flash.jump, { desc = "Flash" })
  vim.keymap.set({ "n", "o" }, "S", flash.treesitter, { desc = "Flash" })
  vim.keymap.set("o", "r", flash.remote, { desc = "Remote Flash" })
  vim.keymap.set("c", "<c-f>", flash.toggle, { desc = "Toggle Flash Search" })
end
