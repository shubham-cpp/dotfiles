vim.opt.path:append "**"
vim.opt.iskeyword:append "-" -- for css make 'is-active' as one word
vim.opt.wildignore:append {
  "**/node_modules/*",
  "**/.git/*",
  "**/dist/*",
  "**/build/*",
  "**/static/*",
  "**/.next/*",
  "*.o",
  "*.out",
  "*.obj",
  "*.exe",
  "*.dll",
  "*.jar",
  "*.pyc",
  "*.rbc",
  "*.class",
  "*.gif",
  "*.ico",
  "*.jpg",
  "*.jpeg",
  "*.png",
  "*.mov",
  "*.mht",
  "*.swp",
  "*.zip",
  "*.tar.gz",
  "*.tar.bz2",
}
for i = 1, 9 do
  vim.keymap.set("n", "<leader>" .. i, i .. "gt", { desc = "Goto tab " .. i })
end
