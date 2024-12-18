local o = vim.opt

o.iskeyword:append "-"
o.path:append "**"

o.scrolloff = 9

o.exrc = true

o.wrap = true
o.showbreak = "󰄾 "

o.backupdir = { vim.fn.stdpath "state" .. "/backup//" }

o.sessionoptions:append { "globals" }
o.sessionoptions:remove "folds"

o.wildignorecase = true
o.wildignore:append {
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

if vim.fn.executable "rg" == 1 then
  o.grepprg = "rg --vimgrep -uu --smart-case" -- Also check RIPGREP_CONFIG_PATH="$HOME/.config/ripgreprc"
end
