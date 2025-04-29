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

vim.opt.clipboard = "unnamedplus" -- allows neovim to access the system clipboard
-- Set wsl-clipboard for vim clipboard if running WSL
-- Check if the current linux kernal is microsoft WSL version
local function is_wsl()
  local version_file = io.open("/proc/version", "rb")
  if vim.fn.executable "wcopy" == 1 and version_file ~= nil and string.find(version_file:read "*a", "microsoft") then
    version_file:close()
    return true
  end
  return false
end

-- If current linux is under WSL then use wclip.exe
-- More info: https://github.com/memoryInject/wsl-clipboard
if is_wsl() then
  -- vim.g.clipboard = {
  --   name = "wsl-clipboard",
  --   copy = {
  --     ["+"] = "wcopy",
  --     ["*"] = "wcopy",
  --   },
  --   paste = {
  --     ["+"] = "wpaste",
  --     ["*"] = "wpaste",
  --   },
  --   cache_enabled = true,
  -- }
  vim.g.clipboard = {
    name = "win32yank",
    copy = {
      ["+"] = "win32yank.exe -i --crlf",
      ["*"] = "win32yank.exe -i --crlf",
    },
    paste = {
      ["+"] = "win32yank.exe -o --lf",
      ["*"] = "win32yank.exe -o --lf",
    },
    cache_enabled = 0,
  }
end

function Fd(file_pattern, _)
  -- if first char is * then fuzzy search
  if file_pattern:sub(1, 1) == "*" then file_pattern = file_pattern:gsub(".", ".*%0") .. ".*" end
  local cmd = 'fd  --color=never --full-path --type file "' .. file_pattern .. '"'
  local result = vim.fn.systemlist(cmd)
  return result
end
if vim.fn.has "nvim-0.11" == 1 and vim.fn.executable "fd" then vim.opt.findfunc = "v:lua.Fd" end

for i = 1, 9 do
  vim.keymap.set("n", "<leader>" .. i, i .. "gt", { desc = "Goto tab " .. i })
end
