vim.keymap.set({ "n", "v" }, "0", "^")
vim.keymap.set("n", "<Esc>", function()
  vim.cmd "nohl"
  return "<Esc>"
end, { expr = true, silent = true })

vim.keymap.set("n", ",s", [[:let @/='\<'.expand('<cword>').'\>'<CR>cgn]])
vim.keymap.set("v", ",s", '"sy:let @/=@s<CR>cgn')
-- Pasting in visual mode doesn't copy
vim.keymap.set("x", "p", [[ 'pgv"'.v:register.'y' ]], { expr = true })
vim.keymap.set("n", "dl", '"_dl')
vim.keymap.set("v", "D", '"_D')
vim.keymap.set({ "n", "v" }, "c", '"_c')
vim.keymap.set("n", "C", '"_C')

vim.keymap.set(
  "n",
  "<LocalLeader>e",
  ':e <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = "Edit in same dir" }
)
vim.keymap.set(
  "n",
  "<LocalLeader>t",
  ':tabe <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = "Edit in same dir(Tab)" }
)
vim.keymap.set(
  "n",
  "<LocalLeader>v",
  ':vsplit <C-R>=expand("%:p:h") . "/" <CR>',
  { silent = false, desc = "Edit in same dir(Split)" }
)

vim.keymap.set("n", "gco", "o<esc>Vcx<esc><cmd>normal gcc<cr>fxa<bs>", { desc = "Add Comment Below" })
vim.keymap.set("n", "gcO", "O<esc>Vcx<esc><cmd>normal gcc<cr>fxa<bs>", { desc = "Add Comment Above" })

for i = 1, 9 do
  vim.keymap.set("n", "<Leader>" .. i, i .. "gt", { desc = "Goto Tab" .. i })
end

if vim.g.vscode == nil then
  vim.keymap.set("n", "<C-j>", "<C-w><C-j>")
  vim.keymap.set("n", "<C-k>", "<C-w><C-k>")
  vim.keymap.set("n", "<C-h>", "<C-w><C-h>")
  vim.keymap.set("n", "<C-l>", "<C-w><C-l>")

  vim.keymap.set("n", ",w", "<cmd>w!<cr>")
  vim.keymap.set("n", ",W", "<cmd>noautocmd w!<cr>")
  vim.keymap.set("n", "zp", "vaBo^<Esc>")
else
  local function repeat_cmd(cmd, mode)
    local n = vim.v.count1
    if mode == "v" then
      local start_line = vim.fn.line "'<" - 1 -- 0-indexed for VSCode
      local end_line = vim.fn.line "'>" - 1 -- 0-indexed for VSCode
      -- Validate line numbers
      local line_count = vim.api.nvim_buf_line_count(0)
      if start_line < 0 or end_line < 0 or start_line >= line_count or end_line >= line_count then
        return -- Skip if lines are invalid
      end
      for i = 1, n do
        require("vscode").call(cmd, { range = { start_line, end_line }, restore_selection = true })
      end
    else
      for i = 1, n do
        require("vscode").call(cmd)
      end
    end
  end
  vim.keymap.set("n", "j", function()
    repeat_cmd("cursorDown", "n")
  end, { silent = true })
  vim.keymap.set("n", "k", function()
    repeat_cmd("cursorUp", "n")
  end, { silent = true })

  -- vim.keymap.set("i", "<c-d>", "<Cmd>call VSCodeNotify('editor.action.addSelectionToNextFindMatch')<cr>")
  -- vim.keymap.set("i", "<c-s-d>", "<Cmd>call VSCodeNotify('editor.action.previousSelectionMatchFindAction')<cr>")
  -- vim.keymap.set("i", "<c-x>", "<Cmd>call VSCodeNotify('editor.action.moveSelectionToNextFindMatch')<cr>")

  vim.keymap.set("n", "<c-j>", "<Cmd>call VSCodeNotify('workbench.action.terminal.toggleTerminal')<cr>")

  vim.keymap.set("n", "ze", "<Cmd>call VSCodeNotify('scrollLineDown')<CR>")
  vim.keymap.set("n", "zy", "<Cmd>call VSCodeNotify('scrollLineUp')<CR>")

  vim.keymap.set("n", "[d", "<Cmd>call VSCodeNotify('editor.action.marker.prev')<CR>")
  vim.keymap.set("n", "]d", "<Cmd>call VSCodeNotify('editor.action.marker.next')<CR>")
  vim.keymap.set("n", "[D", "<Cmd>call VSCodeNotify('editor.action.marker.prevInFiles')<CR>")
  vim.keymap.set("n", "]D", "<Cmd>call VSCodeNotify('editor.action.marker.nextInFiles')<CR>")
  vim.keymap.set("n", "[e", "<Cmd>call VSCodeNotify('editor.action.marker.prev')<CR>")
  vim.keymap.set("n", "]e", "<Cmd>call VSCodeNotify('editor.action.marker.next')<CR>")
  vim.keymap.set("n", "[E", "<Cmd>call VSCodeNotify('editor.action.marker.prevInFiles')<CR>")
  vim.keymap.set("n", "]E", "<Cmd>call VSCodeNotify('editor.action.marker.nextInFiles')<CR>")

  vim.keymap.set("n", "[c", "<Cmd>call VSCodeNotify('workbench.action.editor.previousChange')<CR>")
  vim.keymap.set("n", "]c", "<Cmd>call VSCodeNotify('workbench.action.editor.nextChange')<CR>")

  vim.keymap.set("n", "[f", "<Cmd>call VSCodeNotify('search.action.focusPrevSearchResult')<CR>")
  vim.keymap.set("n", "]f", "<Cmd>call VSCodeNotify('search.action.focusNextSearchResult')<CR>")
  vim.keymap.set("n", "[q", "<Cmd>call VSCodeNotify('search.action.focusPrevSearchResult')<CR>")
  vim.keymap.set("n", "]q", "<Cmd>call VSCodeNotify('search.action.focusNextSearchResult')<CR>")

  vim.keymap.set("n", "[b", "<Cmd>call VSCodeNotify('workbench.action.previousEditor')<cr>")
  vim.keymap.set("n", "]b", "<Cmd>call VSCodeNotify('workbench.action.nextEditor')<cr>")

  vim.keymap.set("n", "gD", "<Cmd>call VSCodeNotify('editor.action.revealDefinitionAside')<CR>")
  vim.keymap.set("n", "gr", "<Cmd>call VSCodeNotify('editor.action.goToReferences')<CR>")

  vim.keymap.set("n", "[s", "<Cmd>call VSCodeNotify('editor.action.toggleStickyScroll')<CR>")
  vim.keymap.set("n", "=<", "<Cmd>call VSCodeNotify('editor.action.trimTrailingWhitespace')<CR>")
  vim.keymap.set("n", "gx", "<Cmd>call VSCodeNotify('editor.action.openLink')<CR>")

  vim.keymap.set("n", "<C-w>c", "<Cmd>call VSCodeNotify('workbench.action.closeActiveEditor')<CR>k")
  vim.keymap.set("n", "<C-w>C", "<cmd>call VSCodeNotify('workbench.action.closeEditorsToTheRight')<cr>")
  vim.keymap.set("n", "<C-w>o", "<Cmd>call VSCodeNotify('workbench.action.closeOtherEditors')<CR>k")

  vim.keymap.set("n", "<leader>1", "<Cmd>call VSCodeNotify('workbench.action.openEditorAtIndex1')<CR>")
  vim.keymap.set("n", "<leader>2", "<Cmd>call VSCodeNotify('workbench.action.openEditorAtIndex2')<CR>")
  vim.keymap.set("n", "<leader>3", "<cmd>call VSCodeNotify('workbench.action.openEditorAtIndex3')<cr>")
  vim.keymap.set("n", "<leader>4", "<cmd>call VSCodeNotify('workbench.action.openEditorAtIndex4')<cr>")
  vim.keymap.set("n", "<leader>5", "<cmd>call VSCodeNotify('workbench.action.openEditorAtIndex5')<cr>")
  vim.keymap.set("n", "<leader>6", "<cmd>call VSCodeNotify('workbench.action.openEditorAtIndex6')<cr>")

  vim.keymap.set("n", "<leader>-", "<Cmd>call VSCodeNotify('eslint.executeAutofix')<CR>")
  vim.keymap.set("n", "<leader>=", "<Cmd>call VSCodeNotify('editor.action.formatDocument')<CR>")
  vim.keymap.set(
    "v",
    "<leader>=",
    [[<Cmd>call VSCodeNotifyRangePos('editor.action.formatSelection', line("v"), line("."), col("v"), col("."), 1)<CR>]]
  )
  vim.keymap.set("n", "g=", "<Cmd>call VSCodeNotify('eslint.executeAutofix')<CR>")
  vim.keymap.set(
    "n",
    "<leader>f",
    "<Cmd>lua require('vscode').action('workbench.action.findInFiles', { args = { query = vim.fn.expand('<cword>') } })<CR>"
  )
  vim.keymap.set(
    "v",
    "<leader>f",
    [[<Cmd>lua require('vscode').action('workbench.action.findInFiles', { args = { query = vim.fn.getline(vim.fn.getpos("'<")[2],vim.fn.getpos("'>")[2]) } })<CR>]]
  )

  vim.keymap.set("n", "<C-Up>", "<Cmd>call VSCodeNotify('editor.action.insertCursorAbove')<cr>")
  vim.keymap.set("n", "<C-Down>", "<Cmd>call VSCodeNotify('editor.action.insertCursorBelow')<cr>")

  vim.keymap.set("n", "<C-Right>", "<Cmd>call VSCodeNotify('workbench.action.nextEditor')<cr>")
  vim.keymap.set("n", "<C-Left>", "<Cmd>call VSCodeNotify('workbench.action.previousEditor')<cr>")

  vim.keymap.set("n", "<leader>o", "<Cmd>call VSCodeNotify('workbench.action.openRecent')<CR>")
  vim.keymap.set("n", "<leader>R", "<cmd>call VSCodeNotify('workbench.action.reloadWindow')<cr>")

  vim.keymap.set("n", ",w", "<Cmd>call VSCodeNotify('workbench.action.files.save')<cr>")
  vim.keymap.set("n", ",W", "<Cmd>call VSCodeNotify('workbench.action.files.saveWithoutFormatting')<cr>")
  vim.keymap.set("n", "gh", "<Cmd>call VSCodeNotify('editor.action.showHover')<cr>")

  vim.keymap.set("n", "za", "<Cmd>call VSCodeNotify('editor.toggleFold')<CR>")
  vim.keymap.set("n", "zC", "<Cmd>call VSCodeNotify('editor.foldAll')<CR>")
  vim.keymap.set("n", "zO", "<Cmd>call VSCodeNotify('editor.unfoldAll')<CR>")
  vim.keymap.set("n", "zp", "<Cmd>call VSCodeNotify('editor.gotoParentFold')<CR>")

  vim.keymap.set("n", "<leader>te", "<cmd>call VSCodeNotify('vscode-harpoon.editEditors')<cr>")
  vim.keymap.set("n", "<leader>`", "<cmd>call VSCodeNotify('vscode-harpoon.addEditor')<cr>")
  vim.keymap.set("n", "<leader>tp", "<Cmd>call VSCodeNotify('vscode-harpoon.editorQuickPick')<CR>")
  vim.keymap.set("n", "<LocalLeader>1", "<cmd>call VSCodeNotify('vscode-harpoon.gotoEditor1')<cr>")
  vim.keymap.set("n", "<LocalLeader>2", "<cmd>call VSCodeNotify('vscode-harpoon.gotoEditor2')<cr>")
  vim.keymap.set("n", "<LocalLeader>3", "<cmd>call VSCodeNotify('vscode-harpoon.gotoEditor3')<cr>")
  vim.keymap.set("n", "<LocalLeader>4", "<cmd>call VSCodeNotify('vscode-harpoon.gotoEditor4')<cr>")
end
