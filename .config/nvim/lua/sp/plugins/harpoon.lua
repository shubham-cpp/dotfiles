return {
  "ThePrimeagen/harpoon",
  branch = "harpoon2",
  dependencies = { "nvim-lua/plenary.nvim" },
  keys = {
    {
      "<leader>a",
      function()
        require("harpoon"):list():append()
      end,
      desc = "Harpoon Append",
    },
    {
      "<C-S-P>",
      function()
        require("harpoon"):list():prev()
      end,
      desc = "Harpoon Prev",
    },
    {
      "<C-S-N>",
      function()
        require("harpoon"):list():next()
      end,
      desc = "Harpoon Next",
    },
    {
      "'1",
      function()
        require("harpoon"):list():select(1)
      end,
      desc = "Harpoon Select 1",
    },
    {
      "'2",
      function()
        require("harpoon"):list():select(2)
      end,
      desc = "Harpoon Select 2",
    },
    {
      "'3",
      function()
        require("harpoon"):list():select(3)
      end,
      desc = "Harpoon Select 3",
    },
    {
      "'4",
      function()
        require("harpoon"):list():select(4)
      end,
      desc = "Harpoon Select 4",
    },
    {
      "<C-e>",
      function()
        require("harpoon").ui:toggle_quick_menu(require("harpoon"):list())
      end,
      desc = "Harpoon Select 4",
    },
  },
  opts = {},
}
