--[[
A collection of color schemes for neovim as list of packer specs.
]]--
local packer_specs = {
  { "LunarVim/Colorschemes" },
  {
    "romgrk/doom-one.vim",
    disable = true,
    config = function()
      vim.o.background = "dark"
      vim.g.doom_one_terminal_colors = true
      --vim.cmd("autocmd ColorScheme * highlight Normal ctermbg=48,48,48 guibg=48,48,48")
      vim.cmd("colorscheme doom-one")
    end,
  },
  {
    "morhetz/gruvbox",
    disable = true,
    config = function()
    	-- Use 256 for compatibility with urxvt
    	-- https://github.com/morhetz/gruvbox/wiki/Configuration
    	vim.g.gruvbox_termcolors = 256
      vim.o.background = "dark"
      vim.cmd("colorscheme gruvbox")
    end,
  },
  {
    "navarasu/onedark.nvim",
    disable = true,
    config = function()
      vim.o.background = "dark"
      vim.g.onedark_style = "warm"
      vim.cmd("colorscheme onedark")
    end,
  },
  {
    "joshdick/onedark.vim",
    disable = true,
    config = function()
      vim.o.background = "dark"
      vim.g.onedark_termcolors = 256
      --vim.cmd("autocmd ColorScheme * highlight Normal ctermbg=48,48,48 guibg=48,48,48")
      vim.cmd("colorscheme onedark")
    end,
  },
  {
    "mhartington/oceanic-next",
    disable = true,
    config_disabled = function()
      vim.o.background = "dark"
      vim.o.termguicolors = false
      vim.cmd("colorscheme OceanicNext")
    end,
  },
  {
    "noah/vim256-color",
    disable = true,
    config = function()
      vim.o.background = "dark"
      vim.o.termguicolors = false
      -- Some options: mango, spacegray, babymate256, hybrid,
      -- Tomorrow-Night-Eighties
      vim.cmd("colorscheme babymate256")
    end,
  },
}

return packer_specs
