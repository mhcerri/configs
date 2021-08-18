-- General
vim.opt.cmdheight = 1
vim.opt.relativenumber = true
vim.opt.spell = true
lvim.format_on_save = true
lvim.lint_on_save = true
-- Define the colorscheme on the plugin config function below instead
--lvim.colorscheme = "spacegray"

-- Key bindings
-- Delete word with Ctrl-Backspace
lvim.keys.insert_mode["<A-BS>"] = "<C-w>"
lvim.keys.command_mode["<A-BS>"] = { "<C-w>", { silent=false, } }
-- Wrap paragraph with Alt-Q
lvim.keys.normal_mode["<A-q>"] = { "{gq}", { silent=true, } }
lvim.keys.insert_mode["<A-q>"] = { "<Esc>{gq}a", { silent=true, } }

lvim.keys.insert_mode["<C-h>B"] = { "<cmd>Telescope keymaps<cr>", { noremap=false, } }
lvim.keys.normal_mode["<C-h>B"] = { "<cmd>Telescope keymaps<cr>", { noremap=false, } }

-- "Emacs mode"
lvim.keys.insert_mode["<C-x><C-c>"] = { "<Esc><cmd>qa<cr>", { silent=true, noremap=false, } }
lvim.keys.normal_mode["<C-x><C-c>"] = { "<cmd>qa<cr>", { silent=true, noremap=false } }
lvim.keys.insert_mode["<C-x><C-s>"] = { "<Esc><cmd>w<cr>a", { silent=true, } }
lvim.keys.normal_mode["<C-x><C-s>"] = { "<cmd>w<cr>", { silent=true, } }

-- Leader binding via which-ke
lvim.leader = "space"
lvim.builtin.which_key.mappings["t"] = { "<cmd>terminal<cr>i", "Terminal" }
lvim.builtin.which_key.mappings["C"] = { "<cmd>Telescope colorscheme<cr>", "Color schemes" }
lvim.builtin.which_key.mappings["o"] = { "<cmd>Telescope oldfiles<cr>", "Old filesl" }
lvim.builtin.which_key.mappings["b"]["b"] = { "<cmd>Telescope buffers<cr>", "Find buffer" }
lvim.builtin.which_key.mappings["<Space>"] = { "<cmd>Telescope buffers<cr>", "Find buffer" }
lvim.builtin.which_key.mappings["H"] = {
  name = "+Help",
  h = { "<cmd>Telescope help_tags<cr>", "Help"},
  o = { "<cmd>Telescope vim_options<cr>", "Vim Options"},
  b = { "<cmd>Telescope keymaps<cr>", "Bindings"},
  m = { "<cmd>Telescope man_pages<cr>", "System Manual Pages"},
}

-- Move lines (using the existing key binding in lunarvim)
for _, mode in ipairs({ 'normal_mode', 'visual_mode', 'visual_block_mode'}) do
  for key, target in pairs({ ["<A-Up>"] = "<A-k>", ["<A-Down>"] = "<A-j>" }) do
    lvim.keys[mode][key] = lvim.keys[mode][target]
  end
end

-- Remap arrow keys to "fix" the command mode
-- lunarvim does not support "icommand_mode"
local cmd_opts = { noremap=true, expr=true, silent=false }
lvim.keys.command_mode["<Up>"]    = { [[pumvisible() ? "\<Left>"  : "\<Up>"   ]], cmd_opts }
lvim.keys.command_mode["<Down>"]  = { [[pumvisible() ? "\<Right>" : "\<Down>" ]], cmd_opts }
lvim.keys.command_mode["<Left>"]  = { [[pumvisible() ? "\<Up>"    : "\<Left>" ]], cmd_opts }
lvim.keys.command_mode["<Right>"] = { [[pumvisible() ? "\<Down>"  : "\<Right>"]], cmd_opts }

-- User Config for predefined plugins
-- After changing plugin config exit and reopen LunarVim, Run :PackerInstall :PackerCompile
lvim.builtin.dashboard.active = true
lvim.builtin.terminal.active = true
lvim.builtin.nvimtree.side = "left"
lvim.builtin.nvimtree.show_icons.git = 1
lvim.builtin.galaxyline.show_mode = true
lvim.builtin.terminal.direction = "horizontal"

-- if you don't want all the parsers change this to a table of the ones you want
lvim.builtin.treesitter.ensure_installed = "maintained"
lvim.builtin.treesitter.ignore_install = { "haskell" }
lvim.builtin.treesitter.highlight.enabled = true

-- LSP settings
-- you can set a custom on_attach function that will be used for all the language servers
-- See <https://github.com/neovim/nvim-lspconfig#keybindings-and-completion>
-- set a formatter if you want to override the default lsp one (if it exists)
-- lvim.lang.python.formatters = { { exe = "black", args = {} }, }
-- set an additional linter
-- lvim.lang.python.linters = { { exe = "flake8", args = {} }, }

-- Additional plugins
-- When adding or removing plugins, run :PackerSync
-- When updating a plugin config, run :PackerCompile
-- Note: config callbacks should rely on the global context of this file.
vim.cmd [[let g:doom_one_terminal_colors = v:true]]
lvim.plugins = {
  -- Colorscheme
  {
    "navarasu/onedark.nvim",
    config = function()
      vim.o.background = "dark"
      vim.g.disable_toggle_style = true
      vim.g.onedark_transparent_background = true
      -- dark, darker, cool, deep, warm, warmer
      vim.g.onedark_style = "warm"
      require('onedark').setup()
    end,
  },
  -- Smart home and end
  {
    "vim-scripts/Smart-Home-Key",
    config = function()
      local smart_home_opts = { silent=true, }
      vim.api.nvim_set_keymap("", "<Home>",
        ":SmartHomeKey<cr>", smart_home_opts)
      vim.api.nvim_set_keymap("i", "<Home>",
        "<C-o>:SmartHomeKey<cr>", smart_home_opts)
    end,
  },
  -- Expand region
  {
    "terryma/vim-expand-region",
    config = function()
      vim.api.nvim_set_keymap("", "<A-Space>",
        "<Plug>(expand_region_expand)", {})
    end,
  },
  -- Spellcheck
  {
    -- This is test.
    "lewis6991/spellsitter.nvim",
    config = function() require("spellsitter").setup() end,
  },
}

-- Autocommands (https://neovim.io/doc/user/autocmd.html)
lvim.autocommands.custom_groups = {
  -- Sane defaults for C similar to the kernel format rules
  { "BufWinEnter", "*.c", "setlocal ts=8 sw=8 noet" },
  { "BufWinEnter", "*.h", "setlocal ts=8 sw=8 noet" },
  -- Persist last cursor position
  {
    "BufReadPost", "*",
    [[if line("'\"") >= 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif]]
  },
}
