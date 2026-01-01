-- init.lua
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- UI --
vim.opt.laststatus = 1
vim.opt.ruler = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.mouse = "a"
vim.opt.visualbell = false
vim.opt.errorbells = false
vim.opt.ttyfast = true
vim.opt.lazyredraw = false
vim.opt.termguicolors = true
vim.opt.background = "dark"
vim.opt.list = true
vim.opt.listchars = {
  trail = '·',
  tab = '⇥ '
}

-- Behavior --
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.wrap = false
vim.opt.scrolloff = 5
vim.opt.sidescrolloff = 8
vim.opt.timeout = true
vim.opt.timeoutlen = 1000
vim.opt.updatetime = 250
vim.opt.wildmenu = true
vim.opt.wildmode = { "longest:full", "full" }
vim.opt.backspace = { "indent", "eol", "start" }
vim.opt.hidden = true

-- Search --
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.incsearch = true
vim.opt.hlsearch = false
vim.opt.showmatch = true

-- Misc --
vim.opt.clipboard = "unnamedplus"
vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = true
vim.opt.undodir = vim.fn.stdpath("data").."/undodir"
-- Not needed?
-- vim.opt.syntax = "on"

-- Plugin manager --
local lazypath = vim.fn.stdpath("data").."/lazy/lazy.nvim"
local lazygitpath = "https://github.com/folke/lazy.nvim.git"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    lazygitpath, "--branch=stable", lazypath
  })
end
vim.opt.rtp:prepend(lazypath)

-- Plugins --
local plugins = {
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    opts = {
      highlight = { enable = true },
      indent = { enable = true },
    },
  },
  { "neovim/nvim-lspconfig" },
  {
    "mason-org/mason-lspconfig.nvim",
    dependencies = {
      {"mason-org/mason.nvim", opts = {}},
      "neovim/nvim-lspconfig",
    },
    opts = {},
  },
  {
    "saghen/blink.cmp",
    build = "cargo build --release",
    fuzzy = { implementation = "prefer_rust" },
    opts = {
      appearance = {
        use_nvim_cmp_as_default = false,
        nerd_font_variant = "mono",
      },
      completion = {
        accept = {
          auto_brackets = { enabled = true },
        },
        menu = {
          draw = { treesitter = { "lsp" } },
        },
        documentation = {
          auto_show = true,
          auto_show_delay_ms = 200,
        },
        ghost_text = { enabled = false },
      },
      sources = {
        default = { "lsp", "path", "buffer" },
      },
      cmdline = { enabled = false },
      keymap = { preset = "enter" },
    },
  },
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-fzf-native.nvim",
    },
    opts = {
      defaults = {
        border = false,
        layout_strategy = "bottom_pane",
        sorting_strategy = "ascending",
        layout_config = {
          height = 15,
          prompt_position = "bottom",
        },
        mappings = {
          i = { ["<C-g>"] = "close" },
          n = { ["<C-g>"] = "close" },
        },
        config = function()
          require("telescope").load_extension("fzf")
        end,
      }
    },
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    opts = {
      window = {
        width = 25,
        mappings = {
          [ "/" ] = "noop",
        },
      },
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      "nvim-tree/nvim-web-devicons",
    },
  },
  {
    "stevearc/oil.nvim", opts = {
      default_file_explorer = false,
      skip_confirm_for_simple_edits = true,
    },
  },
  { "tpope/vim-surround" },
  { "tpope/vim-commentary" },
  { "tpope/vim-fugitive" },
  { "lewis6991/gitsigns.nvim", opts = {} },
  -- { "tribela/vim-transparent" },
  { "mbbill/undotree" },
  { "windwp/nvim-autopairs", opts = {}},
  { "junegunn/vim-easy-align" },
}

vim.cmd("colorscheme purple")

-- Local overrides --
if vim.fn.isdirectory(vim.fn.stdpath("config").."/lua/extra") == 1 then
  table.insert(plugins, #plugins+1, require("extra"))
end

require("lazy").setup({
  spec = plugins,
  defaults = {
    lazy = false,
    version = false,
    news = false,
  },
  install = {  },
  checker = { enabled = false },
  performance = {
    rtp = {
      disabled_plugins = {
        "gzip",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      }
    }
  }
})

-- For some reason treesitter doesn't want to install stuff
-- unless you specify it separately like this?
require("nvim-treesitter").install {
  "python", "rust", "go", "c", "cpp",
  "javascript", "typescript", "lua", "bash",
}

-- Lua functions --
local join_existing_term = function()
  -- Use existing term when available
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) and
       vim.api.nvim_buf_get_option(buf, "buftype") == "terminal" then
       vim.cmd("botright 10split")
       vim.api.nvim_set_current_buf(buf)
       return
      end
  end
  vim.cmd("botright 10split | terminal")
end

-- Keymaps --
local opts = { noremap = true, silent = true }
local keymaps = {
  {"n", "<leader>h", ":noh<CR>"},

  -- Don't judge me
  { "n", "<Up>", "k" },
  { "n", "<Down>", "j" },
  { "n", "<Left>", "h" },
  { "n", "<Right>", "l" },

  { "v", "<S-Up>", "k" },
  { "v", "<S-Down>", "j" },

  { "n", "<leader>nn", ":tabnew<CR>" },
  { "n", "<C-M-l>", ":tabnext<CR>" },
  { "n", "<C-M-Right>", ":tabnext<CR>" },
  { "n", "<C-M-h>", ":tabprev<CR>" },
  { "n", "<C-M-Left>", ":tabprev<CR>" },
  { "n", "<leader>-", ":split<CR>" },
  { "n", "<leader>=", ":vsplit<CR>" },
  { "n", "<leader>wq", ":close<CR>" },
  { "n", "<leader>ww", ":only<CR>" },

  -- Seems redundant but arrows don't work
  -- otherwise, despite being bound to hjkl
  { "n", "<C-h>", "<C-w>h" },
  { "n", "<C-Left>", "<C-w>h" },
  { "n", "<C-j>", "<C-w>j" },
  { "n", "<C-Down>", "<C-w>j" },
  { "n", "<C-k>", "<C-w>k" },
  { "n", "<C-Up>", "<C-w>k" },
  { "n", "<C-l>", "<C-w>l" },
  { "n", "<C-Right>", "<C-w>l" },
  { "n", "<S-Left>", ":vertical resize +2<CR>" },
  { "n", "<S-Right>", ":vertical resize -2<CR>" },
  { "n", "<S-Up>", ":resize +2<CR>" },
  { "n", "<S-Down>", ":resize -2<CR>" },

  { "n", "<leader>ft", join_existing_term },
  { "t", "<Esc>", [[<C-\><C-n>]] },

  { "n", "gd", vim.lsp.buf.definition },
  { "n", "gy", vim.lsp.buf.type_definition },
  { "n", "gi", vim.lsp.buf.implementation },
  { "n", "gr", vim.lsp.buf.references },

  { "n", "<leader>cr", vim.lsp.buf.rename },
  { "n", "<leader>ca", vim.lsp.buf.code_action },
  { "n", "<leader>cf", vim.lsp.buf.format },

  { "n", "K", vim.lsp.buf.hover },
  { "n", "L", ":lua vim.diagnostic.open_float(nil, { focus = false })<CR>" },
  { "n", "[g", vim.diagnostic.goto_prev },
  { "n", "]g", vim.diagnostic.goto_next },

  { "n", "<leader>ff", ":Telescope find_files<CR>" },
  { "n", "<leader>fg", ":Telescope live_grep<CR>" },
  { "n", "<leader>bb", ":Telescope buffers<CR>" },
  { "n", "<leader>xx", ":Telescope diagnostics<CR>" },

  { "n", "<leader>gg", ":tabnew | Git | only<CR>" },
  { "n", "<leader>gs", ":Telescope git_status<CR>" },
  { "n", "<leader>gc", ":Git commit<CR>" },
  { "n", "<leader>gv", ":Gvdiffsplit<CR>" },

  { "n", "<leader>u", ":UndotreeToggle<CR>" },
  { "n", "<leader>e", ":Neotree toggle<CR>" },
  { "n", "<leader>d", ":Oil<CR>" },

  -- Shhhhhhhh
  { { "n", "v", "i" }, "<C-x><C-s>", "<Esc><cmd>w<CR>" },
  { { "n", "v", "i" }, "<C-x><C-c>", "<Esc><cmd>qa<CR>" },
  { { "n", "v", "i", "t", "c" }, "<C-g>", "<Esc>" },
  { { "n", "v", "i" }, "<M-x>", "<Esc><cmd>Telescope commands<CR>" },

  { "v", "<leader>a", ":EasyAlign " },
}
for _,v in ipairs(keymaps) do
  vim.keymap.set(v[1], v[2], v[3], opts)
end

-- Autocmd --
vim.api.nvim_create_autocmd("TermClose", {
  callback = function(a)
    vim.api.nvim_buf_set_var(a.buf, "should_kill", true)
  end,
})

vim.api.nvim_create_autocmd("BufWinLeave", {
  callback = function(a)
    if vim.api.nvim_buf_get_option(a.buf, "buftype") ~= "terminal" then return end
    local ok, should_kill = pcall(vim.api.nvim_buf_get_var, a.buf, "should_kill")
    if not (ok and should_kill) then return end
    vim.schedule(function()
      if vim.api.nvim_buf_is_valid(a.buf) then
        vim.api.nvim_buf_delete(a.buf, { force = true })
      end
    end)
  end,
})
