vim9script

g:mapleader = " "
g:maplocalleader = " "
set timeout timeoutlen=2000

var vim_plug_path = expand('~/.vim/autoload/plug.vim')
if !filereadable(vim_plug_path)
  echo "Installing plug"
  silent execute '!curl -fLo ' .. vim_plug_path .. ' --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

&ruler = v:true
&number = v:true              # Line numbers
&relativenumber = v:true      # Relative line numbers
&tabstop = 2                  # Tab width
&shiftwidth = 2               # Indent width
&expandtab = v:true           # Use spaces instead of tabs
&softtabstop = 2
&smartindent = v:true         # Smart indentation
&wrap = v:false               # Don't wrap lines
&scrolloff = 8                # Keep 8 lines visible when scrolling
&sidescrolloff = 8            # Keep 8 columns visible when scrolling
&syntax = 'on'
&mouse = 'a'                  # Enable mouse in all modes
&ignorecase = v:true          # Ignore case in search
&smartcase = v:true           # Override ignorecase if search contains uppercase
&incsearch = v:true           # Incremental search
&hlsearch = v:true            # Highlight search results
&showmatch = v:true           # Show matching brackets
&wildmenu = v:true            # Enhanced command line completion
&wildmode = 'longest:full,full'
&backspace = 'indent,eol,start'  # Sensible backspace
&hidden = v:true              # Allow switching buffers without saving
&encoding = 'utf-8'           # UTF-8 encoding
&fileencoding = 'utf-8'
&visualbell = v:false        # Visual bell instead of beeping
&errorbells = v:false

var undo_dir = expand('~/.vim/undodir')
if !isdirectory(undo_dir)
  call mkdir(undo_dir, 'p')
endif
&swapfile = v:false
&backup = v:false
&undodir = undo_dir
&undofile = v:true            # Persistent undo

# Fix redraw in Windows Terminal
&ttyfast = v:true
&lazyredraw = v:false
&termguicolors = v:true

# WSL clipboard
if executable('win32yank.exe')
  augroup WSLClipboard
    autocmd!
    autocmd TextYankPost * if v:event.operator ==# 'y' | call system('win32yank.exe -i --crlf', @") | endif
  augroup END
elseif executable('clip.exe')
  augroup WSLClipboard
    autocmd!
    autocmd TextYankPost * if v:event.operator ==# 'y' | call system('clip.exe', @") | endif
  augroup END
endif

# Fix Home/End
map <esc>OH <Home>
cmap <esc>OH <Home>
imap <esc>OH <Home>
map <esc>OF <End>
cmap <esc>OF <End>
imap <esc>OF <End>

nnoremap <leader>h :noh<CR>

nnoremap <Up> k
nnoremap <Down> j
nnoremap <Left> h
nnoremap <Right> l
vnoremap <Up> k
vnoremap <Down> j
vnoremap <Left> h
vnoremap <Right> l
nnoremap <leader>nt :tabnew<CR>
nnoremap <leader>nn :tabnext<CR>
nnoremap <leader>nb :tabprevious<CR>
nnoremap <leader>- :split<CR>
nnoremap <leader>= :vsplit<CR>
nnoremap <leader>wq :close<CR>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <C-Left> <C-w>h
nnoremap <C-Down> <C-w>j
nnoremap <C-Up> <C-w>k
nnoremap <C-Right> <C-w>l
nnoremap <S-Left> :vertical resize -2<CR>
nnoremap <S-Right> :vertical resize +2<CR>
nnoremap <S-Up> :resize +2<CR>
nnoremap <S-Down> :resize -2<CR>
nnoremap <leader>ft :botright terminal<CR>

# Esc to get back to normal mode in terminal
tnoremap <Esc> <C-\><C-n>

# Plugins
plug#begin('~/.vim/plugged')
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
  Plug 'preservim/nerdtree'
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-fugitive'
  Plug 'airblade/vim-gitgutter'
  Plug 'tribela/vim-transparent'
  Plug 'axvr/photon.vim'
plug#end()

g:coc_global_extensions = [
  'coc-clangd',
  'coc-rust-analyzer',
  'coc-go',
  'coc-lua',
  'coc-sh',
  'coc-html',
  'coc-pyright',
  'coc-tsserver',
  'coc-snippets',
  'coc-prettier',
  'coc-pairs',
  'coc-lists',
  'coc-highlight',
  'coc-eslint',
  'coc-yaml',
  'coc-markdownlint',
  'coc-json',
  'coc-docker',
  'coc-css'
]

# LSP keybindings
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <leader>cr <Plug>(coc-rename)
nmap <leader>ca <Plug>(coc-codeaction)
nmap <leader>cf <Plug>(coc-fix-current)
nnoremap <silent> K :call ShowDocumentation()<CR>

# Show hover documentation
def ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    feedkeys('K', 'in')
  endif
enddef

autocmd CursorHold * silent call CocActionAsync('highlight')
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)
g:coc_auto_complete_enabled = v:true

def ToggleCocCompletion()
  if g:coc_auto_complete_enabled
    b:coc_suggest_disable = 1
    g:coc_auto_complete_enabled = v:false
    echo "CoC completion disabled"
  else
    if exists('b:coc_suggest_disable')
      unlet b:coc_suggest_disable
    endif
    g:coc_auto_complete_enabled = v:true
    echo "CoC completion enabled"
  endif
enddef

nnoremap <leader>ac <ScriptCmd>ToggleCocCompletion()<CR>

highlight CocInlayHint ctermfg=240 guifg=#666666 ctermbg=NONE guibg=NONE
highlight CocInlayHintParameter ctermfg=240 guifg=#666666 ctermbg=NONE guibg=NONE
highlight CocInlayHintType ctermfg=240 guifg=#666666 ctermbg=NONE guibg=NONE

nnoremap <leader>ff :CocList files<CR>
nnoremap <leader>fg :CocList grep<CR>
nnoremap <leader>bb :CocList buffers<CR>
nnoremap <leader>xx :CocList diagnostics<CR>

inoremap <silent><expr> <CR> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
inoremap <silent><expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <silent><expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

nnoremap <leader>e :NERDTreeToggle<CR>
g:NERDTreeMinimalUI = 1

colorscheme photon
&background = 'dark'
