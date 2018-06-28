" ~~~~~ PLUGIN NONSENSE ~~~~~
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'bling/vim-airline'
Plugin 'flazz/vim-colorschemes'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'scrooloose/nerdcommenter'
Plugin 'yangmillstheory/vim-snipe'
Plugin 'plasticboy/vim-markdown'
Plugin 'shougo/vimproc'
Plugin 'vim-syntastic/syntastic'

let mapleader = " "

" ~~~~~ Plugin settings ~~~~~

" ~~~~~ Vim Snipe ~~~~~
let g:snipe_jump_tokens = 'euhtn'

map <leader><leader>f <Plug>(snipe-f)

" ~~~~~ Syntastic ~~~~~

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" ~~~~~ Airline ~~~~~
let g:airline#extensions#tabline#enabled = 1
set t_Co=256

" ~~~~~~ Vundle ~~~~~

call vundle#end()
filetype plugin indent on


" ~~~~~ KEY MAPPINGS ~~~~~

" Just noping to force me to use Vim correctly
noremap <left> <nop>
noremap <right> <nop>
noremap <up> <nop>
noremap <down> <nop>
noremap <End> <nop>
noremap <Home> <nop>
noremap <Del> <nop>
noremap <PageUp> <nop>
noremap <PageDown> <nop>
inoremap <PageUp> <nop>
inoremap <PageDown> <nop>


" Actually useful

nnoremap j gj
nnoremap k gk

inoremap hl <ESC>
inoremap lh <ESC>

inoremap <C-l> <right>
inoremap <C-h> <left>
inoremap <C-k> <up>
inoremap <C-j> <down>

" Because I got used to the Emacs shortcut
noremap <C-X>o <C-W><C-W>



" ~~~~~ MODE SWITCHING ~~~~~
au CursorHoldI * stopinsert
" setting the update time for insert mode (switch back to normal)
au InsertEnter * let updaterestore=&updatetime | set updatetime=150000
au InsertLeave * let &updatetime=updaterestore

" ~~~~~ SEARCH ~~~~~ 
set incsearch
set ignorecase

" ~~~~~ INDENTATION ~~~~~
set expandtab
set tabstop=4
set shiftwidth=4
set autoindent
retab

" ~~~~~ COLORS/STYLE ~~~~~
colorscheme Tomorrow-Night-Eighties
syntax on
let html_no_rendering=1
set visualbell


" ~~~~~ BACKUP SETTINGS ~~~~~
set noswapfile

" ~~~~~ LINE NUMBERS ~~~~~
set number
highlight LineNr ctermfg=DarkGrey  
