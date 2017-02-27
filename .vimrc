
" ~~~~~ PLUGIN NONSENSE ~~~~~
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'bling/vim-airline'
Plugin 'flazz/vim-colorschemes'

call vundle#end()
filetype plugin indent on

" airline
let g:airline#extensions#tabline#enabled = 1
set t_Co=256



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

" Fast searching
noremap <space> /

" Home row ESC
inoremap hu <Esc>
inoremap uh <Esc>

" Block comments
noremap ` :call ToggleCommentCurrentLine()<CR>

" Fast scrolling
noremap J 5j
noremap K 5k

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


" ~~~~~ BACKUP SETTINGS ~~~~~
set noswapfile

" ~~~~~ LINE NUMBERS ~~~~~
set number
highlight LineNr ctermfg=DarkGrey  






