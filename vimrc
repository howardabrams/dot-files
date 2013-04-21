"------------------------------------------------
" Basic Settings
"------------------------------------------------
set ruler
set number
" code format
set expandtab
set autoindent
set shiftwidth=2
set softtabstop=2
set tabstop=2
" We're running Vim, not Vi!
" set nocompatible          
" close auto backup
set nobackup
set nowritebackup
" close swp
"set noswf
" Number of things to remember in history.
set history=256
" Highlight search result
set hlsearch
" Show matching brackets.
set showmatch
" No blinking
set novisualbell
" No noise.
set noerrorbells
" Always show status line.
set laststatus=2
" Backups & Files
" set backup                     " Enable creation of backup file.
" set backupdir=~/.vim/backups " Where backups will go.
" set directory=~/.vim/tmp     " Where temporary files will go.
" set nocompatible  " We don't want vi compatibility.
"------------------------------------------------
" Code Format
"------------------------------------------------
" Automatically detect file types
" Enable filetype-specific indenting and plugins
filetype on
filetype plugin indent on
" file format group
"augroup myfiletypes
"  " Clear old autocmds in group
"  autocmd!
"  " autoindent with two spaces, always expand tabs
"  autocmd FileType ruby,eruby,yaml set ai sw=2 sts=2 et
"  autocmd FileType css set ai sw=2 sts=2 et
"  autocmd FileType js set ai sw=4 sts=4 et
"  autocmd FileType javascript set tabstop=4 shiftwidth=4 expandtab
"augroup END
" syntax
syntax enable
syntax on
