;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.


(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     auto-completion
     emacs-lisp
     git
     markdown
     org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     spell-checking
     syntax-checking
     evil-snipe
     html
     scheme
     latex
     javascript
     c-c++
     haskell
     sql
     themes-megapack
     elm
     lua
     elixir
     clojure
     extra-langs
     python
     scala
     rust
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(doom-themes)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(smartparens evil-search-highlight-persist)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil

   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading 1
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-molokai
                         cyberpunk
                         tangotango
                         spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; General, global configuration
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (show-paren-mode 1)
  (global-set-key (kbd "C-c C-x") nil)
  (setq-default indent-tabs-mode nil)
  (setq scroll-conservatively most-positive-fixnum)
  (setq scroll-step 1)
  (scroll-bar-mode -1)
  (setq global-evil-search-highlight-persist nil)
  (setq flycheck-display-errors-delay 1)
  (setq flycheck-display-error-at-point-timer 1)
  (setq flycheck-pos-tip-timeout 30)
  (global-column-enforce-mode)


  (mapc (lambda (mode)
          (remove-hook mode 'enable-flyspell-mode))
        (list
         'elixir-mode-hook
         'haskell-mode-hook
         ))

  ;; Evil configuration
  (setq evil-escape-unordered-key-sequence 1)
  (setq evil-escape-key-sequence "uh")

  (evil-snipe-mode 1)
  (setq evil-snipe-scope 'visible)
  (setq evil-snipe-show-prompt nil)

  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "f") 'evil-snipe-f)
  (define-key evil-normal-state-map (kbd "F") 'evil-snipe-F)

  ;; org-mode configuration
  (setq org-agenda-files (list "~/Classes/Current/algebraic-codes/todo.org"
                               "~/Classes/Current/theory-of-comp/todo.org"
                               "~/Classes/Current/advanced-databases/todo.org"))

  (setq org-tag-alist '(("Homework" . ?h)
                        ("Exam" . ?e)
                        ("Project" . ?p)
                        ("TeachingAssistant" . ?t)
                        ("Internships" . ?i)
                        ("Personal" . ?m)))

  (evil-define-key 'normal org-mode-map
    (kbd "\\osch") 'org-schedule
    (kbd "\\oltd") 'org-agenda-list
    (kbd "\\odl") 'org-deadline)

  ;; mapping extensions to modes
  (setq web-mode-engines-alist '(("jinja" . "\\.html\\'")))

  ;; Specific mode configurations

  ;; web development
  (defun my-web-mode-hook ()
    "Hooks for web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook 'my-web-mode-hook)

  ;; java
  (defun my-java-hook ()
    "Fixes the weird indentation."
    (setq c-basic-offset 4
          tab-width 4
          indent-tabs-mode t)
    (flycheck-mode +1))
  (add-hook 'java-mode-hook 'my-java-hook)

  ;; scheme
  (setq scheme-program-name "petite")
  (evil-leader/set-key "irs" 'run-scheme)

  ;; python
  (setq python-shell-completion-native-enable nil)
  (evil-leader/set-key "irp" 'run-python)

  ;; haskell
  (evil-leader/set-key "irh" 'run-haskell)

  ;; Prolog
  (add-to-list 'auto-mode-alist '("\\.\\(pl\\|pro\\|lgt\\)" . prolog-mode))

  )




;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f782ed87369a7d568cee28d14922aa6d639f49dd676124d817dd82c8208985d0" "066d4710e40eeb85aa7c72afa6c23d09dee4795bf4e450d4869324e917b5f64d" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "1012cf33e0152751078e9529a915da52ec742dabf22143530e86451ae8378c1a" "b825687675ea2644d1c017f246077cdd725d4326a1c11d84871308573d019f67" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "86a731bda96ed5ed69980b4cbafe45614ec3c288da3b773e4585101e7ece40d2" "e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "fbcdb6b7890d0ec1708fa21ab08eb0cc16a8b7611bb6517b722eba3891dfc9dd" "e8586a76a96fd322ccb644ca0c3a1e4f4ca071ccfdb0f19bef90c4040d5d3841" "304c03c9cfcd368b4ab0832357788cd48513fe1bd89b9e531dd47886a83405a1" "ac5584b12254623419499c3a7a5388031a29be85a15fdef9b94df2292d3e2cbb" "d606ac41cdd7054841941455c0151c54f8bff7e4e050255dbd4ae4d60ab640c1" "cadc97db0173a0d0bfc40473cab4da462af0ba8d60befd0a4879b582bcbc092d" "9e147cee63e1a2a6b16021e0645bc66c633c42b849e78b8e295df4b7fe55c56a" "9cb6358979981949d1ae9da907a5d38fb6cde1776e8956a1db150925f2dad6c1" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (quack scheme-complete web-mode powerline-evil evil-snipe badwolf-theme markdown-preview-eww markdown-mode+ markdown-mode elpy airline-themes ample-theme colonoscopy-theme foggy-night-theme flycheck evil-escape evil-magit metalheart-theme magit tango-2-theme tango-plus-theme tangotango-theme tide key-chord git evil)))
 '(py-force-py-shell-name-p t)
 '(py-shell-name "python2")
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "8453c6ba2504874309bdfcda0a69236814cefb860a528eb978b5489422cb1791" "945fe66fbc30a7cbe0ed3e970195a7ee79ee34f49a86bc96d02662ab449b8134" "38e64ea9b3a5e512ae9547063ee491c20bd717fe59d9c12219a0b1050b439cdd" "f782ed87369a7d568cee28d14922aa6d639f49dd676124d817dd82c8208985d0" "066d4710e40eeb85aa7c72afa6c23d09dee4795bf4e450d4869324e917b5f64d" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "1012cf33e0152751078e9529a915da52ec742dabf22143530e86451ae8378c1a" "b825687675ea2644d1c017f246077cdd725d4326a1c11d84871308573d019f67" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "86a731bda96ed5ed69980b4cbafe45614ec3c288da3b773e4585101e7ece40d2" "e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "fbcdb6b7890d0ec1708fa21ab08eb0cc16a8b7611bb6517b722eba3891dfc9dd" "e8586a76a96fd322ccb644ca0c3a1e4f4ca071ccfdb0f19bef90c4040d5d3841" "304c03c9cfcd368b4ab0832357788cd48513fe1bd89b9e531dd47886a83405a1" "ac5584b12254623419499c3a7a5388031a29be85a15fdef9b94df2292d3e2cbb" "d606ac41cdd7054841941455c0151c54f8bff7e4e050255dbd4ae4d60ab640c1" "cadc97db0173a0d0bfc40473cab4da462af0ba8d60befd0a4879b582bcbc092d" "9e147cee63e1a2a6b16021e0645bc66c633c42b849e78b8e295df4b7fe55c56a" "9cb6358979981949d1ae9da907a5d38fb6cde1776e8956a1db150925f2dad6c1" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (toml-mode racer flycheck-rust cargo rust-mode noflet ensime sbt-mode scala-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic wolfram-mode thrift stan-mode scad-mode qml-mode matlab-mode julia-mode arduino-mode clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider queue clojure-mode ob-elixir flycheck-mix alchemist elixir-mode lua-mode zonokai-theme zenburn-theme zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spacegray-theme sourcerer-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flycheck-elm flatui-theme flatland-theme firebelly-theme farmhouse-theme espresso-theme elm-mode dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme apropospriate-theme anti-zenburn-theme ample-zen-theme alect-themes afternoon-theme quack scheme-complete web-mode powerline-evil evil-snipe badwolf-theme markdown-preview-eww markdown-mode+ markdown-mode elpy airline-themes ample-theme colonoscopy-theme foggy-night-theme flycheck evil-escape evil-magit metalheart-theme magit tango-2-theme tango-plus-theme tangotango-theme tide key-chord git evil)))
 '(py-force-py-shell-name-p t)
 '(py-shell-name "python2")
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
)
