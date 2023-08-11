;;; .emacs --- Summary
;; Emacs configuration
;;
;;; Commentary:
;;
;; Average startup with emacs26 -nw: 0.34s

;;; Code:

;; Suppress welcome screen
(setq inhibit-startup-screen t)

;; Tune garbage collector during startup
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold (* 2 1000 1000))))

;; Disable special file name handlers, such as tramp, during the startup.
(defvar ~file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
  (lambda () (setq file-name-handler-alist ~file-name-handler-alist)))

;; Show how long it took to startup
(add-hook
 'emacs-startup-hook
 (lambda ()
   (let ((secs (float-time
		(time-subtract after-init-time before-init-time))))
     (message "Emacs ready in %s with %d garbage collections."
	      (format "%.2f seconds" secs)
	      gcs-done))))

;; Hide tool and menu bars and use reasonable font size
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
(menu-bar-mode 0)
;; Handle default font via ~/.Xresources.
(setq inhibit-x-resources nil)

;; Sane defaults
(show-paren-mode 1)                     ; Highlight matching parenthesis
(column-number-mode 1)                  ; Show column number with line number
(setq vc-follow-symlinks t)             ; Follow symlinks under version control
(prefer-coding-system 'utf-8)           ; Use utf8 by default
(fset 'yes-or-no-p 'y-or-n-p)           ; Lazy prompt
(savehist-mode)                         ; Persistent history
(xterm-mouse-mode)                      ; Support mouse inside terminal
(setq scroll-step 1)                    ; Scroll one line at a time
(setq scroll-margin 5)                  ; Show N lines at the edge when scrolling
(setq                                   ; Scroll one line at a time
 mouse-wheel-scroll-amount
 '(1 ((shift) . 1)))
(setq                                   ; Don't accelerate scrolling
 mouse-wheel-progressive-speed nil)
(setq                                   ; Scroll window under mouse
 mouse-wheel-follow-mouse 't)
(put 'narrow-to-region 'disabled nil)   ; Allow narrowing a region

;; No customization in this file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Remember cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; Backups at .saves folder in the current folder
(setq
 ;; Don't mess with symlinks
 backup-by-copying t
 ;; Don't leave garbage around the entire file system
 backup-directory-alist '(("." . "~/.emacs.d/saves"))
 ;; Remove old versions
 delete-old-versions t
 ;; Number of files to keep
 kept-new-versions 6
 kept-old-versions 2
 ;; Do not create #autosave# files
 auto-save-default nil
 ;; Use backups with version numbers
 version-control t)

;; Default indentation settings
(setq-default indent-tabs-mode t)
(setq tab-width 8)
(defvaralias 'c-indent 'tab-width)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'sh-indentation 'tab-width)
(defvaralias 'sh-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Custom initial scratch message
(defvar initial-scratch-file "~/.emacs.d/scratch")
(if (file-exists-p initial-scratch-file)
    (setq initial-scratch-message
	  (with-temp-buffer
	    (insert-file-contents initial-scratch-file)
	    (buffer-string))))

;; Sane undo
(fset 'undo-auto-amalgamate 'ignore)

;; Shortcuts

;; Kills emacs server
(global-set-key (kbd "C-x Q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-x C") 'kill-buffer-and-window)

;; Cycle between the last two buffers
(defun ~switch-to-other-buffer ()
  "Switch to the last buffer visited but ignore ibuffer."
  (interactive)
  (switch-to-buffer (other-buffer (get-buffer "*Ibuffer*")) nil t))
(global-set-key (kbd "M-o")  '~switch-to-other-buffer)

;; Use text-mode by default for files.
(defun ~text-mode-auto-detected-p ()
  "Use `text-mode' as the default mode for any file instead of \
`fundamental-mode'."
  (if (equal major-mode 'fundamental-mode)
      (not (null (buffer-file-name)))))
(add-to-list 'magic-fallback-mode-alist
	     '(~text-mode-auto-detected-p . text-mode))

;; Kill line backwards
;; Use C-u u to delete from cursor to beginning of line, similarly
;; to C-u used by vim and bash/readline.
(defun ~backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key (kbd "C-c u") '~backward-kill-line)
(global-set-key (kbd "C-c C-u") '~backward-kill-line)

;; Package manager
(require 'package)
(setq package-enable-at-startup nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/") ; Assume ssl
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("gnu"          . "https://elpa.gnu.org/packages/")))
(if (not (bound-and-true-p ~package-initialized))
    (package-initialize))

;; Auto install mechanism
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(setq native-comp-async-report-warnings-errors nil)

;; Install package by default
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Import keys
(use-package gnu-elpa-keyring-update)

;; Make possible to install packages from git
;; It increases init time, should I keep it?
(setq quelpa-update-melpa-p nil)
(setq quelpa-checkout-melpa-p nil)
(setq quelpa-use-package-inhibit-loading-quelpa t)
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)
;; To make possible to install via quelpa when using
;; use-package-always-ensure
(quelpa-use-package-activate-advice)

;; Add support for extended bindings with use-package
(use-package general)

;; Hide modes from status bar (used with use-package)
(use-package diminish
  :config
  ;; Only diminish built-in modes here
  (dolist (m '(eldoc-mode auto-revert-mode))
    (diminish m)))

(use-package delight)

;; Async library
(use-package s)

;; Async
(use-package async
  :config
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

;; Run async external commands with status on the message line
(use-package bpr
  :commands (bpr-process-directory
             bpr-show-progress
             bpr-close-after-success
             bpr-spawn))

;; Function decorator library
(use-package noflet
  :defer t)

;; Fix env (important for go-mode with emacsclient)
(use-package exec-path-from-shell
  :if (daemonp)
  :config
  (dolist (var '("GOPATH" "GOROOT" "NVM_BIN" "PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package simple
  :ensure nil ; builtin
  :config
  ;;(add-hook 'text-mode-hook 'turn-on-auto-fill)) ; Use M-q instead
  (setq backward-delete-char-untabify-method 'hungry))

;; Move lines up and down with M-<up> and M-<down>
(use-package move-text
  :bind
  (;; Up
   ("M-<up>"     . move-text-up)
   ("M-p"        . move-text-up)
   ("ESC <up>"   . move-text-up)
   ("ESC p"      . move-text-up)
   ;; Down
   ("M-<down>"   . move-text-down)
   ("M-n"        . move-text-down)
   ("ESC <down>" . move-text-down)
   ("ESC n"      . move-text-down)))

;; Keep track of recent files
(use-package recentf
  ;; builtin
  :demand
  ;; Note: Initially I was using :defer .1 to reduce startup time, but
  ;; that seems to cause issues with Evil :wq command, when a file is
  ;; changed under a git repository with git-gutter loaded.
  :config
  (setq recentf-max-saved-items 100)
  (let ((inhibit-message t))
    (recentf-mode 1)))

;; Show undo tree with "C-x u"
(use-package undo-tree
  :diminish
  :defer 1)

;; Use simpleclip-copy, simpleclip-paste and simpleclip-cut to
;; interact with the system clipboard.
(use-package simpleclip
  :bind
  (("C-c y"  . copy-to-clipboard)
   ("C-c p"  . paste-from-clipboard)
   ;; Mimic terminal copy and paste in GUI:
   ("C-S-c"  . copy-to-clipboard)
   ("C-S-v"  . paste-from-clipboard))
  :commands (copy-to-clipboard
	     paste-from-clipboard
	     cut-to-clipboard
	     simpleclip-set-contents)
  :init
  (setq browse-url-browser-function
	(lambda (url &rest args)
	  (simpleclip-set-contents url)))
  :config
  (require 'xclip)
  ;; Add support for Wayland
  (when (string= (getenv "XDG_SESSION_TYPE") "wayland")
    ;; credit: yorickvP on Github
    (setq wl-copy-process nil)
    (defun wl-copy (text)
      "Copy TEXT to the wayland clipboard."
      (setq wl-copy-process (make-process :name "wl-copy"
					  :buffer nil
					  :command '("wl-copy" "-f" "-n")
					  :connection-type 'pipe))
      (process-send-string wl-copy-process text)
      (process-send-eof wl-copy-process))

    (defun wl-paste ()
      "Get text from wayland clipboard."
      (if (and wl-copy-process (process-live-p wl-copy-process))
	  nil ; should return nil if we're the current paste owner
	(shell-command-to-string "wl-paste -n | tr -d \r")))
    (setq interprogram-cut-function 'wl-copy)
    (setq interprogram-paste-function 'wl-paste)

    (defun simpleclip-set-contents (str-val)
      "Set the contents of the system clipboard to STR-VAL."
      (wl-copy str-val))

    (defun simpleclip-get-contents ()
      "Return the contents of the system clipboard as a string."
      (wl-paste)))
  (defalias 'copy-to-clipboard 'simpleclip-copy)
  (defalias 'paste-from-clipboard 'simpleclip-paste)
  (defalias 'cut-to-clipboard 'simpleclip-cut)
  (simpleclip-mode 1))

;; xclip is necessary for simpleclip to work
(use-package xclip
  :defer t
  :config
  (xclip-mode 1))

(use-package sr-speedbar)

;; Highlight current line
(use-package hl-line
  :hook (after-change-major-mode . ~enable-hl-line-mode)
  :config
  (defun ~enable-hl-line-mode ()
    "Enable hl-line-mode skipping some modes."
    (if (and (not (derived-mode-p 'term-mode))
             (not (bound-and-true-p rainbow-mode)))
	(hl-line-mode 1))))

;; Show hex colors
(use-package rainbow-mode
  :quelpa (rainbow-mode :fetcher github :repo "amosbird/rainbow-mode")
  :hook ((html-mode css-mode js-mode conf-mode) . rainbow-mode)
  :init
  (defun ~rainbow-mode()
    (rainbow-mode)
    ;; Disable line highlight when rainbow is enabled
    (hl-line-mode -1)))

;; zerodark-theme
(use-package zerodark-theme
  :custom-face
  (org-tag ((t (:background nil :inherit shadow))))
  (org-special-keyword ((t (:inherit shadow))))
  (org-block ((t (:background nil :inherit shadow))))
  (org-block ((t (:background "#20242b"))))
  (org-block-begin-line ((t (:foreground nil :inherit shadow))))
  (org-block-end-line ((t (:foreground nil :inherit shadow))))
  :init
  ;; Fix issue in zerodark when using "emacs --daemon" and true color
  (advice-add 'true-color-p :filter-return (lambda (ret) t))
  :config
  ;; Not sure why it's not possible to set the org block background via
  ;; :custom-face. Use this dirty workaround in the meantime. Might be
  ;; worth it trying to remove it when upgrading emacs or org-mode.
  (add-hook
   'org-mode-hook
   (lambda ()
     (set-face-attribute 'org-block nil :background "#20242b")
     (set-face-attribute 'org-block-begin-line nil :background "#20242b")
     (set-face-attribute 'org-block-end-line nil :background "#20242b")))
  ;; Enable theme
  (setq frame-background-mode 'dark)
  (setq zerodark-use-paddings-in-mode-line nil)
  (load-theme 'zerodark t)
  (set-face-attribute 'mode-line-inactive nil :height 1.0))

;; Mode line
(use-package doom-modeline
  :ensure t
  :custom-face
  (doom-modeline-evil-emacs-state ((t (:background "purple" :bold t))))
  (doom-modeline-evil-normal-state ((t (:background "red" :bold t))))
  (doom-modeline-evil-insert-state ((t (:background "darkgreen" :bold t))))
  (doom-modeline-evil-replace-state ((t (:background "orange" :bold t))))
  (doom-modeline-evil-visual-state ((t (:background "orange" :bold t))))
  (doom-modeline-evil-motion-state ((t (:background "blue" :bold t))))
  (doom-modeline-evil-operator-state ((t (:background "darkblue" :bold t))))
  :init
  (setq evil-emacs-state-tag " EMACS "
	evil-normal-state-tag " NORMAL "
	evil-insert-state-tag " INSERT "
	evil-replace-state-tag " REPLACE "
	evil-visual-state-tag " VISUAL "
	evil-motion-state-tag " MOTION "
	evil-operator-state-tag " OPERATOR ")
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-mu4e t)
  (doom-modeline-mode 1))

;; Extensible vi layer
(use-package evil
  :demand ;; ":defer .1" causes some issues when other packages rely on it.
  :bind
  (; Avoid problems with eval-expression
   ("M-:"     . evil-ex)
   ("M-ESC :" . evil-ex)
   (:map evil-window-map
	 ("<left>"  . evil-window-left)
	 ("<down>"  . evil-window-down)
	 ("<up>"    . evil-window-up)
	 ("<right>" . evil-window-right))
   (:map evil-normal-state-map
	 ("g T"     . previous-buffer)
	 ("g t"     . next-buffer)
	 ("C-e"     . move-end-of-line))
   (:map evil-insert-state-map
	 ;; cua-mode-ish for insert
         ;; WARNING: it doesn't work properly with multi cursors
         ;;          or with visual block
	 ;;("C-S-c"   . kill-ring-save)
	 ;;("C-S-x"   . kill-region)
	 ;;("C-S-v"   . yank)
	 ;;("C-S-p"   . yank-pop)
	 ("C-z"     . undo-tree-undo)))
  :init
  ;; Use default emacs bindings for insert vim mode
  (setq-default evil-disable-insert-state-bindings t)
  ;; Search for symbols not words
  (setq evil-symbol-word-search t)
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-search-module 'evil-search)
  ;; Use evil mode eveywhere
  (setq evil-emacs-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)
  ;; Make TAB work with evil-org
  (setq evil-want-C-i-jump nil)
  ;; Required for evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;; No crazy undos
  (setq evil-want-fine-undo t)
  ;; Esc hook support
  (defvar ~evil-esc-hook '(t)
    "A hook run after ESC is pressed in normal mode.")
  (defun ~evil-attach-escape-hook ()
    "Run the `~evil-esc-hook'."
    (cond
     ((minibuffer-window-active-p (minibuffer-window))
      ;; Quit the minibuffer if open
      (abort-recursive-edit))
     ((evil-ex-hl-active-p 'evil-ex-search)
      ;; Disable ex search buffer highlights
      (evil-ex-nohighlight))
     (t
      ;; Run all escape hooks. If any returns non-nil, then stop there.
      (run-hook-with-args-until-success '~evil-esc-hook))))
  (advice-add #'evil-force-normal-state
	      :after #'~evil-attach-escape-hook)
  :config
  ;; Vim leader bindings.
  ;; Note: it looks like that evil-leader directly loads evil. Because
  ;; of that, there's no way of using the use-package :after property
  ;; to force evil-leader to load after evil:init and before
  ;; evil:config. The alternative would be to move everything in
  ;; evil:init to evil-leader:init.
  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-key
      ;; Leader twice
      evil-leader/leader 'consult-buffer
      "f"  'find-file
      "r"  'recentf
      "p"  'projectile-find-file
      "P"  'projectile-switch-project
      "b"  'list-buffers
      "c"  'company-complete
      "qq" 'save-buffers-kill-terminal
      "qQ" 'save-buffers-kill-emacs
      "k"  'kill-this-buffer
      "w"  'evil-window-delete
      "//" 'evilnc-comment-or-uncomment-lines
      "/c" 'evilnc-copy-and-comment-lines
      "/p" 'evilnc-comment-or-uncomment-paragraphs
      ))
  ;; When deferring evil (and thus evil-collection), some fixes will fail
  ;; to be applied to special modes that are loaded right after emacs is
  ;; started. One example is git-rebase-mode, that is invoked when emacs is
  ;; set as the editor for git.
  ;; Force the insert start for those mode as an workaroud:
  (dolist (mode '(git-rebase-mode dired-mode))
    (evil-set-initial-state mode 'insert))
  ;; Fix emacs
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :diminish (evil-collection-unimpaired-mode)
  :config
  (evil-collection-init))

;; evilnc-* function to comment and uncomment lines
(use-package evil-nerd-commenter)

;; Show information about searches
(use-package evil-anzu
  :defer 1)

;; Improved %
(use-package evil-matchit
  :defer t ; Supports autoload
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; Show evil marks
(use-package evil-visual-mark-mode
  :defer 1
  :after evil
  :config
  (evil-visual-mark-mode 1)
  (advice-add 'evil-delete-marks :after
              (lambda (&rest args)
                (evil-visual-mark-render))))

;; Multiple cursors
(use-package evil-mc
  :diminish
  :after (evil which-key)
  :bind
  ((:map evil-normal-state-map
	 ("grm" . evil-mc-make-all-cursors)
	 ("gru" . evil-mc-undo-all-cursors)
	 ("grs" . evil-mc-pause-cursors)
	 ("grr" . evil-mc-resume-cursors)
	 ("grf" . evil-mc-make-and-goto-first-cursor)
	 ("grl" . evil-mc-make-and-goto-last-cursor)
	 ("grh" . evil-mc-make-cursor-here)
	 ("grj" . evil-mc-make-cursor-move-next-line)
	 ("grk" . evil-mc-make-cursor-move-prev-line)
	 ("grN" . evil-mc-skip-and-goto-next-cursor)
	 ("grP" . evil-mc-skip-and-goto-prev-cursor)
	 ("grn" . evil-mc-skip-and-goto-next-match)
	 ("grp" . evil-mc-skip-and-goto-prev-match)))
  :init
  (which-key-add-key-based-replacements "gr" "evil multi-cursor")
  ;; To avoid conflicts with other packages, disable the internal map.
  (setq evil-mc-key-map nil)
  :config
  (setq-default evil-mc-enable-bar-cursor nil)
  ;; Use a proper face for cursors
  (setq evil-mc-cursor-current-face '(:reverse-video t))
  ;; Non standard commands that need to be hinted:
  (setq-default evil-mc-custom-known-commands
		'((crux-move-beginning-of-line . ((:default . evil-mc-execute-default-call-with-count)))))
  ;; Enable globally to make vim-like binding (ie gr*) available
  (global-evil-mc-mode 1))

;; Better Home and C-k
(use-package crux
  :bind
  (([remap move-beginning-of-line] . crux-move-beginning-of-line)
   ([remap evil-beginning-of-line] . crux-move-beginning-of-line)
   ("C-k"                          . crux-smart-kill-line)))

;; PCRE regular expression style
;; Use `pcre-mode' to enable it.
(use-package pcre2el
  :commands (pcre-mode))

;; Add numbers to lines
(use-package display-line-numbers
  :demand
  :hook
  (((text-mode prog-mode conf-mode) . ~enable-line-number))
  :init
  (setq display-line-numbers-type 'relative)
  :config
  (defun ~enable-line-number () (display-line-numbers-mode 1))
  (defun ~disable-line-number () (display-line-numbers-mode -1))
  (defun ~display-line-numbers-toggle-relative ()
    "Toggle relative line numbers"
    (interactive)
    (if (equal display-line-numbers 'relative)
        (setq display-line-numbers 'absolute)
      (setq display-line-numbers 'relative))))

;; Highlight trailing spaces
(use-package whitespace
  :diminish
  :after (flyspell)
  :hook (((text-mode prog-mode) . whitespace-mode)
	 ((org-mode) . (lambda () (whitespace-mode -1))))
  :config
  ;; Use a face that plays nicer with the cursor.
  (custom-set-faces
   '(whitespace-line ((t (:foreground nil :underline t)))))
  (setq whitespace-line-column 78)
  (setq whitespace-style
	'(face         ;
	  trailing     ; Trailing blanks
	  lines-tail   ; Lines with columns beyond whitespace-line-column
	  tabs)        ; HIghlight tabs
	)
  (modify-face whitespace-tab "#363e4d" nil nil nil nil t))

;; Remove trailing white spaces
(use-package ws-butler
  :diminish
  :hook ((text-mode prog-mode) . ws-butler-mode))

;; Smartly add and ignore closing marks
(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :commands (smartparens-mode))

;; Usage: M-x ialign RET
(use-package ialign
  :commands (ialign))

;; ediff options
(use-package ediff
  :defer t ; Supports autoload
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options ""))

(use-package dired
  :defer t
  :ensure nil ; builtin
  :init
  ;; Enable dired-find-alternate-file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package ranger
  :after (dired)
  :init
  (setq ranger-show-hidden 'hidden)
  (setq ranger-cleanup-on-disable nil)
  (setq ranger-cleanup-eagerly nil)
  (setq ranger-override-dired t))

(use-package ibuffer
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :commands (ibuffer)
  :init
  (defalias 'list-buffers 'ibuffer)
  :config
  ;; Make ibuffer default. This way it's possible to switch buffers with "C-x C-b".
  (setq ibuffer-expert t) ; Do not ask to delete buffer
  (setq-default ibuffer-show-empty-filter-groups nil))

;; emacs friendly completion ui
(use-package vertico
  :bind (:map vertico-map
	 ;; vertico directory navigation.
	 ("RET" . vertico-directory-enter)
	 ("DEL" . vertico-directory-delete-char)
	 ("M-DEL" . vertico-directory-delete-word)
	 ("<next>" . vertico-scroll-up)
	 ("<prior>" . vertico-scroll-down))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  (vertico-mode))

;; fuzzy search for emacs completion
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  :bind (
	 ;; Mimics swiper
	 ("C-s" . consult-line)
	 ;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

(use-package embark
  :bind
  (("M-E" . embark-act)         ;; pick some comfortable binding
   ("M-D" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Use which-key to show the options instead:
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	 (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
	 (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
	 nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
	'(embark-which-key-indicator
	  embark-highlight-indicator
	  embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package rg
  :bind (
         ;;("C-c s" . rg-autoload-keymap)
         ("C-c s d" . rg-dwim)
         ("C-c s t" . rg-literal)
         ("C-c s k" . rg-kill-saved-searches)
         ("C-c s l" . rg-list-searches)
         ("C-c s p" . rg-project)
         ("C-c s r" . rg)
         ("C-c s S" . rg-save-search-as-name)
         ("C-c s s" . rg-save-search)
         ;; Default bind
         ("C-c S" . rg-menu)
         )
  :init
  (defun rg-autoload-keymap ()
  (interactive)
  (if (not (require 'rg nil t))
      (user-error (format "Cannot load rg"))
    (let ((key-vec (this-command-keys-vector)))
      (global-set-key key-vec rg-global-map)
      (setq unread-command-events
        (mapcar (lambda (ev) (cons t ev))
                (listify-key-sequence key-vec)))))))

;; Show hints about shortcuts
(use-package which-key
  :diminish
  :defer .1
  :init
  (setq which-key-sort-order 'which-key-prefix-then-key-order
	which-key-popup-type 'side-window
	which-key-side-window-max-height 0.5
	which-key-side-window-max-width 0.33
	which-key-idle-delay 0.5
	which-key-idle-secondary-delay 0.1
	which-key-min-display-lines 7
	which-key-sort-order 'which-key-key-order-alpha)
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

(use-package hydra
  :bind
  (("C-c g g" . hydra-git-gutter/body)
   ("C-c m"   . hydra-evil-mc/body))
  :config
  ;; Hydra mini state for git-gutter
  (defhydra hydra-git-gutter (:color pink
				     :pre (git-gutter-mode 1)
				     :base-map (make-sparse-keymap))
    "Git gutter"
    ("C-q" nil "quit")
    ("C-c C-c" magit-commit "commit" :column "General" :exit t)
    ("C-n" git-gutter:next-hunk "next" :column "Change")
    ("C-p" git-gutter:previous-hunk "previous")
    ("C-a" git-gutter:stage-hunk "Add (stage) change" :column "Actions")
    ("C-r" git-gutter:revert-hunk "revert change"))
  ;; Hydra mini state for multiple cursors
  (defhydra hydra-evil-mc (:color pink :post (evil-mc-undo-all-cursors)
				  :base-map (make-sparse-keymap))
    "Evil Multiple Cursors"
    ("C-q" nil "quit")
    ("C-c a" evil-mc-make-all-cursors "mark all" :column "General")
    ("C-c q" evil-mc-undo-all-cursors "undo all")
    ("M-n" evil-mc-make-and-goto-next-match "next" :column "Make cursor and match")
    ("M-p" evil-mc-make-and-goto-prev-match "previous")
    ("C-c n" evil-mc-skip-and-goto-next-match "next" :column "Skip and match")
    ("C-c p" evil-mc-skip-and-goto-prev-match "previous")
    ("C-n" evil-mc-make-cursor-move-next-line "next line" :column "Make cursor and move to")
    ("C-p" evil-mc-make-cursor-move-prev-line "prev line")))

(use-package warnings
  ;; builtin
  :config
  (setq warning-suppress-type
        '(flycheck syntax-checker)))

;; Control how popups are handled
(use-package popwin
  :config
  (setq popwin:popup-window-position 'bottom)
  (setq popwin:popup-window-height 0.4)
  ;; Prepend the custom entries so they take precedence
  (setq popwin:special-display-config
	(append
	 '(;; Help like buffers should be selected and shouldn't be
	   ;; dismissed when jumping to another window. It's still
	   ;; possible to dismiss them hitting ESC.
	   (help-mode :stick t)
	   (godoc-mode :stick t)
	   (eudc-mode :stick t)
	   ("*Apropos*" :stick t)
	   ("*Flycheck errors*" :stick t)
	   ("*rg*" :stick t)
	   ("^\\*cargo-.*\\*$" :regexp t :stick t)
	   ("^\\*rust.*\\*$" :regexp t :stick t)
	   ;; Information buffers shouldn't be selected by default:
	   ("*Warnings*" :noselect t)
	   ("*Backtrace*" :noselect t)
	   ("*Compile-Log*" :noselect t)
	   ("*org caldav sync result*" :noselect t)
	   ("*git-gutter:diff*" :noselect t)
           (platformio-compilation-mode :noselect t))
	 popwin:special-display-config))
  ;; Close popups with ESC
  (add-hook '~evil-esc-hook #'popwin:close-popup-window)
  ;; Workaround for help buffers
  ;; https://github.com/m2ym/popwin-el/issues/131#issuecomment-239221901
  (defadvice display-buffer (around display-buffer-prevent-popwin-split last activate)
    (let* ((buffer (ad-get-arg 0)))
      (if (and (derived-mode-p 'help-mode)
	       (get-buffer-window buffer))
	  (let ((display-buffer-alist nil))
	    ad-do-it)
	ad-do-it)))
  ;; Enable popwin
  (popwin-mode 1))

;; Highlight current line when jumping with the cursor.
(use-package pulsar
  :defer 1
  :init
  (setq pulsar-face 'pulsar-magenta)
  :config
  (pulsar-global-mode 1))

;; Highlight symbol under the cursor
(use-package highlight-symbol
  :diminish
  :hook ((prog-mode . highlight-symbol-mode))
  :config
  (setq highlight-symbol-idle-delay 0.1)
  ;; Workaround for urxvt
  (custom-set-faces
   '(highlight-symbol-face ((t (:background "brightblack"))))))

;; Highlight text affected by operations
(use-package volatile-highlights
  :diminish
  :defer 1
  :config
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
			'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)
  (set-face-attribute
   'vhl/default-face nil
   :background (face-background 'default)
   :slant 'italic)
  (volatile-highlights-mode t))

;; Select based on context
(use-package expand-region
  :bind
  (("<M-SPC>" . er/expand-region)
   (:map evil-insert-state-map ("<M-SPC>" . er/expand-region))
   (:map evil-normal-state-map ("<M-SPC>" . er/expand-region))
   (:map evil-visual-state-map ("<M-SPC>" . er/expand-region))))

;; Editable grep buffers
(use-package wgrep
  :mode "\\*grep\\*")

;; Edit multiple occurrences of a symbol at the same time
;; todo: try multiple-cursors.el again.
(use-package iedit
  :defer t ; Should be explicitly required
  :config
  (if (display-graphic-p)
      (set-face-attribute 'iedit-occurrence nil :box t)
    (set-face-attribute 'iedit-occurrence nil :background "magenta")))

(use-package evil-iedit-state
  :bind
  ;; Define key bindings here since it's necessary to
  ;; use evil-iedit-state/iedit-mode instead
  (("C-c E" . evil-iedit-state/iedit-mode)
   ("C-c e" . ~iedit-local-mode)
   (:map evil-iedit-state-map
	 ([backspace] . nil)
	 ("t"         . iedit-toggle-selection))
   (:map evil-insert-state-map
	 ("C-c E" . evil-iedit-state/iedit-mode)
	 ("C-c e" . ~iedit-local-mode))
   (:map evil-normal-state-map
	 ("C-c E" . evil-iedit-state/iedit-mode)
	 ("C-c e" . ~iedit-local-mode))
   (:map evil-visual-state-map
	 ("C-c E" . evil-iedit-state/iedit-mode)
	 ("C-c e" . ~iedit-local-mode)))
  :config
  (require 'iedit)
  (defun ~iedit-local-mode()
    "iedit-mode on the current function."
    (interactive)
    (evil-iedit-state/iedit-mode 0)))

;;;; Snippets
(use-package yasnippet-snippets
  :defer t)

(use-package yasnippet
  :diminish yas-minor-mode
  :defer .1
  :commands (yas-activate-extra-mode)
  :config
  (yas-global-mode 1))

;; Spell checking
(use-package flyspell
  ;; builtin
  :delight '(:eval (concat " FlyS:" (or ispell-local-dictionary
					ispell-dictionary)))
  :bind
  (("C-c d b" . flyspell-buffer)
   ("C-c d d" . ~flyspell-change-dictionary))
  :hook ((text-mode prog-mode) . ~flyspell-smart-mode)
  ;; $ apt install aspell
  :init
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra"))
    (setq ispell-dictionary "en_US")
    )
  :config
  (defun ~flyspell-change-dictionary ()
    "Change dictionary and check buffer."
    (interactive)
    (call-interactively 'ispell-change-dictionary)
    (flyspell-buffer))
  (defun ~flyspell-smart-mode()
    "Enable flyspell based on the current major mode."
    (interactive)
    ;; Do not block when opening a file.
    (run-at-time
     "1 sec" nil
     (lambda ()
       (let ((inhibit-message t))
	 (if (derived-mode-p 'prog-mode)
	     (flyspell-prog-mode)
	   (flyspell-mode 1)))))))

;; Syntax check
(use-package flycheck
  :defer .1
  :config
  (global-flycheck-mode))

;; Aggressive indentation
;; Use `aggressive-indent-mode' to enable it.
(use-package aggressive-indent
  :diminish "AI"
  :commands (aggressive-indent))

(use-package dtrt-indent
  :diminish
  :config
  (dtrt-indent-global-mode 1))

;; Git support
(use-package magit
  :if (executable-find "git")
  ;; global-git-commit-mode forces the load of both git-commit and magit. Copy
  ;; instead the regular expression for special git file names and defer the
  ;; package loading. Keep the :mode clause here, to enforce that magit is
  ;; loader before than git-commit.
  :mode ("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|\
MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'" . git-commit-mode)
  :commands
  (magit-commit
   magit-status
   magit-blame-addition
   magit-log-current
   magit-log-all
   magit-diff)
  :bind
  (("C-c g c" . magit-commit)
   ("C-c g s" . magit-status)
   ("C-c g b" . magit-blame-addition)
   ("C-c g l" . magit-log-current)
   ("C-c g L" . magit-log-all)
   ("C-c g d" . magit-diff))
  :init
  ;; Set YASnippet mode
  (defun ~git-commit-mode ()
    "Run when entering git-commit mode"
    (interactive)
    (when (derived-mode-p 'text-mode)
      (yas-activate-extra-mode 'text-mode+git-commit-mode)))
  (add-hook 'git-commit-setup-hook '~git-commit-mode)
  ;; Binding hint
  (which-key-add-key-based-replacements "C-c g" "magit")
  :config
  (setq magit-revision-insert-related-refs nil))

(use-package git-rebase
  :ensure magit
  :bind
  (:map git-rebase-mode-map
        ("ESC <up>" . git-rebase-move-line-up)
        ("ESC <down>" . git-rebase-move-line-down)))

(use-package transient
  :bind
  (:map transient-map
        ("<escape>" . transient-quit-one)))

(use-package git-commit
  :commands (git-commit-mode))

(use-package git-timemachine
  :bind
  (("C-c g t" . git-timemachine))
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

;; Show git status on the left margin of the file.
(use-package git-gutter
  :diminish
  :defer .1
  :bind
  (("C-c ="   . git-gutter:popup-hunk)
   ("C-c g h" . git-gutter:popup-hunk)
   ("C-c g n" . git-gutter:next-hunk)
   ("C-c g p" . git-gutter:previous-hunk)
   ("C-c g a" . git-gutter:stage-hunk)
   ("C-c g r" . git-gutter:revert-hunk))
  :config
  (setq git-gutter:update-interval 0)
  (global-git-gutter-mode 1)
  (if (not (version<= "26.0.50" emacs-version))
      (git-gutter:linum-setup)))

;; LSP
(use-package lsp-mode
  :hook ((prog-mode . lsp-mode)
	 (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-rust-analyzer-server-display-inlay-hints t
	lsp-rust-analyzer-display-lifetime-elision-hints-enable t)
  (setq lsp-warn-no-matched-clients nil)
  (setq lsp-auto-guess-root t)
  (add-hook 'lsp-after-open-hook
	    (lambda () (when (lsp-find-workspace 'rust-analyzer nil)
                         (lsp-rust-analyzer-inlay-hints-mode)))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom-face
  (lsp-ui-doc-background ((t (:background "#3a3f4b"))))
  :init
  (setq lsp-ui-sideline-show-hover t
	lsp-ui-sideline-show-code-actions t))

;; Auto complete
(use-package company
  :demand
  :diminish "Comp"
  :bind
  ((:map company-active-map
	 ("RET"      . company-complete-selection)
	 ("<return>" . company-complete-selection)
	 ("C-o"      . company-complete-selection)
	 ("<escape>" . ~company-abort)
	 ("<down>"   . company-select-next)
	 ("C-n"      . company-select-next)
	 ("<up>"     . company-select-previous)
	 ("C-p"      . company-select-previous)
	 ("<next>"   . company-next-page)
	 ("<prior>"  . company-previous-page)))
  :config
  (defun ~company-abort ()
    (interactive)
    "Abort completion and return to normal state."
    (company-abort)
    (if (boundp 'evil-mode)
	(evil-force-normal-state)))
  (company-mode)
  ;; Time before completion starts
  (setq company-idle-delay 0.1)
  ;; Avoid triggering the generation of tag completion table (that bricks
  ;; the UI and might take very long on large projects).
  (setq-default company-etags-use-main-table-list nil)
  ;; The minimum prefix length for idle completion.
  (setq company-minimum-prefix-length 3)
  ;; Allow user to type a value that is not listed in the completion.
  (setq company-require-match 'never)
  ;; Trigger auto completion on some characters
  (setq company-auto-complete 'company-explicit-action-p)
  ;; Enable it globally.
  (global-company-mode))

;; clang based completion server
;; apt install clang
;; M-x irony-install-server
(use-package irony
  :hook
  (((c++-mode c-mode) . irony-mode)
   (irony-mode        . irony-cdb-autosetup-compile-options)))

;; flycheck irony support
(use-package flycheck-irony
  :defer t
  :hook
  ((flycheck-mode . flycheck-irony-setup)))

;; company integration
(use-package company-irony
  :after (:all company irony)
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package irony-eldoc
  :hook
  ((irony-mode . irony-eldoc)))

;; C headers completion based on the irony server
(use-package company-irony-c-headers
  :after (:all company-irony)
  :config
  (add-to-list
   'company-backends
   '(company-irony-c-headers company-irony)))

;; Basic support for etags if gtags is not available:
(use-package etags
  :defer .1
  :config
  ;; Don't ask before re-reading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil))

;; Tag-like jump without any support
(use-package dumb-jump
  :init
  (which-key-add-key-based-replacements "C-c j" "dump-jump")
  :bind
  (("C-c j o" . dumb-jump-go-other-window)
   ("C-c j j" . dumb-jump-go)
   ("C-c j b" . dumb-jump-back)
   ("C-c j i" . dumb-jump-go-prompt)
   ("C-c j x" . dumb-jump-go-prefer-external)
   ("C-c j z" . dumb-jump-go-prefer-external-other-window))
  :config
  (require 'cl)
  (delete "Makefile" dumb-jump-project-denoters)
  (setq dumb-jump-force-searcher 'git-grep-plus-ag)
  (setq dumb-jump-max-find-time 20))

(use-package avy
  :bind
  (("C-c ;" . avy-goto-char)
   ("C-c :" . avy-goto-char-2)))

(use-package ag
  :bind
  (("C-c A a"   . ag)
   ("C-c A A"   . ag)
   ("C-c A f"   . ag-files)
   ("C-c A r"   . ag-regexp)
   ("C-c A p p" . ag-project)
   ("C-c A p f" . ag-project-files)
   ("C-c A p r" . ag-project-regexp))
  :init
  (which-key-add-key-based-replacements "C-c a" "Ag"))

;; Code and project navigation
(use-package projectile
  :commands (projectile-mode)
  :hook
  ((prog-mode . projectile-mode))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (which-key-add-key-based-replacements "C-c p" "Projectile")
  :config
  (defun ~projectile-get-project-name (project-root)
    "Default function used create project name to be displayed based on the value of PROJECT-ROOT."
    (let ((default (file-name-nondirectory (directory-file-name project-root))))
      (if (not (equal default "git"))
	  default
	(file-name-nondirectory
	 (directory-file-name
	  (file-name-directory (directory-file-name project-root)))))))
  (setq projectile-project-name-function '~projectile-get-project-name)
  (setq projectile-mode-line
	'(:eval (format " Proj[%s]" (projectile-project-name))))
  ;; fd in Ubuntu 20.04 is missing the option --strip-cwd-prefix, for
  ;; force find by default:
  (setq projectile-generic-command "find . -type f | cut -c3- | tr '\\n' '\\0'")
  (projectile-mode))

(use-package ibuffer-projectile
  :after ibuffer
  :hook
  ((ibuffer . ~ibuffer-set-filter-groups))
  :init
  ;; Custom groups:
  (setq ~custom-ibuffer-filter-groups
	'(("Mail" (or (mode . mu4e-compose-mode)
		      (name . "\\*mu4e-")))
	  ("IRC" (mode . erc-mode))))
  ;; Hook to add projetile and custom groups:
  (defun ~ibuffer-set-filter-groups()
    "Set ibuffer for projectile and custom groups."
    (setq ibuffer-filter-groups
	  (append ~custom-ibuffer-filter-groups
		  (ibuffer-projectile-generate-filter-groups)))
    (message "~ibuffer-set-filter-groups: groups set")
    (let ((ibuf (get-buffer "*Ibuffer*")))
      (when ibuf
	(with-current-buffer ibuf
          (pop-to-buffer ibuf)
          (ibuffer-update nil t))))))

;; Better embedded terminal

(use-package vterm
  :commands (vterm))

(use-package multi-vterm
  :commands
  (multi-vterm
   multi-vterm-next
   multi-vterm-prev)
  :bind
  (("C-c T" . multi-vterm)))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

;; org mode
(use-package org
  :pin gnu
  :after (company)
  :mode ("\\.org$" . org-mode)
  :commands (org-mode orgtbl-mode)
  :hook ((org-mode . ~org-format-on-save))
  :bind
  (; "C-c !" for inactive timestamps conflicts with flycheck bindings,
   ; so overload the binding to active timestamps ("C-c ."):
   (:map org-mode-map
	 ("C-c ."   . nil)
	 ("C-c . ." . org-time-stamp)
	 ("C-c . a" . org-time-stamp)
	 ("C-c . !" . org-time-stamp-inactive)
	 ("C-c . i" . org-time-stamp-inactive)))
  :config
  (setq org-modules
	(append org-modules '(org-tempo)))
  ;; Use org indent
  (setq org-startup-indented t)
  ;; Use regular background for code blocks
  (dolist (f '(org-block-begin-line org-block org-block-end-line))
    (set-face-background f (face-background 'org-default)))
  ;; Add dates when a task is marked as done
  (setq org-log-done 'time)
  ;; Use #+ATTR_ORG: :width N
  (setq org-image-actual-width nil)
  ;; Default LaTeX packages when exporting
  (setq org-latex-packages-alist '(("" "parskip" t)))
  ;; Add new items to the top
  (setq org-reverse-note-order t)
  ;; Quick src blocks with <, ie <el for emacs-lisp:
  (setq org-structure-template-alist
	(append org-structure-template-alist
		'(("el"   . "src emacs-lisp")
		  ("cc"   . "src c")
		  ("py"   . "src python")
		  ("sh"   . "src sh")
		  ("bash" . "src bash")
		  ("perl" . "src perl"))))
  ;; Auto-complete
  ;; https://emacs.stackexchange.com/questions/21171/company-mode-completion-for-org-keywords
  (defun ~org-keyword-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend '~org-keyword-backend))
      (prefix (and (eq major-mode 'org-mode)
		   (cons (company-grab-line "^#\\+\\(\\w*\\)" 1)
			 t)))
      (candidates (mapcar #'upcase
			  (cl-remove-if-not
			   (lambda (c) (string-prefix-p arg c))
			   (pcomplete-completions))))
      (ignore-case t)
      (duplicates t)))
  (add-to-list 'company-backends '~org-keyword-backend)
  ;; Fix blank lines
  (defun unpackaged/org-fix-blank-lines (&optional prefix)
    "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
    (interactive "P")
    (org-map-entries (lambda ()
                       (org-with-wide-buffer
			;; `org-map-entries' narrows the buffer, which prevents us from seeing
			;; newlines before the current heading, so we do this part widened.
			(while (not (looking-back "\n\n" nil))
                          ;; Insert blank lines before heading.
                          (insert "\n")))
                       (let ((end (org-entry-end-position)))
			 ;; Insert blank lines before entry content
			 (forward-line)
			 (while (and (org-at-planning-p)
                                     (< (point) (point-max)))
                           ;; Skip planning lines
                           (forward-line))
			 (while (re-search-forward org-drawer-regexp end t)
                           ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                           ;; for some reason it doesn't work correctly when operating on hidden text.
                           ;; This works, taken from `org-agenda-get-some-entry-text'.
                           (re-search-forward "^[ \t]*:END:.*\n?" end t)
                           (goto-char (match-end 0)))
			 (unless (or (= (point) (point-max))
                                     (org-at-heading-p)
                                     (looking-at-p "\n"))
                           (insert "\n"))))
                     t (if prefix
                           nil
			 'tree)))
  (defun ~org-sort-level-1 ()
    "Sort entries under each level 1 headings."
    (interactive)
    ;; Save current position based on the current headline and the
    ;; cursor position inside it. This is not an ideal solution
    ;; because it doesn't handle duplicate headlines, but it's still
    ;; better than nothing:
    (let* ((headline (nth 4 (org-heading-components)))
         (headline-pos (marker-position (org-find-exact-headline-in-buffer headline)))
         (relative-pos (- (point) headline-pos)))
      ;; Sort the first level entries. That will probably move the
      ;; cursor to a different position:
      (org-map-entries
       (lambda ()
	 (org-sort-entries nil ?o))
       "LEVEL=1" 'file)
      ;; Restore the cursor based on the headline and relative cursor
      ;; position:
      (let* ((headline-pos (marker-position (org-find-exact-headline-in-buffer headline)))
             (pos (+ headline-pos relative-pos)))
        (goto-char pos)))
    ;; Clean any trailing whitespace for sanity:
    (delete-trailing-whitespace nil nil))
  (defun ~org-format ()
    "Format org buffer."
    (interactive)
    (when (not (string-match-p "_archive$" buffer-file-name))
      (~org-sort-level-1)
      (unpackaged/org-fix-blank-lines t)))
  (defun ~org-format-on-save ()
    "Reformat buffer on save."
    (interactive)
    (add-hook 'before-save-hook '~org-format 0 t)
    (add-hook 'edit-server-done-hook '~org-format 0 t))
  (defun ~org-archive-all-done ()
    "Archive all elements marked as done in the file."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from
	     (org-element-property :begin (org-element-at-point))))
     "TODO=\"DONE\"" 'file)))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

;; Hide and automatically show hidden elements.
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :init
  ;; Hide by default:
  (setq org-hide-emphasis-markers t)
  ;; Auto unhide the following elements:
  (setq org-appear-autoentities t)
  (setq org-appear-autolinks t)
  (setq org-appear-autokeywords t))

;; Replacing orgstruct-mode
(use-package outshine
  :defer t)

(use-package evil-org
  :after (:all evil org)
  :hook
  ((org-mode      . evil-org-mode)
   (evil-org-mode . evil-org-set-key-theme)))

;; org-agenda
;;
;; Tips and tricks:
;; 1. To create a new entry use "M-RET" to create a new outline/item.
;; 2. To add a timestamp use "C-c .".
;;    1. To pick a date use "S-<arrows>"
;;    2. To pick a time, simply type:
;;       - "<start>-<end>" (ie: "10:00-11:00"), or
;;       - "<start>+<hours>" (ie: "10:00+1")
;; Or use an org-capture template.
(use-package org-agenda
  :ensure org
  :defer t
  :bind
  (("C-c a" . org-agenda))
  :init
  (setq org-agenda-files '("~/Dropbox/org/" "~/Dropbox/org/projects" "~/Dropbox/org/1on1"))
  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :level . 0)
                             (org-agenda-files :maxlevel . 1)))
  (setq org-outline-path-complete-in-steps nil) ; Refile in a single go
  (setq org-refile-use-outline-path t) ; Show full paths for refiling
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-complete-tags-always-offer-all-agenda-tags t)
  ;; Save after refiling
  (advice-add 'org-refile :after
        (lambda (&rest _)
        (org-save-all-org-buffers)))
  ;; Add special color to common tags. We can customize the list of
  ;; available tags (wich shortcuts) using org-tag-persistent-alist
  ;; too:
  (setq org-tag-faces
	'(("important" . (:foreground "white" :background "firebrick" :weight bold))
	  ("request" . (:foreground "green"))
	  ("bug" . (:foreground "orange"))
	  ))

  :config
  (evil-set-initial-state 'org-agenda-mode 'insert))

(use-package org-capture
  :ensure org
  :commands (~org-capture-make-frame)
  :bind
  (("C-c c" . org-capture))
  :config
  (require 'noflet)
  ;; Start a capture in insert mode
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  ;;templates
  (defvar ~org-capture-root "~/Dropbox/org/")
  (setq org-capture-templates
	`(
	  ("t" "Todo" entry
	   (file+headline ,(concat ~org-capture-root "todos.org") "Recent")
	   "* TODO %?\n%u\n%a"
	   :empty-lines 1 :prepend t)
	  ("w" "Week" entry
	   (file+headline ,(concat ~org-capture-root "week.org") "Current")
	   "** %?\n%u\n%a"
	   :empty-lines 1)
	  ("a" "Agenda" entry
	   (file ,(concat ~org-capture-root "cal.org"))
	   "* %?\n%^T"
	   :empty-lines 1)
	  ))

  ;; The following is a workaround to open an org-capture frame
  ;; directly from the desktop.
  ;;
  ;; To open a new GUI frame use:
  ;;     emacsclient --display "$DISPLAY" -ne '(~org-capture-make-frame)'
  ;;
  ;; However, that somehow messes up with i3 after the frame is
  ;; closed. So an alternative is to use a terminal instead:
  ;;
  ;;     for_window [instance=org-capture-frame] floating enable
  ;;     bindsym $mod+p Exec --no-startup-id \
  ;;     	alacritty --class org-capture-frame \
  ;;     	-e emacsclient -t -c -e '(~org-capture-make-frame)'

  ;; Close the frame when a capture is completed.
  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "org-capture-frame" (frame-parameter nil 'name))
	(save-buffers-kill-terminal)))
  ;; Close the frame when a capture is aborted.
  (defadvice org-capture-destroy
      (after delete-capture-frame activate)
    "Advise capture-destroy to close the frame"
    (if (equal "org-capture-frame" (frame-parameter nil 'name))
	(save-buffers-kill-terminal)))
  ;; Function to be called by emacsclient to create the capture frame
  (defun ~org-capture-make-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    ;; That name will be used by the advices above to kill the frame
    ;; when necessary.
    (make-frame '((name . "org-capture-frame")))
    (select-frame-by-name "org-capture-frame")
    ;; Replace only for this call the behavior of the function that
    ;; splits and jumps to the other window. That will force the
    ;; org-capture selection screen to use the entire screen.
    (noflet ((org-switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
      (org-capture))
    (evil-insert-state)))

;; Query org files
(use-package org-ql
  :defer t
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"
            :files (:defaults (:exclude "helm-org-ql.el"))))

;; Notification for org-agenda
(use-package org-notify
  :quelpa (org-notify :fetcher github :repo "mhcerri/org-notify")
  :after (server)
  :init
  (setq org-notify-icon "~/.emacs.d/org.png"
	org-notify-max-notifications-per-run nil
	org-notify-default-notify-urgency 'low)
  :config
  (org-notify-add
   'default
   '(:time "5m" :period "1h" :actions -notify))
  ;; Only enable by default for the emacs daemon:
  (when (daemonp)
    (require 'org)
    (org-notify-start)))

;; Make conf-mode more useful
(use-package conf-mode
  :mode "/\\.config/")

;; C and C-like
(use-package cc-mode
  :defer t
  :config
  (setq c-block-comment-prefix "* ")
  (setq c-default-style '((other . "linux"))))

;; Platform.io
(use-package platformio-mode
  :defer t
  :hook
  (((c++-mode arduino-mode) . ~pio-cpp-hook)
   (irony-mode . ~pio-irony-hook))
  :commands
  (platformio-conditionally-enable)
  :init
  (defun ~pio-cpp-hook()
    (irony-mode)
    (irony-eldoc)
    (platformio-conditionally-enable))
  (defun ~pio-irony-hook()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async)
    (irony-cdb-autosetup-compile-options)))

;; Python
;; apt install python3-jedi python3-flake8 python3-autopep8 python3-yapf python3-pip
(use-package elpy
  :defer t
  :hook
  ((python-mode . elpy-enable))
  :init
  ;; Fix error in process sentinel..
  (setenv "PYTHONIOENCODING" "utf-8")
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
  (add-to-list 'process-coding-system-alist '("elpy" . (utf-8 . utf-8)))
  (add-to-list 'process-coding-system-alist '("flake8" . (utf-8 . utf-8)))
  :config
  ;; Set python3 as default:
  (setq python-shell-interpreter "python3")
  (setq elpy-rpc-python-command "python3")
  (setq flycheck-python-pycompile-executable "python3")
  (setq flycheck-python-flake8-executable "flake8"))

(use-package company-jedi
  :hook (python-mode . ~company-jedi)
  :init
  (defun ~company-jedi()
    (add-to-list 'company-backends 'company-jedi)))

;; GoLang
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (defun setup-go-mode-env ()
    ""
    (interactive)
    (message "setup-go-mode-env")
    (make-local-variable 'process-environment)
    (let ((val (shell-command-to-string "wgo env GOPATH")))
      (if (not (string= val "no workspace"))
	  (setenv "GOPATH" val)
	)
      )
    )
  (add-hook 'go-mode-hook 'setup-go-mode-env)
  (add-hook 'before-save 'gofmt-before-save))

(use-package company-go
  :after (company)
  :config
  (add-to-list 'company-backends 'company-go))

(use-package go-eldoc
  :hook
  ((go-mode . go-eldoc-setup)))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :defer t)

(use-package rustic
  :after (rust-mode))

(use-package lua-mode
  :defer t)

(use-package company-lua
  :after (company)
  :config
  (add-to-list 'company-backends 'company-lua))

(use-package love-minor-mode
  :defer t
  :quelpa (love-minor-mode :fetcher github :repo "mhcerri/love-minor-mode"))

(use-package web-mode
  :defer t
  :config
  ;; Fix bracket color
  (set-face-foreground 'web-mode-html-tag-bracket-face nil))

(use-package lorem-ipsum)

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package arduino-mode
  :after (irony)
  :mode "\\.ino\\'"
  :config
  (add-to-list 'irony-supported-major-modes 'arduino-mode)
  (add-to-list 'irony-lang-compile-option-alist '(arduino-mode . "c++")))

;; Mail
(use-package sendmail
  :ensure nil ; builtin
  :defer t
  :mode ("mutt-.*$" . mail-mode)
  :hook
  ((mail-mode . ~mail-mode))
  :init
  (defun ~mail-mode()
    (font-lock-add-keywords nil
			    '(("^[ \t]*>[ \t]*>[ \t]*>.*$"
			       (0 'mail-multiply-quoted-text-face))
			      ("^[ \t]*>[ \t]*>.*$"
			       (0 'mail-double-quoted-text-face))))
    ;; Arrange paragraphs and ignore white space issues
    ;;(auto-fill-mode 1) ; Use M-q instead
    (whitespace-mode -1)
    ;; Enable spell checking
    (flyspell-mode 1)
    ;; Move the cursor to the right position
    (re-search-forward "^$")
    (forward-char 1)
    ;; Enable org modes
    (outshine-mode)
    (orgtbl-mode)))

(use-package message
  :ensure nil ; builtin
  :defer t
  ;:mode ("mutt-.*$" . message-mode)
  :hook
  ((message-mode . ~message-mode))
  :init
  ;; Use custom format for the citation line:
  (setq message-citation-line-function #'message-insert-formatted-citation-line)
  ;; Use default format without the extra new line:
  (setq message-citation-line-format "On %a, %b %d %Y, %N wrote:")

  (defun ~message-mode()
    (font-lock-add-keywords nil
			    '(("^[ \t]*>[ \t]*>[ \t]*>.*$"
			       (0 'message-multiply-quoted-text-face))
			      ("^[ \t]*>[ \t]*>.*$"
			       (0 'message-double-quoted-text-face))))
    ;; Arrange paragraphs and ignore white space issues
    ;;(auto-fill-mode 1) ; Use M-q instead
    (whitespace-mode -1)
    ;; Enable spell checking
    (flyspell-mode-on)
    ;; Move the cursor to the right position
    (re-search-forward "^$")
    (forward-char 1)
    ;; Enable org modes
    (outshine-mode)
    (orgtbl-mode))
  :config
  ;; This is needed to allow msmtp to do its magic:
  (setq message-sendmail-f-is-evil 't)
  ;; Need to tell msmtp which account we're using
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  ;; Use msmtp as sendmail
  (setq-default sendmail-program "msmtp")
  ;; Use sendmail
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  ;; Read email address from msmtp configuration.
  (let ((from (shell-command-to-string
	       (concat "echo | msmtp -P | awk '$1 == \"from\" "
		       "{ printf(\"%s\", $3); exit }'"))))
    (if (string-match
	 "^[a-za-z0-9_.+-]+@[a-za-z0-9-]+\\.[a-za-z0-9-.]+$" from)
	(setq-default user-mail-address from)
      (message "Invalid email from msmtp configuration: '%s'" from))))

(use-package mu4e
  ;; Always use custom mu binaries:
  :load-path "~/Documents/workspace/mu/build/mu4e/"
  :general
  ("C-c @"  'mu4e)
  (:states 'normal :keymaps 'mu4e-headers-mode-map
	   "N"            '~mu4e-headers-narrow-unread
	   "I"            '~mu4e-headers-narrow-important
	   "TAB"          'mu4e-headers-next-unread
           "C-r"          '~mu4e-headers-mark-thread-read
           "<insert>"     '~mu4e-headers-mark-thread-read
           "<insertchar>" '~mu4e-headers-mark-thread-read)
  (:states 'normal :keymaps '(mu4e-main-mode-map
                              mu4e-headers-mode-map
                              mu4e-view-mode-map
                              mu4e-compose-mode-map)
           "<f5>"         '~mu4e-update-mail-and-index)
  (:states 'normal :keymaps 'mu4e-view-mode-map
	   "g u"          '~mu4e-view-copy-url)
  (:states 'normal :keymaps '(mu4e-headers-mode-map
			      mu4e-view-mode-map)
	   "c p"          '~mu4e-compose-reply-patch)
  :init
  ;; Use mu4e for e-mail in emacs
  (setq mail-user-agent 'mu4e-user-agent)
  (set-variable 'read-mail-command 'mu4e)
  ;; Always encrypt to your self
  (setq-local mml-secure-openpgp-encrypt-to-self t)
  :config
  ;; Use my custom mu
  (setq mu4e-mu-binary "~/Documents/workspace/mu/build/mu/mu")
  ;; Use ivy for prompts
  (setq mu4e-completing-read-function 'completing-read-default)

  ;; Path to Maildir directory
  (setq mu4e-root-maildir "~/.mail/work/")

  ;; The next are relative to `mu4e-maildir'
  ;; instead of strings, they can be functions too, see
  ;; their docstring or the chapter 'Dynamic folders'
  (setq mu4e-sent-folder   "/Sent Mail"
	mu4e-drafts-folder "/Drafts"
	mu4e-trash-folder  "/Trash")

  ;; Headers
  ;; (setq mu4e-headers-results-limit -1)
  (setq mu4e-search-results-limit 1000)
  (setq mu4e-headers-include-related t)
  (setq mu4e-headers-fields
	'((:human-date     . 12)
	  (:flags          .  4)
	  (:from-or-to     . 22)
	  (:thread-subject . nil)))

  ;(setq mu4e-use-fancy-chars t)
  (setq mu4e-headers-thread-mark-as-orphan 'first
        mu4e-headers-thread-child-prefix '("├>" . "├▸ ")
	mu4e-headers-thread-first-child-prefix '("├>" . "├▸ ")
        mu4e-headers-thread-last-child-prefix '("└>" . "└▸ ")
        mu4e-headers-thread-connection-prefix '("│" . "│ ")
	mu4e-headers-thread-single-orphan-prefix '("─>" . "─▸")
	mu4e-headers-thread-orphan-prefix '("┬>" . "┬▸"))

  ;; Bookmarks (works better than mu4e-maildir-shortcuts)
  ;; Avoid appending since for some reason use-package is evaluating :config
  ;; twice for mu4e
  (setq mu4e-bookmarks
	`(;; Common folders
	  (:name "/INBOX"
                 :key ?i
	         :query "maildir:\"/INBOX\"")
	  (:name "/INBOX (unread & flagged)"
                 :key ?I
	         :query "(maildir:\"/INBOX\") AND (flag:unread OR flag:flagged)")
	  (:name ,mu4e-drafts-folder
                 :key ?d
	         :query ,(format "maildir:\"%s\"" mu4e-drafts-folder))
	  (:name ,mu4e-sent-folder
                 :key ?s
	         :query ,(format "maildir:\"%s\"" mu4e-sent-folder))
	  ;; Work folders
	  (:name "/Bugs/Important"
		 :key ?b
		 :query "maildir:\"/Bugs/Important\"")
          (:name "/Canonical Kernel Team"
		 :key ?c
		 :query "maildir:\"/Canonical Kernel Team\"")
          (:name "/Public/Kernel Team"
		 :key ?u
		 :query "maildir:\"/Public/Kernel Team\"")
	  (:name "/Internal/AllHands"
		 :key ?a
		 :query "maildir:\"/Internal/AllHands\"")))

  ;; mu4e actions (access via "a")
  (defun ~mu4e-action-save-message (msg)
    "Save email to a temp file."
    (interactive)
    (let ((src (mu4e-message-field msg :path))
	  (dst (read-file-name "Save to file: ")))
      (copy-file src dst)
      (message (concat "Saved file: " dst))))

  (dolist (l '(mu4e-headers-actions mu4e-view-actions))
    (dolist (i '(("Save email" . ~mu4e-action-save-message)
		 ("Apply mbox" . mu4e-action-git-apply-mbox)))
      (add-to-list l i 't)))

  ;; Utility functions for shorcuts
  (defun ~mu4e-headers-mark-thread-read ()
    "Mark thread under the cursor as read."
    (interactive)
    (if (> (mu4e-mark-marks-num) 0)
       ;(mu4e-mark-unmark-all)
       (mu4e-warn "ABORTING! Existing marks..."))
    (mu4e-headers-mark-thread nil '(read . nil))
    (mu4e-mark-execute-all t))

  (defun ~mu4e-headers-narrow-unread ()
    "Narrow the filter to unread messages."
    (interactive)
    (mu4e-headers-search-narrow "flag:unread"))

  (defun ~mu4e-headers-narrow-important ()
    "Narrow the filter to unread and flagged messages."
    (interactive)
    (mu4e-headers-search-narrow "flag:unread OR flag:flagged"))

  (defun ~mu4e-view-copy-url (&optional multi)
    "Offer to copy URLs to the clipboard.
If MULTI (prefix-argument) is nil, save a single one, otherwise, offer
to save a range of URLs."
    (interactive "P")
    (mu4e~view-handle-urls
     "URL to save" multi
     (lambda (url)
       (simpleclip-set-contents url)
       (mu4e-message "Copied %s to clipboard" url))))

  (defun ~mu4e-update-mail-and-index (full)
    "Run mu4e-update-mail-and-index using my mbsync quick profile by default."
    (interactive "P")
    (let* ((arg (if full "-a" "quick"))
	   (mu4e-get-mail-command (format "%s %s" ~mu4e-get-mail-executable arg)))
      (mu4e-update-mail-and-index 't)))

  (defun ~mu4e-compose-reply-patch-hook()
    "Hook to edit replies to patches."
    (remove-hook 'mu4e-compose-mode-hook
		 #'~mu4e-compose-reply-patch-hook)
    ;; Prompt for the patch action:
    (let* ((prefix (mu4e-read-option
		    "Patch action "
		    '(("ack" . "ACK")
		      ("nack" . "NACK")
		      ("Applied" . "APPLIED"))))
	   (body (pcase prefix
		   ("ACK" (when (and (bound-and-true-p user-full-name)
				     (bound-and-true-p user-mail-address))
			    (format "Acked-by: %s <%s>"
				    user-full-name
				    user-mail-address)))
		   ("APPLIED" "Applied. Thanks!")
		   (prefix nil))))
      ;; Update subject
      (save-excursion
	(goto-line 0)
	(when (re-search-forward "^\\(Subject:\\)\\([ \\t]*\\(Re\\|RE\\):\\)?" nil t)
	  (replace-match (format "\\1 %s:" prefix) t)))
      ;; Add acked-by tag
      (when (bound-and-true-p body)
	;; Move to the end of the body, just before the first
	;; empty line:
	(goto-char (point-max))
	(forward-line -1)
	(re-search-backward "^$")
	(insert (format "\n%s\n" body)))))

  (defun ~mu4e-compose-reply-patch()
    "Reply to patches with pre formated messages."
    (interactive)
    ;; The mu4e-compose-mode-hook is run asynchronously with
    ;; mu4e-compose-reply. mu4e-compose-reply will request a reply
    ;; message, and the hook is only invoked later when the server sends
    ;; the reply command back. Because of that, it's not possible to
    ;; override mu4e-compose-mode-hook only for a single
    ;; mu4e-compose-reply call. The workaround is to register the
    ;; special hook, call mu4e-compose-reply and then the hook will
    ;; remove itself.
    (add-hook 'mu4e-compose-mode-hook
	      #'~mu4e-compose-reply-patch-hook)
    (mu4e-compose-reply))

  ;; Make thread subjects smarter
  ;; mu4e-headers-fields would have been a better alternative, but
  ;; that doesn't work well with mu4e-columns-faces.
  (defvar-local ~mu4e~headers-thread-subject-state '())
  (defun ~mu4e~headers-thread-subject (msg)
    "Get the subject if it doesn't match the parent's subject,
ignoring prefixes added for replies. In other words, show the
subject of a thread only once, similar to e.g. 'mutt'."
    (let* ((tinfo  (mu4e-message-field msg :meta))
	   (level (plist-get tinfo :level))
           (subj (mu4e-msg-field msg :subject)))
      ;; Trim and pad the list (if missing levels)
      (let* ((length (length ~mu4e~headers-thread-subject-state))
	     (padding (make-list (max 0 (- level length)) nil)))
	(setq ~mu4e~headers-thread-subject-state
	      (cl-subseq
	       (append ~mu4e~headers-thread-subject-state padding)
	       0 level)))
      ;; Get parent
      (let* ((parent (car (last ~mu4e~headers-thread-subject-state)))
	     (reply-regexp "^\\(Re\\|RE\\)[ \t]*:[ \t]*"))
	;; Append to the list for the next header
	(setq ~mu4e~headers-thread-subject-state
	      (append ~mu4e~headers-thread-subject-state (list subj)))
	(concat
	 ;; prefix subject with a thread indicator
	 (mu4e~headers-thread-prefix tinfo)
	 ;; Compare with the parent subject to decide if we can suppress
	 ;; it or not:
	 (if (string-equal
	      (replace-regexp-in-string reply-regexp "" (or parent ""))
	      (replace-regexp-in-string reply-regexp "" (or subj "")))
	     "" subj)))))
  (defun ~mu4e~headers-cached-thread-subject (msg)
    "try to cache the return of ~mu4e~headers-thread-subject, so
the header prefix and subject do not break when updating a single
message without the thread context when viewing or flagging a
message."
    (let* ((tinfo  (mu4e-message-field msg :meta)))
      (when (not (plist-get tinfo :cached-thread-subject))
	(setq tinfo
	      (plist-put tinfo :cached-thread-subject
			 (~mu4e~headers-thread-subject msg))))
      (plist-get tinfo :cached-thread-subject)))
  (advice-add 'mu4e~headers-thread-subject :override
	      #'~mu4e~headers-cached-thread-subject)

  ;; Replace the internal function that updates the headers view to
  ;; prevent any update while the view window is open.
  (defun mu4e~headers-maybe-auto-update ()
    "Update the current headers buffer after indexing has brought
some changes, `mu4e-headers-auto-update' is non-nil and there is
no user-interaction ongoing."
    (when (and mu4e-headers-auto-update          ;; must be set
	       mu4e-index-update-status
	       (not (zerop (plist-get mu4e-index-update-status :updated)))
	       (zerop (mu4e-mark-marks-num))     ;; non active marks
	       (not (active-minibuffer-window))) ;; no user input only
      ;; rerun search if there's a live window with search results;
      ;; otherwise we'd trigger a headers view from out of nowhere.
      (when (and (buffer-live-p (mu4e-get-headers-buffer))
		 ;; CHANGE BEGIN: Only update when the headers buffer is focused
		 (eq (mu4e-get-headers-buffer) (window-buffer (selected-window)))
		 ;; CHANGE END
		 (window-live-p (get-buffer-window (mu4e-get-headers-buffer) t)))
	(mu4e-search-rerun))))

  (setq mu4e-headers-auto-update t)

  ;; Ignore unwanted contacts
  (defun ~mu4e-contact-processor (contact)
    (cond
     ;; remove unwanted
     ((string-match-p "no[t]?[-\\.]?repl\\(y\\|ies\\)" contact) nil)
     ((string-match-p "@docs.google.com" contact) nil)
     ((string-match-p ".launchpad.net" contact) nil)
     ;; default
     (t contact)))
  (setq mu4e-contact-process-function '~mu4e-contact-processor)

  ;; Compat fixes:
  ;; Fix evil-collection for mu4e
  ;(defalias 'mu4e-view-quit 'mu4e~view-quit-buffer)
  (defun mu4e~view-quit-buffer ()
    "Fix evil q in mu4e-view."
    (interactive)
    (mu4e-view-quit))

  ;; Faces
  ;; Make highlighted line better
  (set-face-attribute 'mu4e-header-highlight-face nil :underline nil)
  (set-face-attribute 'mu4e-header-highlight-face nil :reverse-video t)

  ;; Configure compose mode
  (add-hook
   'mu4e-compose-mode-hook
   (lambda ()
     ;; "org-mode" minor mode
     (outshine-mode)
     (orgtbl-mode)
     ;; Always GPG sign the messages
     (mml-secure-message-sign-pgp)
     ;; Use M-q instead
     ;;(auto-fill-mode 1)
     ;; Do not mark trailing spaces as red
     (whitespace-mode -1)))

  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; Mail signature
  (setq mu4e-compose-signature "Regards,\nMarcelo\n")

  ;; Mail view
  (setq mu4e-view-show-addresses t)
  (setq mu4e-view-show-images t)
  (setq mu4e-view-prefer-html nil)

  ;; Required by mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; update every N minutes
  (setq
   ~mu4e-get-mail-executable "~/bin/mbsync.sh"
   mu4e-get-mail-command (format "%s quick" ~mu4e-get-mail-executable)
   mu4e-update-interval (* 5 60)))

(use-package mu4e-thread-folding
  :quelpa (mu4e-thread-folding :fetcher github :repo "rougier/mu4e-thread-folding")
  :general
  (:states 'normal :keymaps 'mu4e-headers-mode-map
	   "TAB"          'mu4e-headers-toggle-at-point
           "<left>"       'mu4e-headers-fold-at-point
           "<S-left>"     'mu4e-headers-fold-all
           "<right>"      'mu4e-headers-unfold-at-point
           "<S-right>"    'mu4e-headers-unfold-all))

(use-package mu4e-speedbar
  :ensure nil ; Provided by mu4e above
  :after (mu4e))

(use-package alert
  :defer t
  :init
  (setq alert-fade-time 0)
  (setq alert-default-style 'libnotify))

(use-package mu4e-alert
  :after (mu4e)
  :init
  (setq mu4e-alert-interesting-mail-query "maildir:/INBOX AND flag:unread"
	mu4e-alert-icon "internet-mail")
  :config
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications))

(use-package mu4e-jump-to-list
  :after (mu4e))

;; Diff "mode" for mu4e-view-mode (which is backed by gnus)
(use-package message-view-patch
  :hook (gnus-part-display . message-view-patch-highlight))

(use-package mu4e-query-fragments
  :after (mu4e)
  :init
  (setq mu4e-query-fragments-list
	'(("%junk" . "maildir:/Junk OR subject:SPAM")
	  ("%hidden" . "flag:trashed OR %junk")))
  (setq mu4e-query-fragments-append "AND NOT %hidden"))

(use-package ldap
  :commands (ldap-search)
  :init
  ;; The remaining configuration is done via ~/.ldaprc
  (setq ldap-default-host "" ; That's necessary to use the host from the ldap conf
	ldap-ldapsearch-args '("-LL" "-tt" "-x" "-y" "/home/mhcerri/.ldappw")))

;; IRC
(use-package erc
  :commands (erc erc-tls)
  :hook ((erc-mode . ~erc-setup))
  :bind (:map erc-mode-map
	      ("C-c L" . ~erc-show-log))
  :init
  (setq erc-pcomplete-nick-postfix ", "
	erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")
	erc-insert-timestamp-function 'erc-insert-timestamp-left
	erc-timestamp-format "[%H:%M] "
	erc-log-channels-directory "~/.log/erc/"
	erc-log-write-after-insert t
	erc-log-insert-log-on-open nil ; use C-c l instead.
	erc-query-display 'buffer
	erc-auto-query 'bury
	erc-interpret-mirc-color t)
  :config
  (defun ~erc-setup ()
    "Configure ERC."
    (erc-fill-disable)
    (erc-spelling-mode 1))
  ;; Load additional ERC modules:
  (setq erc-modules (append erc-modules '(log)))
  (erc-update-modules)
  ;; Open the corresponding log file:
  (defun ~erc-show-log ()
    "Open the corresponding ERC log file."
    (interactive)
    (find-file (erc-current-logfile))
    (end-of-buffer)))

(use-package ercn
  :after erc
  :init
  (setq ercn-notify-rules
	'((current-nick . all)
          (query-buffer . all)
          (keyword . all)
	  (message . ("#ltc-friends" "#ubuntu-kernel" "##mhcerri"))))
  (defun ~ercn-notify (nickname message)
    (alert (format "<b>%s</b>: %s" nickname message)
	   :icon "internet-chat"
	   :title (format "ERC: %s@%s" (buffer-name)
			  (erc-shorten-server-name
			   (or erc-server-announced-name
			       erc-session-server)))))
  (add-hook 'ercn-notify-hook '~ercn-notify))

(use-package erc-hl-nicks
  :after erc
  :config
  ;; Disable for default:
  (erc-hl-nicks-disable))

(use-package erc-image
  :after erc)

(use-package erc-view-log
  :after erc
  :config
  ;; Use `erc-log-channels-directory` to set the auto-mode for ERC log
  ;; files.
  (when (boundp 'erc-log-channels-directory)
    (add-to-list 'auto-mode-alist
		 `(,(format "%s.*\\.txt"
			    (regexp-quote
			     (file-name-as-directory
			      (expand-file-name
			       erc-log-channels-directory))))
		   . erc-view-log-mode))))

;; Load custom el files
(let* ((dir "~/.emacs.d/custom.d/")
       (dir (file-name-as-directory dir)))
  (if (file-directory-p dir)
      (mapc (lambda (file) (load-file file))
	    (directory-files dir t "\\.el$"))))

(provide 'init)
;;; init.el ends here
