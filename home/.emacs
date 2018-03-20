;;; .emacs --- Summary
;; Emacs configuration
;;
;;; Commentary:
;;
;; To do:
;; - Auto spell checking when entering prog-mode
;; - Improve auto-completion
;; - Org mode and markdown
;; - Code navigation
;; - Snippets (git, etc)

;;; Code:

;; Suppress welcome screen
(setq inhibit-startup-screen t)

;; Hide tool and menu bars and use reasonable font size
(if (display-graphic-p)
    (progn
      (scroll-bar-mode 0)
      (tool-bar-mode 0)))
(menu-bar-mode 0)
(set-face-attribute 'default nil :height 110)

;; Sane defaults
(show-paren-mode 1)           ; Highlight matching parenthesis
(column-number-mode 1)        ; Show column number with line number
(cua-mode 1)                  ; Regular Ctrl-C/Ctrl-X/Ctrl-V
(setq vc-follow-symlinks t)   ; Follow symlinks under version control
(prefer-coding-system 'utf-8) ; Use utf8 by default
(fset 'yes-or-no-p 'y-or-n-p) ; Lazy prompt
(savehist-mode)               ; Persistent history
(xterm-mouse-mode)            ; Support mouse inside terminal
(global-hl-line-mode 1)       ; Highlight current line
(setq scroll-step 1)          ; Scroll one line at a time
(setq scroll-margin 5)        ; Show N lines at the edge when scrolling
(setq                         ; Scroll one line at a time
 mouse-wheel-scroll-amount
 '(1 ((shift) . 1)))
(setq                         ; Don't accelerate scrolling
 mouse-wheel-progressive-speed
 nil)
(setq                         ; Scroll window under mouse
 mouse-wheel-follow-mouse 't)

;; ediff options
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "")

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
(require 'cc-mode)
(setq-default indent-tabs-mode t)
(setq backward-delete-char-untabify-method 'hungry)
(setq tab-width 8)
(setq c-block-comment-prefix "* ")
(defvaralias 'c-indent 'tab-width)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'sh-indentation 'tab-width)
(defvaralias 'sh-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq c-default-style '((other . "linux")))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make ibuffer default. This way it's possible to switch buffers with "C-x C-b".
(defalias 'list-buffers 'ibuffer)

;; Mutt specific configurations
(defun my:mutt-mode()
  "Mutt mode"
  (interactive)
  (progn
    (mail-mode)
    (auto-fill-mode 1)
    (whitespace-mode -1)
    (re-search-forward "^$")
    (forward-char 1)))
(add-to-list 'auto-mode-alist '("/mutt" . my:mutt-mode))

;; Shortcuts
;;------------------------------------------------------------------------------

;; Kills emacs server
(global-set-key (kbd "C-x Q") 'kill-emacs)

;; Cycle between the last two buffers
(global-set-key (kbd "M-o")  'mode-line-other-buffer)

;; Kill line backwards
;; Use C-u u to delete from cursor to beginning of line. Similarly to C-u used by vim and bash
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key (kbd "C-c u") 'backward-kill-line)

;; Package manager
;;==============================================================================
;; M-x package-refresh-contents RET
;; M-x package-install RET evil RET
(require 'package)
(setq package-archives
      '(("marmalade"    . "https://marmalade-repo.org/packages/")
	("melpa"        . "https://melpa.org/packages/") ; Assume ssl
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("gnu"          . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Auto install mechanism
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)

;; Hide modes from status bar (used it use-package)
(use-package diminish :ensure t)

;; zerodark-theme
;; This theme uses all-the-icons.el which needs "Ubuntu mono"
;; to work in the terminal.
(use-package zerodark-theme
  :ensure t
  :config
  (load-theme 'zerodark t)
  (if (not (display-graphic-p))
      (set-face-attribute
       'hl-line nil
       :background "gray22"
       :foreground nil
       :bold t))
  (zerodark-setup-modeline-format))

;; Extensible vi layer
(use-package evil
  :ensure t
  :demand
  :bind
  ((:map evil-window-map
	 ("<left>"  . evil-window-left)
	 ("<down>"  . evil-window-down)
	 ("<up>"    . evil-window-up)
	 ("<right>" . evil-window-right))
   (:map evil-normal-state-map
	 ("g T"     . previous-buffer)
	 ("g t"     . next-buffer)
	 ("C-e"     . move-end-of-line)))
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
  :config
  (evil-mode 1))

;; PCRE regular expression style
;; Use `pcre-mode' to enable it.
(use-package pcre2el
  :ensure t)

;; Efficiently add numbers to lines
(use-package nlinum
  :ensure t
  :diminish
  :hook
  (((text-mode prog-mode) . nlinum-mode))
  :config
  (if (display-graphic-p)
      (setq nlinum-format "%4d")
    (setq nlinum-format "%4d\u2502")))

;; Highlight trailing spaces
(use-package whitespace
  :ensure t
  :diminish
  :hook
  (((text-mode prog-mode) . whitespace-mode))
  :config
  (setq whitespace-line-column 78)
  (setq whitespace-style
	'(face         ;
	  trailing     ; Trailing blanks
	  lines-tail)  ; Lines with columns beyond whitespace-line-column
	))

(use-package smart-backspace
  :ensure t
  :diminish
  :config
  ;; (global-set-key [?\C-?] 'smart-backspace)
  (define-key evil-insert-state-map [?\C-?] 'smart-backspace))

(use-package smartparens-config
  :ensure smartparens
  :config
  (smartparens-global-mode))

;; ivy, swiper and counsel - Better "M-x", "C-s" and "C-x f"
(use-package ivy
  :ensure t
  :demand
  :diminish
  :bind
  ((:map ivy-minibuffer-map
	 ([escape] . minibuffer-keyboard-quit))
   )
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :diminish
  :bind
  (("C-s"     . swiper)))

(use-package counsel
  :ensure t
  :diminish
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c f"   . counsel-git)
   ("C-c C-f" . counsel-git)
   ("C-c s"   . counsel-git-grep)
   ("C-c C-s" . counsel-git-grep)
   ("C-c /"   . counsel-ag)
   ("C-c o"   . counsel-find-file-extern)
   ("C-S-s"   . counsel-ag)
   ("C-c l"   . counsel-locate)))

;; Show hints about shortcuts
(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-prefix-then-key-order
	which-key-popup-type 'side-window
	which-key-side-window-max-height 0.5
	which-key-side-window-max-width 0.33
	which-key-idle-delay 0.5
	which-key-min-display-lines 7))

;; Highlight symbol under the cursor
(use-package highlight-symbol
  :ensure t
  :diminish
  :hook
  ((prog-mode . highlight-symbol-mode))
  :config
  (setq highlight-symbol-idle-delay 0.1))

;; Select based on context
(use-package expand-region
  :ensure t
  :bind
  (("C-@" . er/expand-region)
   (:map evil-insert-state-map ("C-@" . er/expand-region))
   (:map evil-normal-state-map ("C-@" . er/expand-region))
   (:map evil-visual-state-map ("C-@" . er/expand-region))))

;; Edit multiple occurrences of a symbol at the same time
;; todo: try multiple-cursors.el agan.
(use-package iedit
  :ensure t
  :demand
  ;;(add-hook 'iedit-mode-hook '(lambda () (which-key-show-keymap 'iedit-mode-keymap)))
  :hook ((iedit-mode . (lambda () (which-key-show-keymap 'iedit-mode-keymap))))
  :bind
  (("C-c E" . iedit-mode)
   ("C-c e" . iedit-local-mode)
   (:map evil-insert-state-map
	 ("C-c E" . iedit-mode)
	 ("C-c e" . iedit-local-mode))
   (:map evil-normal-state-map
	 ("C-c E" . iedit-mode)
	 ("C-c e" . iedit-local-mode))
   (:map evil-visual-state-map
	 ("C-c E" . iedit-mode)
	 ("C-c e" . iedit-local-mode)))
  :config
  (defun iedit-local-mode()
    "iedit-mode on the current function."
    (interactive)
    (iedit-mode 0)))

;; Snippets
(use-package yasnippet-snippets :ensure t)

(use-package yasnippet
  :ensure t
  :demand
  :config
  (yas-global-mode 1))

;; Spell checking
(use-package flyspell
  :diminish "FlyS"
  :hook
  ((text-mode prog-mode) . flyspell-smart-mode)
  :config
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))
  ;;(setq flyspell-issue-message-flag nil)
  (defun flyspell-visible-region()
    "Check spelling only on the visible region"
    (interactive)
    (flyspell-region (window-start) (window-end)))
  (defun flyspell-smart-mode()
    "Enable flyspell based on the current major mode."
    (interactive)
    (progn
      (if (derived-mode-p 'prog-mode)
	  (flyspell-prog-mode)
	(flyspell-mode 1))
      ;; fixme: workaround to avoid checking the entire buffer and blocking
      ;; It might be possible to call that after scrolling the buffer
      ;;(flyspell-buffer)
      ;;(flyspell-visible-region)
      )))

;; Syntax check
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

;; Aggressive indentation
;; Use `aggressive-indent-mode' to enable it.
(use-package aggressive-indent
  :ensure t
  :diminish "AI")

;; Git support
(use-package magit
  :ensure t
  :if (executable-find "git")
  :commands
  (magit-status magit-blame)
  :bind
  (("C-c g s" . magit-status)
   ("C-c g b" . magit-blame)
   ("C-c g l" . magit-log-current)
   ("C-c g L" . magit-log-all))
  :init
  (which-key-add-key-based-replacements "C-c g" "magit")
  :config
  (use-package evil-magit
    :ensure t
    :demand
    :after (:all evil magit)))

;; Auto complete
(use-package company
  :ensure t
  :diminish "Company"
  :commands (company-mode)
  :hook
  ((prog-mode . company-mode))
  :config
  ;; Time before completion starts
  (setq company-idle-delay 0.1)
  ;; Avoid triggering the generation of tag completion table (that blicks
  ;; the UI and might take very long on large projects).
  (setq company-etags-use-main-table-list nil)
  ;; The minimum prefix length for idle completion.
  (setq company-minimum-prefix-length 3)
  ;; Allow user to type a value that is not listed in the completion.
  (setq company-require-match nil))

;; clang based completion server
(use-package irony
  :ensure t
  :hook
  (((c++-mode c-mode) . irony-mode)
   (irony-mode        . irony-cdb-autosetup-compile-options)))

;; company integration
(use-package company-irony
  :ensure t
  :after (:all company irony)
  :config
  (add-to-list 'company-backends 'company-irony))

;; C headers completion based on the irony server
(use-package company-irony-c-headers
  :ensure t
  :after (:all company-irony)
  :config
  (add-to-list
   'company-backends
   '(company-irony-c-headers company-irony)))

;; Basic support for etags if gtags is not available:
(use-package etags :ensure t
  :config
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil))

(use-package counsel-etags :ensure t)

;; counsel-gtags completely replaces ggtags.el and offers better
;; support for creating and updating tags files.
(use-package counsel-gtags
  :ensure t
  :after (:all evil counsel)
  :hook
  ((prog-mode . counsel-gtags-mode))
  :bind
  (
   ;; Emulate etags/ctags jump
   ("C-]" . counsel-gtags-dwim)
   (:map evil-normal-state-map ("C-]" . counsel-gtags-dwim))
   (:map evil-insert-state-map ("C-]" . counsel-gtags-dwim))
   ;; Emulate etags/ctags return to previous location
   ("C-t" . counsel-gtags-go-backward)
   (:map evil-normal-state-map ("C-t" . counsel-gtags-go-backward))
   (:map evil-insert-state-map ("C-t" . counsel-gtags-go-backward))))

;; Code and project navigation
(use-package projectile
  :ensure t
  :after (:all counsel-etags)
  :commands (projectile-mode)
  :hook
  ((prog-mode . projectile-mode))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; which-key prefix title
  (which-key-add-key-based-replacements "C-c p" "Projectile")
  :config
  (projectile-mode))

(provide '.emacs)
;;; .emacs ends here
