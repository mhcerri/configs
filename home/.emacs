;;; .emacs --- Summary
;; Emacs configuration
;;
;;; Commentary:
;;
;; To do:
;; - Better spell checking
;; - Org mode and markdown
;; - Text-mode for files without extension (ie README)
;; - Email

;;; Code:

;; Suppress welcome screen
(setq inhibit-startup-screen t)

;; Hide tool and menu bars and use reasonable font size
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
(menu-bar-mode 0)
(set-face-attribute 'default nil :height 105)

;; Sane defaults
(show-paren-mode 1)                     ; Highlight matching parenthesis
(column-number-mode 1)                  ; Show column number with line number
(cua-mode 1)                            ; Regular Ctrl-C/Ctrl-X/Ctrl-V
(setq vc-follow-symlinks t)             ; Follow symlinks under version control
(prefer-coding-system 'utf-8)           ; Use utf8 by default
(fset 'yes-or-no-p 'y-or-n-p)           ; Lazy prompt
(savehist-mode)                         ; Persistent history
(xterm-mouse-mode)                      ; Support mouse inside terminal
(global-hl-line-mode 1)                 ; Highlight current line
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
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make ibuffer default. This way it's possible to switch buffers with "C-x C-b".
(require 'ibuffer)
(setq ibuffer-expert t) ; Do not ask to delete buffer
(setq-default ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
(defalias 'list-buffers 'ibuffer)

;; Custom initial scratch message
(defvar initial-scratch-file "~/.emacs.d/scratch")
(if (file-exists-p initial-scratch-file)
    (setq initial-scratch-message
	  (with-temp-buffer
	    (insert-file-contents initial-scratch-file)
	    (buffer-string))))

;; Mutt specific configurations
(defun my:mutt-mode()
  "Mutt mode"
  (interactive)
  (progn
    (mail-mode)
    (orgstruct++-mode)
    (orgtbl-mode)
    (auto-fill-mode 1)
    (whitespace-mode -1)
    (re-search-forward "^$")
    (forward-char 1)))
(add-to-list 'auto-mode-alist '("/mutt" . my:mutt-mode))

;; Shortcuts
;;------------------------------------------------------------------------------

;; Kills emacs server
(global-set-key (kbd "C-x Q") 'save-buffers-kill-emacs)

;; Cycle between the last two buffers
(global-set-key (kbd "M-o")  'mode-line-other-buffer)

;; Kill line backwards
;; Use C-u u to delete from cursor to beginning of line, similarly
;; to C-u used by vim and bash/readline.
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key (kbd "C-c u") 'backward-kill-line)

;; Package manager
(require 'package)
(setq package-archives
      '(("marmalade"    . "https://marmalade-repo.org/packages/")
	("melpa"        . "https://melpa.org/packages/") ; Assume ssl
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("gnu"          . "https://elpa.gnu.org/packages/")
	("org"          . "http://orgmode.org/elpa/")))
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
       :foreground nil))
  (setq zerodark-use-paddings-in-mode-line nil)
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
  ;; Make TAB work with evil-org
  (setq evil-want-C-i-jump nil)
  ;; Required for evil-collection
  (setq evil-want-integration nil)
  ;; Esc hook support
  (defvar evil-esc-hook '(t)
        "A hook run after ESC is pressed in normal mode.")
  (defun evil--attach-escape-hook ()
    "Run the `evil-esc-hook'."
    (cond
     ((minibuffer-window-active-p (minibuffer-window))
      ;; Quit the minibuffer if open
      (abort-recursive-edit))
     ((evil-ex-hl-active-p 'evil-ex-search)
      ;; Disable ex search buffer highlights
      (evil-ex-nohighlight))
     (t
      ;; Run all escape hooks. If any returns non-nil, then stop there.
      (run-hook-with-args-until-success 'evil-esc-hook))))
  (advice-add #'evil-force-normal-state
	      :after #'evil--attach-escape-hook)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :load-path "~/.emacs.d/evil-collection"
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (evilnc-default-hotkeys))

;; PCRE regular expression style
;; Use `pcre-mode' to enable it.
(use-package pcre2el
  :ensure t)

;; Async
(use-package async
  :ensure t
  :config
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

;; Add numbers to lines
;; nlinum might be efficient but it doesn't play well with git-gutter.
(use-package linum
  :ensure t
  :hook
  (((text-mode prog-mode) . my:linum-hook))
  :config
  ;; Avoid performance issues
  (defun my:is-buffer-too-big ()
    "Check buffer size"
    (or (> (buffer-size) (* 10000 80))
	(> (line-number-at-pos (point-max)) 10000)))
  (defun my:linum-hook ()
    "Disable linum if buffer is too big"
    (if (my:is-buffer-too-big)
	(progn
	  (message "Buffer is too big! Disabling line numbers...")
	  (linum-mode -1))
      (linum-mode 1)
      ))
  ;; Use separator
  (setq linum-format "%4d\u2502")
  ;; Hide fringe in gui mode for consistency
  (add-to-list 'default-frame-alist '(left-fringe . 0))
  (add-to-list 'default-frame-alist '(right-fringe . 0))
  (setq-default left-fringe-width 0))

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

;; Control how popups are handled
(use-package popwin
  :ensure t
  :config
  (setq popwin:popup-window-position 'bottom)
  (push '("*Warnings*" :noselect t)
	popwin:special-display-config)
  ;; Close popups with ESC
  (add-hook 'evil-esc-hook #'popwin:close-popup-window)
  (popwin-mode 1))

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
  (("<M-SPC>" . er/expand-region)
   (:map evil-insert-state-map ("<M-SPC>" . er/expand-region))
   (:map evil-normal-state-map ("<M-SPC>" . er/expand-region))
   (:map evil-visual-state-map ("<M-SPC>" . er/expand-region))))

;; Editable grep buffers
(use-package wgrep
  :ensure t)

;; Edit multiple occurrences of a symbol at the same time
;; todo: try multiple-cursors.el again.
(use-package iedit
  :ensure t
  :config
  (if (display-graphic-p)
      (set-face-attribute 'iedit-occurrence nil :box t)
    (set-face-attribute 'iedit-occurrence nil :background "magenta")))

(use-package evil-iedit-state
  :ensure t
  :after (:all iedit)
  :bind
  ;; Define key bindings here since it's necessary to
  ;; use evil-iedit-state/iedit-mode instead
  (("C-c E" . evil-iedit-state/iedit-mode)
   ("C-c e" . iedit-local-mode)
   (:map evil-iedit-state-map
	 ([backspace] . nil)
	 ("t"         . iedit-toggle-selection))
   (:map evil-insert-state-map
	 ("C-c E" . evil-iedit-state/iedit-mode)
	 ("C-c e" . iedit-local-mode))
   (:map evil-normal-state-map
	 ("C-c E" . evil-iedit-state/iedit-mode)
	 ("C-c e" . iedit-local-mode))
   (:map evil-visual-state-map
	 ("C-c E" . evil-iedit-state/iedit-mode)
	 ("C-c e" . iedit-local-mode)))
  :config
  (defun iedit-local-mode()
    "iedit-mode on the current function."
    (interactive)
    (evil-iedit-state/iedit-mode 0)))

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
  :after (:all flyspell-lazy)
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
  (magit-commit-popup
   magit-status
   magit-blame
   magit-log-current
   magit-log-all)
  :bind
  (("C-c g c" . magit-commit-popup)
   ("C-c g s" . magit-status)
   ("C-c g b" . magit-blame)
   ("C-c g l" . magit-log-current)
   ("C-c g L" . magit-log-all))
  :init
  ;; Set YASnippet mode
  (defun my:git-commit-mode ()
	"Run when entering git-commit mode"
	(interactive)
	(when (derived-mode-p 'text-mode)
	  (yas-activate-extra-mode 'text-mode+git-commit-mode)))
  (add-hook 'git-commit-setup-hook 'my:git-commit-mode)
  ;; Binding hint
  (which-key-add-key-based-replacements "C-c g" "magit"))

(use-package evil-magit
  :ensure t
  :after (:all evil magit)
  :config
  ;; evil-magit fails to convert popups, that's a workaround for that:
  (evil-set-initial-state 'magit-popup-mode 'insert))

(use-package git-gutter
  :ensure t
  :demand
  :diminish
  :bind
  (("C-c ="   . git-gutter:popup-hunk)
   ("C-c g h" . git-gutter:popup-hunk)
   ("C-c g n" . git-gutter:next-hunk)
   ("C-c g p" . git-gutter:previous-hunk)
   ("C-c g S" . git-gutter:stage-hunk)
   ("C-c g R" . git-gutter:revert-hunk))
  :config
  (setq git-gutter:update-interval 0)
  (global-git-gutter-mode 1)
  (git-gutter:linum-setup))

;; Auto complete
(use-package company
  :ensure t
  :diminish "Company"
  :commands (company-mode)
  :hook
  ((after-init . global-company-mode))
  :config
  (company-mode)
  ;; Time before completion starts
  (setq company-idle-delay 0.1)
  ;; Avoid triggering the generation of tag completion table (that bricks
  ;; the UI and might take very long on large projects).
  (setq-default company-etags-use-main-table-list nil)
  ;; The minimum prefix length for idle completion.
  (setq company-minimum-prefix-length 3)
  ;; Allow user to type a value that is not listed in the completion.
  (setq company-require-match nil)
  ;; Trigger auto completion on some characters
  (setq company-auto-complete t))

(use-package company-statistics
  :ensure t
  :config
  (company-statistics-mode 1))

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
  ;; Don't ask before re-reading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil))

(use-package counsel-etags :ensure t)

;; counsel-gtags completely replaces ggtags.el and offers better
;; support for creating and updating tags files.
(use-package counsel-gtags
  :ensure t
  :after (:all evil counsel)
  :diminish "Gtags"
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

;; Tag-like jump without any support
(use-package dumb-jump
  :ensure t
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
  (setq dumb-jump-selector 'ivy))

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
  (which-key-add-key-based-replacements "C-c p" "Projectile")
  :config
  (setq projectile-mode-line
	'(:eval (format " Proj[%s]" (projectile-project-name))))
  (projectile-mode))

(use-package ibuffer-projectile
  :ensure t
  :after (:all ibuffer projectile)
  :hook
  ((ibuffer . ibuffer-projectile-set-filter-groups)))

;; Better embedded terminal
(use-package multi-term
  :ensure t
  :commands
  (multi-term
   multi-term-next
   multi-term-prev)
  :bind
  (("C-c t" . multi-term))
  :config
  (setq multi-term-program (getenv "SHELL"))
  (setq multi-term-switcg-after-close 'PREVIOUS))

;; org mode
(use-package org
  :ensure org-plus-contrib
  :defer t
  :mode ("\\.org$" . org-mode)
  :config
  ;; Auto-complete
  ;; https://emacs.stackexchange.com/questions/21171/company-mode-completion-for-org-keywords
  (defun org-keyword-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'org-keyword-backend))
      (prefix (and (eq major-mode 'org-mode)
		   (cons (company-grab-line "^#\\+\\(\\w*\\)" 1)
			 t)))
      (candidates (mapcar #'upcase
			  (cl-remove-if-not
			   (lambda (c) (string-prefix-p arg c))
			   (pcomplete-completions))))
      (ignore-case t)
      (duplicates t)))
  (add-to-list 'company-backends 'org-keyword-backend))

(use-package evil-org
  :ensure t
  :after (:all evil org)
  :hook
  ((org-mode      . evil-org-mode)
   (evil-org-mode . evil-org-set-key-theme)))

;; GoLang
(use-package go-mode
  :ensure t)
(use-package company-go
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-go))

(provide '.emacs)
;;; .emacs ends here
