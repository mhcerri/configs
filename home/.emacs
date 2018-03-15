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
    (tool-bar-mode 0))
(menu-bar-mode 0)
(scroll-bar-mode 0)
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
    (whitespace-mode -1)
    (re-search-forward "^$")
    (forward-char 1)))
(add-to-list 'auto-mode-alist '("/mutt" . my:mutt-mode))

;; Shortcuts
;;------------------------------------------------------------------------------

;; Kills emacs server
(global-set-key (kbd "C-c C-c") 'kill-emacs)

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
(use-package zerodark-theme :ensure t
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
(use-package evil :ensure t :demand
  :init
  ;; Use default emacs bindings for insert vim mode
  (setq-default evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)
  ;; Use evil mode eveywhere
  (setq evil-emacs-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)
  :bind
  ((:map evil-window-map
	 ("<left>"  . evil-window-left)
	 ("<down>"  . evil-window-down)
	 ("<up>"    . evil-window-up)
	 ("<right>" . evil-window-right))
   (:map evil-normal-state-map
	 ("<backtab>" . previous-buffer)
	 ("TAB"       . next-buffer)
	 ("C-e"       . move-end-of-line))))

;; Efficiently add numbers to lines
(use-package nlinum :ensure t
  :config
  (if (display-graphic-p)
      (setq nlinum-format "%4d")
    (setq nlinum-format "%4d\u2502"))
  :hook
  (((text-mode prog-mode) . nlinum-mode)))

;; Highlight trailing spaces
(use-package whitespace :ensure t
  :config
  (setq whitespace-line-column 78)
  (setq whitespace-style
	'(face         ;
	  trailing     ; Trailing blanks
	  lines-tail)) ; Lines with columns beyond whitespace-line-column
  :hook
  (((text-mode prog-mode) . whitespace-mode)))

(use-package smart-backspace :ensure t
  :config
  ;; (global-set-key [?\C-?] 'smart-backspace)
  (define-key evil-insert-state-map [?\C-?] 'smart-backspace))

(use-package smartparens-config :ensure smartparens
  :config
  (smartparens-global-mode))

;; ivy, swiper and counsel - Better "M-x", "C-s" and "C-x f"
(use-package ivy :ensure t
  :demand
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1)
  :bind
  ((:map ivy-minibuffer-map
	 ([escape] . minibuffer-keyboard-quit))
   ;;   ("C-x b"   . ivy-switch-buffer)))
   ))

(use-package swiper :ensure t
  :bind
  (("C-s"     . swiper)))

(use-package counsel :ensure t
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
(use-package which-key :ensure t
  :diminish which-key-mode
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
(use-package highlight-symbol :ensure t
  :config
  (setq highlight-symbol-idle-delay 0.1)
  :hook
  ((prog-mode . highlight-symbol-mode)))

;; Spell checking
(use-package flyspell
  :config
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))
  (setq flyspell-issue-message-flag nil)
  (defun my:enable-flyspell()
    "Enable flyspell based on the current major mode."
    (interactive)
    (progn
      (if (derived-mode-p 'prog-mode)
	  (flyspell-prog-mode)
	(flyspell-mode 1))
      (flyspell-buffer)))
  :hook
  ((find-file text-mode prog-mode) . my:enable-flyspell))

;; Syntax check
(use-package flycheck :ensure t
  :config
  (global-flycheck-mode))

;; Aggressive indentation
(use-package aggressive-indent :ensure t
  :config
  (aggressive-indent-global-mode 1))

;; Auto complete
(use-package company :ensure t
  :config
  (setq company-idle-delay 0.1)
  :hook
  ((prog-mode . company-mode)))

(use-package irony :ensure t
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  :hook
  (((c++-mode c-mode objc-mode) . irony-mode)
   (irony-mode                  . my-irony-mode-hook)
   (irony-mode                  . irony-cdb-autosetup-compile-options)))

(use-package company-irony :ensure t
  :after (:all company irony))

(provide '.emacs)
;;; .emacs ends here
