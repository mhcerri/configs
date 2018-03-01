;;
;; Emacs configuration
;;

;; TODOs:
;; - spell
;; - auto-completion
;; - mouse
;; - org mode and markdown
;; - linters
;; - Snippets (git, etc)


;; General
;;==============================================================================

;; Theme
(load-theme 'tango)

;; Suppress welcome screen
(setq inhibit-startup-screen t)

;; Hide tool and menu bars
(if (display-graphic-p)
    (tool-bar-mode -1))
(if (not (display-graphic-p))
    (menu-bar-mode -1))

;; Sane defaults
(show-paren-mode 1) ; Highlight matching parenthesis
(column-number-mode 1) ; Show column number with line number

;; Line number
(global-linum-mode nil)
(if (display-graphic-p)
    (setq linum-format "%4d ")
  (setq linum-format "%4d\u2502 "))

;; Follow symlinks under version control
(setq vc-follow-symlinks 't)

;; Remember cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; Default indentation settings
(setq-default indent-tabs-mode t)
(setq tab-width 8)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq c-default-style "linux")

;; Make ibuffer default. This way it's possible to switch buffers with "C-x C-b".
(defalias 'list-buffers 'ibuffer)

;; ido-mode
;; Further improvements with ido-vertical-mode bellow
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys ()
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map (kbd "TAB") 'ido-next-match))
  (add-hook 'ido-setup-hook 'ido-define-keys)

;; Shotcuts
;;------------------------------------------------------------------------------

;; Kills emacs server
(global-set-key (kbd "C-c C-c") 'kill-emacs)

;; Re-load ~/.emacs
(defun my-reload()
  "Load ~/.emacs again."
  (interactive)
  (load-file "~/.emacs"))
(global-set-key (kbd "C-c C-r") 'my-reload)

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
(setq package-archives '(("melpa"     . "https://melpa.org/packages/") ; Assume ssl
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("gnu"       . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Auto install mechanism
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)

;; evil - eVIl mode
(use-package evil :ensure t
  :config
  (evil-mode 1)
  ;; C-w <arrow> bindings
  (define-key evil-window-map (kbd "<left>")  'evil-window-left)
  (define-key evil-window-map (kbd "<down>")  'evil-window-down)
  (define-key evil-window-map (kbd "<up>")    'evil-window-up)
  (define-key evil-window-map (kbd "<right>") 'evil-window-right)
  (global-set-key (kbd "C-*") 'evil-search-symbol-forward)
  (global-set-key (kbd "C-#") 'evil-search-symbol-backward))

;; ido-vertical-mode - Better buffer switching
(use-package ido-vertical-mode :ensure t
  :config
  (setq ido-use-faces t)
  (set-face-attribute 'ido-vertical-first-match-face nil
		      :background "#e5b7c0")
  (set-face-attribute 'ido-vertical-only-match-face nil
		      :background "#b00000"
		      :foreground "white")
  (set-face-attribute 'ido-vertical-match-face nil
		      :foreground "#b00000")
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (ido-vertical-mode 1))

;; councel - Better M-x and "C-x f"
(use-package counsel :ensure t
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c f"   . counsel-git)
   ("C-c s"   . counsel-git-grep)
   ("C-c /"   . counsel-ag)
   ("C-c o"   . counsel-find-file-extern)
   ("C-S-s"   . counsel-ag)
   ("C-c l"   . counsel-locate)))

;; which-key - Show hints about shortcuts
(use-package which-key :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-prefix-then-key-order
	which-key-popup-type 'side-window
	which-key-side-window-max-height 0.5
	which-key-side-window-max-width 0.33
	which-key-idle-delay 0.5
	which-key-min-display-lines 7))
