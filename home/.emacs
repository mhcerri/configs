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
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
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
