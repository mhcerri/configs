;;
;; Emacs configuration
;;

;; TODOs:
;; - spell
;; - auto-completion
;; - better buffer switching
;; - mouse
;; - org mode and markdown
;; - linters
;; - avoid some questions, ie: "Symbolic link to Git-controlled source file; follow link? (yes or no)"
;; - or maybe ensure default actions for those questions
;; - Snippets (git, etc)

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

;; Shotcuts

;; Kills emacs server
(global-set-key (kbd "C-c C-c") 'kill-emacs)
(defun my-reload()
  "Load ~/.emacs again."
  (interactive)
  (load-file "~/.emacs"))

;; Re-load ~/.emacs
(global-set-key (kbd "C-c C-r") 'my-reload)
	
;; Kill line backwards
;; Use C-u u to delete from cursor to beginning of line. Similarly to C-u used by vim and bash 
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key (kbd "C-c u") 'backward-kill-line)

;; Package manager
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

;; Evil mode
(use-package evil
  :init (evil-mode 1)
  :ensure t)
