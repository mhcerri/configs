;;; early-init.el --- Early initialization
;;; Commentary:

;;; Code:
(setq inhibit-default-init t)
(setq gc-cons-threshold most-positive-fixnum)
(package-initialize)
(setq-default ~package-initialized t)

(provide 'early-init)
;;; early-init.el ends here
