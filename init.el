(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable))

;; don't know how to enable flycheck-mode with above method, so enable right now
(elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


(use-package ensime
	     :ensure t
	     :pin melpa-stable)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages
   (quote
    (julia-mode web-mode jinja2-mode flycheck elpy realgud php-mode git scss-mode django-snippets django-mode sass-mode json-mode typescript-mode docker-compose-mode dockerfile-mode yaml-mode ensime ecb magit cargo company racer slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq inferior-lisp-program "sbcl")

;; (add-to-list 'load-path "~/.emacs.d/mode/rust-mode")
;; (autoload 'rust-mode "rust-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq rust-format-on-save t)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(global-set-key (kbd "C-x g") 'magit-status)

(global-display-line-numbers-mode)

;; (require 'django-html-mode)
(require 'django-mode)
;; (add-to-list 'auto-mode-alist '("\\.dhtml$" . django-html-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
        

(add-hook 'web-mode-hook
          (lambda() (local-set-key (kbd "C-c /") #'web-mode-element-close)))

(add-hook 'elpy-mode-hook  ;; C-c C-/ is interpreted in emacs as C-c C-_
          (lambda() (local-set-key (kbd "C-c C-_") #'comment-or-uncomment-region)))
(require 'julia-mode)
(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))

;; (setq jedi:environment-root "jedi")
;; (setq jedi:environment-virtualenv
;;       (append python-environment-virtualenv
;;               '("--python" "/usr/local/bin/python3")))

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'django-mode 'jedi:setup)
;; (setq jedi:complete-on-dot t)

(add-to-list `load-path "~/.emacs.d/mode/vue-mode")
(autoload 'vue-mode "vue-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vue$" . vue-mode))


(global-set-key (kbd "C-x C-\\") 'set-input-method)
(define-key key-translation-map (kbd "C-c p") (kbd "π"))
(define-key key-translation-map (kbd "C-c a") (kbd "α"))
(define-key key-translation-map (kbd "C-c d") (kbd "̣Δ"))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt"))

(global-set-key (kbd "C-x g") 'magit-status)

