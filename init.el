;; -------------------------------
;; Basic Package Setup
;; -------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;disable splash screen and startup message
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; Install packages if missing
(dolist (pkg '(eglot company clang-format))
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; -------------------------------
;; Eglot Configuration
;; -------------------------------
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; Explicitly tell Eglot to use clangd
(setq eglot-server-programs
      '((c-mode . ("clangd"))
        (c++-mode . ("clangd"))))

;; Make Eglot a bit more responsive
(setq eglot-send-changes-idle-time 0.5)
(setq eglot-autoreconnect t)

;; -------------------------------
;; Company (Completion) Setup
;; -------------------------------
(add-hook 'after-init-hook 'global-company-mode)


;; Optional tweaks for smoother experience
(setq company-idle-delay 0.2        ;; how soon completion pops up
      company-minimum-prefix-length 1
      company-tooltip-align-annotations t)

;; No company-mode in shell-mode
(add-hook 'shell-mode-hook (lambda () (global-company-mode 0)))

;; -------------------------------
;; Clang-Format Integration
;; -------------------------------
(require 'clang-format)

(defun my-c-cpp-format-buffer ()
  "Format buffer with clang-format if available."
  (when (derived-mode-p 'c-mode 'c++-mode)
    (clang-format-buffer)))

;; Auto-format before saving C/C++ files
(add-hook 'before-save-hook 'my-c-cpp-format-buffer)

;; Optional: bind manual formatting to a key
(with-eval-after-load 'cc-mode
  (define-key c-mode-map  (kbd "C-c f") 'clang-format-buffer)
  (define-key c++-mode-map (kbd "C-c f") 'clang-format-buffer))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -------------------------------
;; Doom-Themes Config
;; -------------------------------

(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
