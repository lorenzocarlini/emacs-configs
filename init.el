;; -------------------------------
;; Basic Package Setup
;; -------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Disable splash screen and initial scratch message
(setq inhibit-startup-message t
      initial-scratch-message nil)

;; Ensure packages are installed
(dolist (pkg '(eglot company clang-format auctex pdf-tools doom-themes))
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))


;; -------------------------------
;; AUCTeX / LaTeX Configuration
;; -------------------------------
(require 'tex)
(require 'preview)

;; Enable PDF mode by default
(setq TeX-PDF-mode t
      TeX-process-asynchronous t
      TeX-save-query nil        ;; Don't ask to save buffers
      TeX-show-compilation nil    ;; Show compilation buffer
      TeX-command-run-all t     ;; Compile -> view -> bibtex automatically
      TeX-command-default "LaTeX")

;; Preview settings
(add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
(setq preview-image-type 'png)



;; PDF Tools setup
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-start-server t)
  ;; Auto-refresh PDF after compilation
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

;; Automatically revert DocView buffers if used
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; Optional: force C-c C-c to compile & refresh without prompts
(defun my-latex-build-and-view ()
  "Compile LaTeX and refresh PDF buffer without prompts."
  (interactive)
  (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file)
  (TeX-revert-document-buffer))
(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c C-c") 'my-latex-build-and-view))

;; -------------------------------
;; Eglot (LSP) Configuration
;; -------------------------------
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(setq eglot-server-programs
      '((c-mode . ("clangd"))
        (c++-mode . ("clangd"))))
(setq eglot-send-changes-idle-time 0.5
      eglot-autoreconnect t)

;; -------------------------------
;; Company (Completion) Setup
;; -------------------------------
(add-hook 'after-init-hook 'global-company-mode)

(setq company-idle-delay 0.2
      company-minimum-prefix-length 1
      company-tooltip-align-annotations t)

;; Disable company in shell-mode
(add-hook 'shell-mode-hook (lambda () (company-mode -1)))

;; -------------------------------
;; Clang-Format Integration
;; -------------------------------
(require 'clang-format)

(defun my-c-cpp-format-buffer ()
  "Format buffer with clang-format if in C/C++ mode."
  (when (derived-mode-p 'c-mode 'c++-mode)
    (clang-format-buffer)))

;; Auto-format before saving C/C++ files
(add-hook 'before-save-hook 'my-c-cpp-format-buffer)

;; Optional: manual formatting keybinding
(with-eval-after-load 'cc-mode
  (define-key c-mode-map  (kbd "C-c f") 'clang-format-buffer)
  (define-key c++-mode-map (kbd "C-c f") 'clang-format-buffer))


;; -------------------------------
;; Python Mode and Language Support (Lazy Loaded)
;; -------------------------------

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook ((python-mode . eglot-ensure)
         (python-mode . company-mode)
         (python-mode . eldoc-mode)
         (python-mode . my-auto-activate-venv)
         (python-mode . display-line-numbers-mode)
         (python-mode . python-black-on-save-mode))
  :init
  ;; Ensure supporting packages are installed only when needed
  (use-package python-black :ensure t :defer t)
  (use-package pyvenv :ensure t :defer t)
  (use-package flycheck :ensure t :defer t)
  (use-package yasnippet :ensure t :defer t)
  :config
  ;; -------------------------------
  ;; Eglot LSP
  ;; -------------------------------
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp"))) ;; or pyright-langserver

  ;; -------------------------------
  ;; Virtual Environment Handling
  ;; -------------------------------
  (require 'pyvenv)
  (pyvenv-mode 1)
  (setq pyvenv-workon "default")

  (defun my-auto-activate-venv ()
    "Automatically activate .venv if present in project."
    (let ((venv-path (locate-dominating-file default-directory ".venv")))
      (when venv-path
        (pyvenv-activate (expand-file-name ".venv" venv-path)))))

  ;; -------------------------------
  ;; Code Formatting (Black)
  ;; -------------------------------
  (require 'python-black)
  (setq python-black-command "black"
        python-black-extra-args '("--line-length" "88"))

  (with-eval-after-load 'python
    (define-key python-mode-map (kbd "C-c f") 'python-black-buffer))

  ;; -------------------------------
  ;; Syntax Checking (Flycheck fallback)
  ;; -------------------------------
  (require 'flycheck)
  (add-hook 'eglot-managed-mode-hook
            (lambda () (flycheck-mode -1)))
  (add-hook 'python-mode-hook
            (lambda () (unless (eglot-managed-p)
                         (flycheck-mode 1))))

  ;; -------------------------------
  ;; Misc. Enhancements
  ;; -------------------------------
  (setq python-indent-offset 4
        indent-tabs-mode nil
        eldoc-idle-delay 0.3
        eldoc-echo-area-use-multiline-p t)
  (font-lock-add-keywords 'python-mode
                          '(("\\<\\(TODO\\|FIXME\\|NOTE\\):" 1 font-lock-warning-face t))))


;; -------------------------------
;; Doom-Themes Configuration
;; -------------------------------
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-atom")
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; -------------------------------
;; Custom-set Variables / Faces
;; -------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons doom-modeline yasnippet pyvenv python-black pdf-tools flycheck doom-themes company clang-format auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -------------------------------
;; Doom Modeline - Portable (No Icon Fonts)
;; -------------------------------
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  ;; Disable all fancy fonts/icons for portability
  (doom-modeline-icon nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-env-version nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-bar-width 1)
  (doom-modeline-height 18)
  (doom-modeline-minor-modes nil))
