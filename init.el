;; -------------------------------
;; Basic Emacs Setup
;; -------------------------------
(setq inhibit-startup-message t
      initial-scratch-message nil)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; -------------------------------
;; General Utilities
;; -------------------------------
(global-display-line-numbers-mode 1)
(setq-default indent-tabs-mode nil)

;; -------------------------------
;; Company
;; -------------------------------
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  :config
  (add-hook 'shell-mode-hook (lambda () (company-mode -1))))

;; -------------------------------
;; Eglot (LSP)
;; -------------------------------
(use-package eglot
  :hook ((c-mode c++-mode python-mode) . eglot-ensure)
  :custom
  (eglot-send-changes-idle-time 0.5)
  (eglot-autoreconnect t)
  :config
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp"))))

;; -------------------------------
;; Clang-Format
;; -------------------------------
(use-package clang-format
  :defer t
  :hook (before-save . my-c-cpp-format-buffer)
  :config
  (defun my-c-cpp-format-buffer ()
    "Format buffer with clang-format if in C/C++ mode."
    (when (derived-mode-p 'c-mode 'c++-mode)
      (clang-format-buffer)))
  :bind (:map c-mode-map ("C-c f" . clang-format-buffer)
              :map c++-mode-map ("C-c f" . clang-format-buffer)))

;; -------------------------------
;; AUCTeX / LaTeX
;; -------------------------------
(use-package tex
  :ensure auctex
  :defer t
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq TeX-PDF-mode t
        TeX-process-asynchronous t
        TeX-save-query nil
        TeX-show-compilation nil
        TeX-command-default "LaTeX"))

(use-package pdf-tools
  :defer t
  :config
  (pdf-tools-install)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (add-hook 'doc-view-mode-hook 'auto-revert-mode))

(defun my-latex-build-and-view ()
  "Compile LaTeX and refresh PDF buffer without prompts."
  (interactive)
  (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file)
  (TeX-revert-document-buffer))
(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c C-c") 'my-latex-build-and-view))

;; -------------------------------
;; Python
;; -------------------------------
(use-package pyvenv :defer t)
(use-package python-black :hook (python-mode . python-black-on-save-mode))
(use-package flycheck :defer t)

;; Ensure Emacs uses Python 3 globally
(let ((python3-bin (or (executable-find "python3")
                       (executable-find "python"))))
  (when python3-bin
    (setq python-shell-interpreter python3-bin)
    (add-to-list 'exec-path (file-name-directory python3-bin))
    (setenv "PATH" (concat (file-name-directory python3-bin) ":" (getenv "PATH")))))

(setq gud-pdb-command-name "python3 -m pdb")
(setq python-indent-offset 4)

(defun my-auto-activate-venv ()
  "Activate project .venv if present."
  (let ((venv-dir (locate-dominating-file default-directory ".venv")))
    (when venv-dir
      (pyvenv-activate (expand-file-name ".venv" venv-dir))
      (setq python-shell-interpreter
            (expand-file-name "bin/python" (expand-file-name ".venv" venv-dir)))
      (message "Activated virtualenv: %s" venv-dir))))

(defun my-python-setup ()
  "Configure Python environment."
  (my-auto-activate-venv)
  (company-mode 1)
  (eldoc-mode 1)
  ;; Highlight TODOs/FIXMEs
  (font-lock-add-keywords nil '(("\\<\\(TODO\\|FIXME\\|NOTE\\):" 1 font-lock-warning-face t)))
  ;; Keybinding for manual Black formatting
  (define-key python-mode-map (kbd "C-c f") 'python-black-buffer)
  ;; Disable flycheck in Eglot-managed buffers
  (add-hook 'eglot-managed-mode-hook (lambda () (flycheck-mode -1)) nil t)
  ;; Enable flycheck only if Eglot is not active
  (unless (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (flycheck-mode 1))
  ;; Start Eglot if pylsp exists
  (when (executable-find "pylsp")
    (eglot-ensure)))


(add-hook 'python-mode-hook 'my-python-setup)

;; Run Python file in bottom buffer
(defun my-run-python-file ()
  "Save, format with Black, and run the current Python file in a bottom buffer."
  (interactive)
  (save-buffer)
  (when (eq major-mode 'python-mode)
    (python-black-buffer))
  (let ((command (concat (or python-shell-interpreter "python3")
                         " "
                         (shell-quote-argument buffer-file-name))))
    (let ((display-buffer-alist
           '(("\\*Python Output\\*"
              (display-buffer-reuse-window display-buffer-at-bottom)
              (window-height . 15)))))
      (compilation-start command 'compilation-mode
                         (lambda (_) "*Python Output*")))))

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "<f5>") 'my-run-python-file)))

;; -------------------------------
;; Doom Themes
;; -------------------------------
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; -------------------------------
;; Doom Modeline (No Icon Fonts)
;; -------------------------------
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
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

;; -------------------------------
;; Custom Variables
;; -------------------------------
(custom-set-variables
 '(package-selected-packages
   '(doom-modeline yasnippet pyvenv python-black pdf-tools flycheck doom-themes company clang-format auctex)))
(custom-set-faces)
