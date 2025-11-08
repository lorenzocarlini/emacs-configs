;; -------------------------------
;; Basic Package Setup
;; -------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq inhibit-startup-message t
      initial-scratch-message nil)

(dolist (pkg '(eglot company clang-format auctex pdf-tools doom-themes))
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; -------------------------------
;; AUCTeX / LaTeX Configuration
;; -------------------------------
(require 'tex)
(require 'preview)

(setq TeX-PDF-mode t
      TeX-process-asynchronous t
      TeX-save-query nil
      TeX-show-compilation nil
      TeX-command-run-all t
      TeX-command-default "LaTeX")

(add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
(setq preview-image-type 'png)

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(defun my-latex-build-and-view ()
  "Compile LaTeX and refresh PDF buffer without prompts."
  (interactive)
  (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file)
  (TeX-revert-document-buffer))
(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c C-c") 'my-latex-build-and-view))

;; -------------------------------
;; Eglot Configuration
;; -------------------------------
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(setq eglot-server-programs
      '((c-mode . ("clangd"))
        (c++-mode . ("clangd"))))
(setq eglot-send-changes-idle-time 0.5
      eglot-autoreconnect t)

;; -------------------------------
;; Company Setup
;; -------------------------------
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 1
      company-tooltip-align-annotations t)
(add-hook 'shell-mode-hook (lambda () (company-mode -1)))

;; -------------------------------
;; Clang-Format Integration
;; -------------------------------
(require 'clang-format)

(defun my-c-cpp-format-buffer ()
  "Format buffer with clang-format if in C/C++ mode."
  (when (derived-mode-p 'c-mode 'c++-mode)
    (clang-format-buffer)))
(add-hook 'before-save-hook 'my-c-cpp-format-buffer)

(with-eval-after-load 'cc-mode
  (define-key c-mode-map  (kbd "C-c f") 'clang-format-buffer)
  (define-key c++-mode-map (kbd "C-c f") 'clang-format-buffer))

;; -------------------------------
;; Python Mode - Full Portable Setup
;; -------------------------------


;; Ensure Emacs uses Python 3 globally
(let ((python3-bin (or (executable-find "python3")
                       (executable-find "python"))))
  (when python3-bin
    (setq python-shell-interpreter python3-bin)
    (add-to-list 'exec-path (file-name-directory python3-bin))
    (setenv "PATH" (concat (file-name-directory python3-bin) ":" (getenv "PATH")))))

;; Ensure the Emacs debugger (M-x pdb) uses python3 -m pdb
(setq gud-pdb-command-name "python3 -m pdb")

;; Set Python indent globally before loading python-black
(setq python-indent-offset 4
      indent-tabs-mode nil)

;; Load python-black early (so it sees python-indent-offset)
(use-package python-black
  :ensure t)

(use-package pyvenv
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :defer t)

(defun my-auto-activate-venv ()
  "Activate project .venv if present."
  (let ((venv-dir (locate-dominating-file default-directory ".venv")))
    (when venv-dir
      (pyvenv-activate (expand-file-name ".venv" venv-dir))
      (setq python-shell-interpreter
            (expand-file-name "bin/python" (expand-file-name ".venv" venv-dir)))
      (message "Activated virtualenv: %s" venv-dir))))

(defun my-python-setup ()
  "Setup Python environment for current buffer."
  (my-auto-activate-venv)
  (company-mode 1)
  (eldoc-mode 1)
  (display-line-numbers-mode 1)
  ;; Buffer-local font-lock TODOs
  (font-lock-add-keywords nil
                          '(("\\<\\(TODO\\|FIXME\\|NOTE\\):" 1 font-lock-warning-face t)))
  ;; Enable python-black-on-save per buffer
  (python-black-on-save-mode 1)
  ;; Keybinding for manual formatting
  (define-key python-mode-map (kbd "C-c f") 'python-black-buffer)
  ;; Flycheck setup
  (require 'flycheck)
  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook (lambda () (flycheck-mode -1))))
  (unless (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (flycheck-mode 1))
  ;; Eglot LSP
  (when (executable-find "pylsp")
    (eglot-ensure)))

(add-hook 'python-mode-hook 'my-python-setup)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp"))))

;; -------------------------------
;; Run Python File in Bottom Buffer
;; -------------------------------
(defun my-run-python-file ()
  "Save, format with black, and run the current Python file in a small bottom buffer."
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
 '(package-selected-packages
   '(all-the-icons doom-modeline yasnippet pyvenv python-black pdf-tools flycheck doom-themes company clang-format auctex)))
(custom-set-faces)

;; -------------------------------
;; Doom Modeline - Portable (No Icon Fonts)
;; -------------------------------
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
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
