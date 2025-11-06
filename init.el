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
 '(package-selected-packages nil))
(custom-set-faces)
