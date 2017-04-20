;; (defvar emacs-dir "/home/idfumg/.emacs.d")
;; (add-to-list 'load-path emacs-dir)

;; MISCS

;; GTAGS
(defun regenerate-gtags (directory)
  (interactive "DChoose directory: ")
  (let ((default-directory directory))
    (shell-command-to-string "gtags")
    (message "Done.")))

(defun regenerate-gtags-sirena (directory)
  (interactive "DChoose directory: ")
  (let ((default-directory directory))
    (shell-command-to-string "find . -path ./externallibs -prune -o -name \"*.[cChHpP]*\" -print > gtags.files")
    (shell-command-to-string "gtags")
    (message "Done.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USEFULL FUNCTIONS INTERACTIVE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(defun byte-recompile-init-files ()
  "Recompile all of the startup files"
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USEFULL FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun disable-modes (modes)
  (dolist (mode modes)
    (when (functionp mode)
      (funcall mode -1))))

(defun enable-modes (modes)
  (dolist (mode modes)
    (funcall mode t)))

(defun bind-global-keys (key-fns)
  (dolist (key-fn key-fns)
    (global-set-key (kbd (car key-fn)) (cdr key-fn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MARMALADE/ELPA/MELPA Repo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MY PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my/install-packages
  '(
    ;; package management
    use-package

    ;; hide modes from mode-line
    diminish

    ;; auto-complete
    company
    irony company-irony company-c-headers ;; c / c++ / objc
    anaconda-mode company-anaconda ;; python
;;    company-cmake ;; cmake
    company-statistics ;; statistics

    ;; flycheck
    flycheck
    flycheck-pos-tip

    ;; highlight word
    highlight-symbol

    ;; pretty smooth scrolling
    smooth-scrolling

    ;; compile
    smart-compile

    ;; open huge files quickly
    vlf

    ;; web-mode for web development
    web-mode

    ;; search
    engine-mode

    ;; multiple cursors
    multiple-cursors phi-search phi-search-mc mc-extras

    ;; edit-list
    edit-list

    ;; HideShow
    hideshow-org

    ;; Neotree for file browsering.
    neotree

    ;; Projectile - working with projects
    projectile helm-projectile

    ;; test
    helm-gtags helm-company

    ;; lisp
    slime
    ))

(dolist (package my/install-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Load use-package, used for loading packages everywhere else
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET CUSTOM VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c++-tab-always-indent t)
 '(default-tab-width 4 t)
 '(echo-keystrokes 0.1)
 '(fill-column 80)
 '(find-file-visit-truename nil)
 '(font-use-system-font t)
 '(gc-cons-threshold 20000000)
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ru")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(jit-lock-chunk-size 1000)
 '(jit-lock-defer-time 0.01)
 '(jit-lock-stealth-load 100)
 '(jit-lock-stealth-time 1)
 '(large-file-warning-threshold (* 30 1024 1024))
 '(line-move-visual t)
 '(make-pointer-invisible nil)
 '(mouse-sel-retain-highlight t)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1))))
 '(query-replace-highlight t)
 '(read-file-name-completion-ignore-case t)
 '(require-final-newline t)
 '(ring-bell-function (lambda nil) t)
 '(scheme-program-name "guile-2.0")
 '(scroll-step 1)
 '(search-highlight t)
 '(sentence-end-double-space nil)
 '(show-trailing-whitespace nil)
 '(tab-width 4)
 '(user-full-name "Pushkin Artem")
 '(user-mail-address "idfumg@gmail.com")
 '(visible-bell t)
 '(company-clang-begin-after-member-access nil)
 )

(load-theme 'wombat)

;; Make sure auto automatically rescan for imenu changes:
(set-default 'imenu-auto-rescan t)

(disable-modes
 '(menu-bar-mode
   scroll-bar-mode
   tooltip-mode
   tool-bar-mode
   blink-cursor-mode
   whitespace-mode
   set-fringe-style
   delete-selection-mode
   electric-pair-mode
   guru-global-mode
   ))

(enable-modes
 '(iswitchb-mode
   column-number-mode
   line-number-mode
   show-paren-mode
   ;; turn on syntax highlighting for all buffers.
   global-font-lock-mode
   ;; If you change buffer, or focus, disable the current buffer's mark:
   transient-mark-mode
   ;; Automatically revert file if it's changed on disk.
   global-auto-revert-mode
   delete-selection-mode
   ))

(bind-global-keys
 '(;; Kill whole line.
   ("C-M-k" . kill-whole-line)
   ("C-M-л" . kill-whole-line)

   ;; Goto line number.
   ("M-g" . goto-line)
   ("M-п" . goto-line)

   ;; Goto end of line.
   ("C-e" . move-end-of-line)
   ("C-у" . move-end-of-line)

   ;; Goto begin of line
   ;; ("C-a" . move-beginning-of-line)
   ;; ("C-ф" . move-beginning-of-line)
   ("C-a" . back-to-indentation)
   ("C-ф" . back-to-indentation)

   ;; Bind moving keys for russian symbols.
   ("M-а" . forward-word)
   ("M-и" . backward-word)
   ("C-а" . forward-char)
   ("C-и" . backward-char)
   ("C-т" . next-line)
   ("C-з" . previous-line)

   ;; Bind delete keys.
   ("M-d" . kill-word)
   ("M-в" . kill-word)
   ("C-d" . delete-forward-char)
   ("C-в" . delete-forward-char)

   ("M--" . highlight-symbol-at-point)
   ("C-=" . highlight-symbol-next)
   ("C--" . highlight-symbol-prev)))

(require 'highlight-symbol)

;; Set yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set font.
(set-default-font "DejaVu Sans Mono")
(set-language-environment 'UTF-8)

;; Set enconding.
(setq default-input-method 'russian-computer)
(prefer-coding-system 'cp866)
(prefer-coding-system 'utf-8-unix)
;; (set-default-coding-systems 'cp866)

;; Bury the *scratch* buffer, never kill it:
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

; savehist
(setq savehist-additional-variables
      ;; also save my search entries
      '(search-ring regexp-search-ring)
      savehist-file "~/.emacs.d/savehist")
(savehist-mode t)
(setq-default save-place t)

;; delete-auto-save-files
(setq delete-auto-save-files t)
(setq backup-directory-alist
      '(("." . "~/.emacs_backups")))

;; Prettify all the symbols, if available (an Emacs 24.4 feature).
(when (boundp 'global-prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '("lambda" . ?λ) prettify-symbols-alist)))
  (add-hook 'clojure-mode-hook
            (lambda ()
              (push '("fn" . ?ƒ) prettify-symbols-alist)))
  (global-prettify-symbols-mode +1))

(setq vc-handled-backends '(SVN Git))

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compile setup.
(use-package smart-compile
  :config
  (dolist (elem '((c-mode . "make -sj6")))
	(add-to-list 'smart-compile-alist elem)))

;; open huge files quickly.
(use-package vlf-setup
  :config
  (setq vlf-application 'dont-ask))

;; Uniquify buffers, using angle brackets, so you get foo and foo<2>.
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Turn on auto-fill mode in text buffers.
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(use-package diminish
  :init
  (progn
    (diminish 'auto-fill-function "AF")))

;; Hide abbrev mode from mode-line.
(use-package abbrev
  :diminish abbrev-mode)

;; Start a server if not running, but a only for text-only.
(use-package server
  :config
  (progn
    (when (not (window-system))
      (if (server-running-p server-name)
          nil
        (server-start)))))

;; Allows me to use C-c LEFT to undo window configuration changes.
;; (use-package winner
;;   :config (winner-mode 1))

(use-package smooth-scrolling
  :config
  (setq smooth-scroll-margin 4))

(use-package company-irony
  :config
  (progn (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
         (add-hook 'irony-mode-hook 'company-statistics-mode)
         (add-to-list 'irony-additional-clang-options "-std=c++11")
         (define-key irony-mode-map [remap completion-at-point] 'helm-company)
         (define-key irony-mode-map [remap complete-symbol] 'helm-company)
         (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
         (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
         (use-package company-statistics)
         (use-package helm-company)

         (dolist (mode-hook '(c++-mode-hook
                              c-mode-hook))
           (add-hook mode-hook 'irony-mode)))
  :bind
  (("C-<return>" . helm-company)))

(use-package flycheck
  :ensure t
  :defer t

  :init
  (use-package flycheck-pos-tip
    :init
    (progn
      (dolist (mode-hook '(c++-mode-hook
                           c-mode-hook))
        (add-hook mode-hook 'flycheck-mode)
        (add-hook mode-hook 'flycheck-pos-tip-mode))

      (dolist (mode-hook '(c++-mode-hook
                           c-mode-hook))
        (add-hook mode-hook (lambda() (setq flycheck-clang-language-standard "c++11")))
        (add-hook mode-hook (lambda() (dolist (path '("-I."))
                                   (append path flycheck-clang-include-path)))))

      (setq flycheck-pos-tip-timeout 0)
      (setq flycheck-display-errors-delay 0.5)))

  :bind
  (("C-c f e" . flycheck-list-errors)
   ("C-c f n" . flycheck-next-error)
   ("C-c f p" . flycheck-previous-error)))

(defun hs-hide-leafs-recursive (minp maxp)
  "Hide blocks below point that do not contain further blocks in
    region (MINP MAXP)."
  (when (hs-find-block-beginning)
    (setq minp (1+ (point)))
    (funcall hs-forward-sexp-func 1)
    (setq maxp (1- (point))))
  (unless hs-allow-nesting
    (hs-discard-overlays minp maxp))
  (goto-char minp)
  (let ((leaf t))
    (while (progn
             (forward-comment (buffer-size))
             (and (< (point) maxp)
                  (re-search-forward hs-block-start-regexp maxp t)))
      (setq pos (match-beginning hs-block-start-mdata-select))
      (if (hs-hide-leafs-recursive minp maxp)
          (save-excursion
            (goto-char pos)
            (hs-hide-block-at-point t)))
      (setq leaf nil))
    (goto-char maxp)
    leaf))

(defun hs-hide-leafs ()
  "Hide all blocks in the buffer that do not contain subordinate
    blocks.  The hook `hs-hide-hook' is run; see `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (message "Hiding blocks ...")
     (save-excursion
       (goto-char (point-min))
       (hs-hide-leafs-recursive (point-min) (point-max)))
     (message "Hiding blocks ... done"))
   (run-hooks 'hs-hide-hook)))

(use-package hideshow
  :ensure t
  :defer t
  :init
  (dolist (mode-hook '(c-mode-hook
                       c++-mode-hook
                       python-mode-hook
                       java-mode-hook
                       emacs-lisp-mode-hook
                       lisp-mode-hook))
    (add-hook mode-hook 'hs-minor-mode))

  :diminish hs-minor-mode
  :bind
  (("C-<right>" . hs-show-block)
   ("C-<left>" . hs-hide-block)
   ("C-<down>" . hs-show-all)
   ("C-<up>" . hs-hide-all)
   ("M-<up>" . hs-hide-leafs)
   ("M-<down>" . hs-show-all)))

(use-package company-anaconda
  :defer t
  :init
  (progn (use-package company
           :init
           (global-company-mode)
           (company-statistics-mode))
         (add-to-list 'company-backends 'company-anaconda)
         (add-hook 'python-mode-hook 'anaconda-mode))
  :diminish (anaconda-mode))


(defun my/web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(defun my/sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))

(use-package web-mode
  :ensure t
  :defer t
  :mode "\\.html?\\'"
  :config
  (progn
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-ac-sources-alist
          '(("css" . (ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
          )))

(use-package slime
  :ensure t
  :defer t
  :config
  (progn
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (slime-setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET THEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(font-lock-remove-keywords 'c++-mode
                           '(("auto" . 'font-lock-keyword-face)))

(font-lock-add-keywords 'c++-mode
                        '(("constexpr" . 'font-lock-keyword-face))
                        '(("auto" . 'font-lock-type-face)))

;;(when (display-graphic-p)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; C-u C-x =

 ;; '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "Turquoise"))))
 ;;  '(font-lock-comment-face ((t (:foreground "MediumAquamarine"))))
 ;;  '(font-lock-constant-face ((((class color) (background dark)) (:bold t :foreground "DarkOrchid"))))
 ;;  '(font-lock-doc-string-face ((t (:foreground "green2"))))
 ;;  '(font-lock-function-name-face ((t (:foreground "SkyBlue"))))
 ;;  '(font-lock-keyword-face ((t (:bold t :foreground "CornflowerBlue"))))

 ;;  '(font-lock-reference-face ((t (:foreground "DodgerBlue"))))
 ;;  '(font-lock-string-face ((t (:foreground "LimeGreen"))))

 '(default ((t (:overline nil :inherit nil :stipple nil :background "#042029" :foreground "#C7C7C7" :inverse-video nil :box nil :strike-through nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Liberation Mono"))))

 '(cursor ((t (:background "firebrick1" :foreground "black"))))
 '(font-lock-builtin-face ((t (:foreground "white"))))
 '(font-lock-comment-face ((t (:foreground "gray40"))))
 '(font-lock-constant-face ((t (:foreground "#1C86EE" :weight normal))))
 '(font-lock-type-face ((t (:foreground "#1C86EE"  :weight normal))))
 '(font-lock-function-name-face ((t (:foreground "brown"))))
 '(font-lock-keyword-face ((t (:foreground "#819501" :weight normal))))
 '(font-lock-string-face ((t (:foreground "#169186"))))
 '(font-lock-variable-name-face ((t (:foreground "#C7C7C7"))))
 '(font-lock-doc-face ((t (:foreground "tomato" :slant italic))))
 '(font-lock-preprocessor-face ((t (:italic nil :foreground "brown"))))

 '(border ((t nil)))
 '(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face :weight ultra-bold)) (((class color) (min-colors 16)) nil)))
 '(fringe ((nil (:background "black"))))
 '(helm-buffer-directory ((t (:background "LightGray" :foreground "DarkRed"))))
 '(helm-buffer-file ((t (:inherit font-lock-builtin-face))))
 '(helm-candidate-number ((t (:foreground "orange3"))))
 '(helm-ff-directory ((t (:foreground "steel blue"))))
 '(helm-ff-dotted-directory ((t (:foreground "steel blue"))))
 '(helm-ff-file ((t (:foreground "grey"))))
 '(helm-ff-prefix ((t (:background "orange3" :foreground "black"))))
 '(helm-header-line-left-margin ((t (:foreground "orange3"))))
 '(helm-match ((t (:foreground "orange3" :weight bold))))
 '(helm-selection ((t (:background "gray20" :distant-foreground "orange3" :foreground "black" :weight extra-bold))))
 '(helm-source-header ((t (:foreground "orange3" :weight bold :height 1.1 :family "Sans Serif"))))
 '(helm-visible-mark ((t (:foreground "dark orange"))))

 '(highlight ((t (:background "khaki1" :foreground "black" :box (:line-width -1 :color "firebrick1")))))
 '(highlight-current-line-face ((t (:inherit highlight))))
 '(lazy-highlight ((t (:background "paleturquoise" :foreground "black"))))
 '(link ((t (:foreground "DodgerBlue3" :underline t))))
 '(menu ((t (:background "gray2" :foreground "#FFF991"))))
 '(minibuffer-prompt ((t (:foreground "dark green"))))
 '(mouse ((t (:background "Grey" :foreground "black"))))
 '(trailing-whitespace ((((class color) (background dark)) (:background "firebrick1")))))
;;)

(when (not (display-graphic-p))
(custom-set-faces
 '(default ((t (:overline nil :inherit nil :stipple nil :background "black" :foreground "#C7C7C7" :inverse-video nil :box nil :strike-through nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Liberation Mono"))))
 
 
)
)

; make sure the frames have the dark background mode by default
(setq default-frame-alist (quote (
  (frame-background-mode . dark)
  )))

;; Mode line setup
(setq-default
 mode-line-format
 '(
   ; read-only or modified status
   " "
   (:eval
    (cond (buffer-read-only
           (propertize "*" 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize "*" 'face 'mode-line-modified-face))
          (t "")))
   ; Position, including warning for 80 columns
   (:propertize "%5l : " face mode-line-position-face)
   (:eval (propertize "%2c" 'face
                      (if (> (current-column) 90)
                          'mode-line-90col-face
                        'mode-line-position-face)))
   "  "
   (:eval (propertize "%p" 'face 'mode-line-procent-face))
   "  "
   ; directory and buffer/file name
   ;; (:propertize (:eval (shorten-directory default-directory 30))
   ;;              face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ; narrow [default -- keep?]
   "%n"
   ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   ))

;; Helper function
;; (defun shorten-directory (dir max-length)
;;   "Show up to `max-length' characters of a directory name `dir'."
;;   (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
;;         (output ""))
;;     (when (and path (equal "" (car path)))
;;       (setq path (cdr path)))
;;     (while (and path (< (length output) (- max-length 4)))
;;       (setq output (concat (car path) "/" output))
;;       (setq path (cdr path)))
;;     (when path
;;       (setq output (concat ".../" output)))
;;     output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-90col-face)
(make-face 'mode-line-procent-face)

(set-face-attribute 'mode-line nil
    :foreground "gray50" :background "gray20"
    :inverse-video nil
	:height 120
	:weight 'bold)
(set-face-attribute 'mode-line-inactive nil
    :foreground "gray30" :background "gray30"
    :inverse-video nil
    :box '(:line-width 1 :color "gray30" :style nil))
(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "green")
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "red")
(set-face-attribute 'mode-line-90col-face nil
    :inherit 'mode-line-position-face
	:family "Menlo"
	:weight 'bold
	:height 110
    :foreground "dark goldenrod"
	:background "gray20")
;; (set-face-attribute 'mode-line-folder-face nil
;;     :inherit 'mode-line-face
;;     :foreground "gray60")

;;(when (display-graphic-p) (progn
(set-face-attribute 'mode-line-position-face nil
	:inherit 'mode-line-face
	:foreground "gray50"
    :family "Menlo"
	:weight 'bold
	:height 110)
(set-face-attribute 'mode-line-procent-face nil
	:inherit 'mode-line-position-face
	:height 110
	:weight 'bold
    :foreground "gray50")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "dark goldenrod"
	:weight 'bold
	:height 130)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray50"
	:height 120)
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray50"
    :height 120)
(set-face-attribute 'mode-line-process-face nil
	:inherit 'mode-line-face
	:weight 'bold
	:height 120
    :foreground "gray50")
;;))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET INTENDATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(c-add-style "mycodingstyle"
             '((c-basic-offset . 4)
               (c-comment-only-line-offset . 0)
               (c-hanging-braces-alist . ((substatement-open before after)))
               (c-offsets-alist . ((topmost-intro        . 0)
                                   (topmost-intro-cont   . 0)
                                   (substatement         . 4)
                                   (substatement-open    . 4)
                                   (statement-case-open  . 4)
                                   (statement-cont       . 4)
                                   (access-label         . -4)
                                   (inclass              . 4)
                                   (inline-open          . 0)
                                   (innamespace          . 4)
                                   ))))
;; treat all tabs to spaces
(defun intendation-c-mode-hook ()
  (c-set-style "mycodingstyle"))

(add-hook 'c-mode-common-hook   'intendation-c-mode-hook)
(add-hook 'c++-mode-common-hook 'intendation-c-mode-hook)

(global-set-key (kbd "RET") 'newline-and-indent)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C & C++ HOOKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-4") 'ff-find-other-file)))

;; (defun run-python-once ()
;;   (remove-hook 'python-mode-hook 'run-python-once)
;;   (run-python))

(add-hook 'python-mode-hook
          (lambda()
            (run-python "/usr/bin/python")))






;;;;;;;;;;;

(defun sanityinc/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(bind-key "C-M-<backspace>" 'sanityinc/kill-back-to-indentation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package "eldoc"
;;   :ensure t
;;   :diminish eldoc-mode
;;   :commands turn-on-eldoc-mode
;;   :defer t
;;   :init
;;   (progn
;;   (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;;   (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
;;   (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adjust opacity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sanityinc/adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -6)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 6)))
(global-set-key (kbd "M-C-0") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(setq browse-url-chromium-program "/usr/bin/chromium")
(setq browse-url-firefox-program "/usr/bin/firefox")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/chromium")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search / Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package engine-mode
  :ensure t
  :config
  (progn
    (defengine my-blog "https://www.google.ca/search?q=site:sachachua.com+%s" :keybinding "b")
    (defengine my-photos "http://www.flickr.com/search/?w=65214961@N00&q=%s" :keybinding "f")
    (defengine mail "https://mail.google.com/mail/u/0/#search/%s" :keybinding "m")
    (defengine google "http://google.com/search?q=%s" :keybinding "g")
    (defengine emacswiki "http://google.com/search?q=site:emacswiki.org+%s" :keybinding "e")
    (defengine translate "https://translate.google.ru/#en/ru/+%s" :keybinding "e")
    (bind-key* "C-c w g" 'engine/search-google)
    (bind-key* "C-c w t" 'engine/search-translate)
    (engine-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package multiple-cursors
  :ensure t :defer t
  :bind
  (("C-c m m" . mc/mark-all-dwim)

   ("C-c m n" . mc/mark-next-like-this)
   ("C-c m p" . mc/mark-previous-like-this)

   ("C-c m l" . mc/edit-ends-of-lines)))

(use-package phi-search
  :ensure t :defer t)

(use-package phi-search-mc
  :ensure t :defer t
  :config
  (phi-search-mc/setup-keys))

(use-package mc-extras
  :ensure t :defer t
  :config
    (define-key mc/keymap (kbd "C-. =") 'mc/compare-chars))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package edit-list
  :ensure t :defer t
  :commands edit-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Neo tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package neotree
  :ensure t
  :defer t

  :bind
  (("C-c n t" . neotree-toggle)
   ("C-c n m" . neotree-rename-node)
   ("C-c n d" . neotree-delete-node)
   ("C-c n c" . neotree-create-node)
   ("C-c n r" . neotree-change-root)
   ("C-c n h" . neotree-hidden-file-toggle))

  :config
  (setq neo-smart-open t))

(use-package helm
  :init
  (progn (setq helm-locate-fuzzy-match t)
         (helm-mode t))
  :bind
  (("C-x b" . helm-buffers-list)))

(use-package projectile
  :init
  (progn (projectile-mode t)
         (setq projectile-indexing-method 'native)
         (setq projectile-enable-caching t)
         (setq projectile-completion-system 'helm)
         (setq projectile-switch-project-action 'neotree-projectile-action)
         (projectile-global-mode)

         (use-package helm-ggtags
           :init
           (progn (helm-gtags-mode t))

           :bind
           (("C-." . helm-gtags-find-tag)
            ("M-." . helm-gtags-find-rtag)
            ("C-," . helm-gtags-previous-history)))

         (use-package helm-projectile
           :bind
           (("C-c f" . helm-projectile-find-file)
            ("C-c g" . helm-projectile-grep)
            ("C-c p p" . helm-projectile)

            ("M-s" . helm-occur)

            ("C-x f" . helm-locate)
            ("C-x C-f" . helm-find-files)
            ))))

(provide '.emacs)
;;; .emacs ends here
