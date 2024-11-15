;; (add-to-list 'load-path "~/.emacs.d")

(defmacro with-system (type &rest body)
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(defun is-x11-mode? ()
  window-system)

(defun is-terminal-mode? ()
  (not (is-x11-mode?)))

(defmacro with-terminal-mode (&rest body)
  (declare (indent defun))
  `(when (is-terminal-mode?)
     ,@body))

(defmacro with-x11-mode (&rest body)
  (declare (indent defun))
  `(when (is-x11-mode?)
     ,@body))

(defun my/vc/get-root (directory)
  (interactive "DChoose directory: ")

  (unless directory
    (error "Error! Directory is nil!"))

  (-let ((default-directory-saved default-directory)
         (result nil))

    (cd directory)
    (while (and (not (my/vc/root? default-directory))
                (not (s-equals? default-directory "/")))
      (cd ".."))
    (unless (s-equals? default-directory "/")
      (setq result default-directory))
    (cd default-directory-saved)
    result))

(defun my/sirena/sirena-svn? (directory)
  (-let [svn-url (shell-command-to-string (s-concat "svn info " directory " --show-item url"))]
    (s-contains? "svn+ssh://svn/SVNroot/sirena" svn-url)))

(defun my/sirena/sirena-git? (directory)
  (-let [svn-url (shell-command-to-string "git remote get-url origin")]
    (s-contains? "sirena" svn-url)))

(defun my/sirena/in-project-now? ()
  (interactive)
  (when (buffer-file-name)
    (message (buffer-file-name))
    (-when-let (svn-root (my/vc/get-root (file-name-directory (buffer-file-name))))
      (or (my/sirena/sirena-svn? svn-root)
          (my/sirena/sirena-git? svn-root)))))

(defmacro with-sirena-project (&rest body)
  (declare (indent defun))
  `(when (my/sirena/in-project-now?)
     ,@body))

(defmacro with-common-project (&rest body)
  (declare (indent defun))
  `(unless (my/sirena/in-project-now?)
     ,@body))

(defun my/sirena-encoding ()
  (cond ((eq system-type 'gnu/linux) 'cp866-unix)
        (t 'cp866-unix)))

(defun my/common-project-encoding ()
  (cond ((eq system-type 'gnu/linux) 'utf-8-unix)
        (t 'utf-8-unix)))

(defun my/is-sirena-encoding? ()
  (eq buffer-file-coding-system (my/sirena-encoding)))

(defun my/is-common-project-encoding? ()
  (eq buffer-file-coding-system (my/common-project-encoding)))

(defun my/return-t (orig-fun &rest args)
  t)

(defun my/disable-yornp (orig-fun &rest args)
  (advice-add 'yes-or-no-p :around #'my/return-t)
  (advice-add 'y-or-n-p :around #'my/return-t)
  (let ((res (apply orig-fun args)))
    (advice-remove 'yes-or-no-p #'my/return-t)
    (advice-remove 'y-or-n-p #'my/return-t)
    res))

(defun my/setup-encodings (encoding)
  ;;(setq default-process-coding-system '(cp866 . cp866))
  (setq locale-coding-system encoding)
  (setq set-buffer-process-coding-system encoding)
  (setq default-buffer-file-coding-system encoding)
  (setq default-input-method 'russian-computer)

  (prefer-coding-system encoding)
  ;; (prefer-coding-system 'utf-8-unix)
  (prefer-coding-system encoding)

  (set-language-environment 'UTF-8)
  (set-default-coding-systems encoding)
  (set-terminal-coding-system encoding)
  (set-file-name-coding-system encoding)
  (set-keyboard-coding-system encoding)
  (set-selection-coding-system encoding)
  ;; (set-buffer-file-coding-system encoding)

  (with-system windows-nt
    (set-w32-system-coding-system encoding)
    (setq w32-system-coding-system encoding))

  (advice-add 'revert-buffer-with-coding-system :around #'my/disable-yornp)
  (when (and buffer-file-name (not (eq buffer-file-coding-system encoding)))
    (revert-buffer-with-coding-system encoding))
  )

(defun my/setup/encodings ()
  (with-sirena-project
    (message "This is sirena project...")
    (unless (my/is-sirena-encoding?)
      (message "Configuring sirena project encoding...")
      (my/setup-encodings (my/sirena-encoding))))

  (with-common-project
    (unless (my/is-common-project-encoding?)
      (message "Configuring common project encoding...")
      (my/setup-encodings (my/common-project-encoding)))))

(defun my/setup/packages ()
  (unless (require 'package)
    (error "Error! Can't find 'package!"))

  (add-to-list 'load-path "~/.emacs.d/packages")
  (require 's) ;; https://github.com/magnars/s.el
  (require 'load-env-vars) ;; https://github.com/diasjorge/emacs-load-env-vars/blob/master/load-env-vars.el
  ;; (require 'copilot) ;; https://github.com/zerolfx/copilot.el

  (package-initialize)

  (let ((repos '(
		 ("melpa" . "https://melpa.org/packages/")
		 ("elpa" . "https://elpa.gnu.org/packages/")
		 )))
    (dolist (repo repos)
      (unless (assoc (car repo) package-archives)
        (add-to-list 'package-archives repo t))))

  (let ((packages '(;; package management
                    use-package

                    ;; hide modes from mode-line
                    ;; diminish

                    ;; auto-complete
                    company
                    company-statistics
                    company-irony
                    company-irony-c-headers
                    irony

                    ;; company-tabnine

                    ;; python
                    anaconda-mode
                    company-anaconda
                    pyvenv
                    blacken
                    python-mode

                    ;; highlight word
                    highlight-symbol

                    ;; pretty smooth scrolling
                    smooth-scrolling

                    ;; open huge files quickly
                    ;; vlf

                    ;; multiple cursors
                    multiple-cursors phi-search phi-search-mc mc-extras

                    ;; edit-list

                    ;; HideShow
                    ;; hideshow-org

                    ;; Neotree for file browsering.
                    neotree
                    ;;treemacs

                    ;; Projectile - working with projects
                    projectile helm-projectile

                    ;; test
                    helm-gtags
                    helm-company

                    swiper-helm
                    ivy
                    counsel

                    ;; grep -> ack -> ag
                    ;; https://github.com/ggreer/the_silver_searcher
                    ;; sudo apt install silversearcher-ag
                    ag

                    ;; sudo add-apt-repository ppa:x4121/ripgrep
                    ;; sudo apt-get update
                    ;; sudo apt install ripgrep
                    ;; rg

                    ;; org-mode
                    org

                    ;; theme
                    solarized-theme

                    ;; strings manipulations
                    ;; https://github.com/magnars/s.el
                    ;; s

                    ;; lists manipulations
                    ;; https://github.com/magnars/dash.el
                    dash

                    ;; System resources monitor in the mode line when idle
                    symon

                    doom-modeline

                    ;; Lua packages
                    lua-mode
                    ;; company-lua

                    ;; hash tables
                    ht

                    ;; popups
                    popup

                    ;; http
                    request
                    json

                    ;; elixir
                    elixir-mode
                    alchemist

                    ;; docker
                    dockerfile-mode

                    ;; yaml
                    yaml-mode

                    ;; system environment
                    ;;load-env-vars
                    dotenv-mode

                    ;; golang
                    go-mode
                    lsp-mode
                    lsp-ui
                    ;;company-mode
                    lsp-treemacs
                    helm-lsp
                    dap-mode

                    ;; github copilot
                    s
                    dash
                    editorconfig

                    flycheck
                    flycheck-golangci-lint

                    ;; Should be called at the first time after installation
                    ;; M-x all-the-icons-install-fonts
                    all-the-icons
                    doom-modeline
                    doom-themes

                    ;; ui improvements
                    ivy
                    which-key
                    which-func
                    ivy-rich
                    helpful

                    js2-mode

                    drag-stuff

                    )))

    (let ((package-list-was-refreshed? nil))
      (dolist (package packages)
        (unless (package-installed-p package)
          (unless package-list-was-refreshed?
            (package-refresh-contents)
            (setq package-list-was-refreshed? t))
          (message "Install package %s" package)
          (package-install package))))))

(defun my/env/load ()
  (require 'load-env-vars)
  (message "Loading environment variables")
  (let ((envfile (expand-file-name "~/.env")))
    (if (file-exists-p envfile)
        (load-env-vars envfile)
      (message "Error! .env file does not exist!")))
  )

(defun my/setup/font-size ()
  (let ((font-size-default "110")
        (font-size-env (or (getenv "EMACS_FONT_SIZE") (getenv "DOT_ENV_EMACS_FONT_SIZE"))))
    (setq font-size (string-to-number (or font-size-env font-size-default)))))

(defun my/setup/mode-line (&rest args)
  (let ((read-only-color (plist-get args :read-only-color))
        (modified-color (plist-get args :modified-color))
        (filename-color (plist-get args :filename-color))
        (position-color (plist-get args :position-color))
        (major-mode-color (plist-get args :major-mode-color))
        (minor-mode-color (plist-get args :minor-mode-color))
        (very-long-line-color (plist-get args :very-long-line-color))
        (percent-position-color (plist-get args :percent-position-color))
        (background-color (plist-get args :background-color))
        (foreground-color (plist-get args :foreground-color)))

    (make-face 'read-only-face)
    (make-face 'modified-face)
    (make-face 'filename-face)
    (make-face 'position-face)
    (make-face 'major-mode-face)
    (make-face 'minor-mode-face)
    (make-face 'very-long-line-face)
    (make-face 'percent-position-face)

    (set-face-attribute 'read-only-face nil
                        :inherit 'face
                        :foreground read-only-color)

    (set-face-attribute 'modified-face nil
                        :inherit 'face
                        :foreground modified-color)

    (set-face-attribute 'filename-face nil
                        :inherit 'face
                        :foreground filename-color
	                    :weight 'bold)

    (set-face-attribute 'position-face nil
	                    :inherit 'face
	                    :foreground position-color
                        :family "Menlo"
	                    :weight 'bold)

    (set-face-attribute 'major-mode-face nil
                        :inherit 'face
                        :foreground major-mode-color)

    (set-face-attribute 'minor-mode-face nil
                        :inherit 'mode-face
                        :foreground minor-mode-color)

    (set-face-attribute 'very-long-line-face nil
                        :inherit 'position-face
	                    :family "Menlo"
	                    :weight 'bold
                        :foreground very-long-line-color
	                    :background "gray20")

    (set-face-attribute 'percent-position-face nil
	                    :inherit 'position-face
	                    :weight 'bold
                        :foreground percent-position-color)

    (set-face-attribute 'mode-line nil
                        :background background-color
                        :foreground foreground-color
                        :underline nil)

    (setq-default
     mode-line-format
     '(
       ;; Read-only or modified status
       ""
       (:eval
        (cond
         (buffer-read-only (propertize "*" 'face 'read-only-face))
         ((buffer-modified-p) (propertize "*" 'face 'modified-face))
         (t "")))

       ;; Current buffer position in percent
       " "
       (:propertize "%p" face percent-position-face)

       ;; Current position
       " "
       (:propertize "%l" face position-face)

       ;; Current column with hightlight when out of max width value
       ":"
       (:eval (propertize "%c" 'face (if (> (current-column) 90)
                                         'very-long-line-face
                                       'position-face)))

       ;; Current buffer name
       " "
       (:propertize "%b" face filename-face)

       ;; Show version control type if available
       (vc-mode vc-mode)

       ;; Current mode name
       " "
       (:propertize mode-name face major-mode-face)

       ;; List of minor mode names
       (:propertize minor-mode-alist face minor-mode-face))
     )))

(defun my/setup/font ()
  ;; http://dpi.lv or xrdb -query
  ;; ~/.Xdefaults
  ;; *customization: -color
  ;; Xcursor.size:   24
  ;; Xcursor.theme:  DMZ-White
  ;; Xft.antialias:  true
  ;; Xft.autohint: true
  ;; Xft.dpi:    221
  ;; Xft.hinting:    true
  ;; Xft.hintstyle:  hintfull
  ;; Xft.rgba:   rgb
  ;; Emacs.font: Iosevka Fixed

  (defun my/current-font ()
    (face-attribute 'default :font))

  (defun my/print-current-font ()
    (interactive)
    (message "%s" (my/current-font)))

  (defun my/screen-width ()
    (if (is-x11-mode?)
        (x-display-pixel-width)
      (display-pixel-width)))

  (defun my/screen-height ()
    (if (is-x11-mode?)
        (x-display-pixel-height)
      (display-pixel-height)))

  (defun my/font-exists? (font)
    (find-font (font-spec :name font)))

  (defun my/font-size (font)
    (cond ((eq font 'iosevka)
           (cond ((and (>= (my/screen-height) 1440) (>= (my/screen-width) 2560)) " 15")
                 ((and (>= (my/screen-height) 1080) (>= (my/screen-width) 1920)) " 12")
                 ((and (>= (my/screen-height) 1050) (>= (my/screen-width) 1680)) " 19")
                 ((and (>= (my/screen-height) 1028) (>= (my/screen-width) 1680)) " 6")
                 (t " 16")))
          (t " 16")))

  (defun my/find-font ()
    ;; https://fonts.google.com/specimen/Source+Code+Pro
    ;; https://fonts.google.com/specimen/JetBrains+Mono?query=jetbrains
    (let* (
           (jbm "JetBrainsMono Nerd Font")
           (scp "Source Code Pro")
           (iosevka "Iosevka Fixed")
           (ubuntu "Ubuntu Mono")
           (consolas "Consolas")
           (liberation "Liberation Mono"))
      (cond
       ((my/font-exists? jbm) (s-concat jbm (my/font-size 'jbm)))
       ((my/font-exists? scp) (s-concat scp (my/font-size 'scp)))
       ((my/font-exists? iosevka) (s-concat iosevka (my/font-size 'iosevka)))
       ((my/font-exists? ubuntu) (s-concat ubuntu (my/font-size 'ubuntu)))
       ((my/font-exists? consolas) (s-concat consolas (my/font-size 'consolas)))
       ((my/font-exists? liberation) (s-concat liberation (my/font-size 'liberation)))
       (t ""))))

  (defun my/set-font-helper (font frame)
    (set-frame-parameter frame 'font font))

  (defun my/set-font (font)
    (my/set-font-helper font nil)
    (push (lambda (frame) (my/set-font-helper font frame)) after-make-frame-functions))

  (my/set-font (my/find-font))
  (message "Screen width: %s, height: %s" (my/screen-width) (my/screen-height))
  (message "Current font: %s" (my/current-font)))

(defun my/setup/theme-backup ()
  (load-theme 'solarized-dark t)

  (my/setup/mode-line
   :read-only-color "green"
   :modified-color "red"
   :very-long-line-color "dark goldenrod"
   :position-color "gray50"
   :percent-position-color "gray50"
   :filename-color "dark goldenrod"
   :major-mode-color "gray50"
   :minor-mode-color "gray50")

  ;; Set cursor color
  (set-cursor-color "gray")

  ;; Set region foreground highlight color
  (set-face-foreground 'region nil)

  ;; Set region background highlight color
  (set-face-background 'region "DarkSlateGray")
  )

(defun my/setup/theme ()
  (message "Configuring x11 theme")

  (use-package doom-modeline
    :init
    (doom-modeline-mode 1)
    :custom ((doom-modeline-height 25)
             (doom-modeline-buffer-file-name-style 'file-name)))

  (use-package doom-themes
    :init
    (load-theme 'doom-solarized-dark t)
    (doom-themes-neotree-config)
    )

  (use-package doom-themes
    :init
    (load-theme 'doom-one t)
    (doom-themes-neotree-config)
    )

  ;; (use-package rainbow-delimiters
    ;; :hook (prog-mode . rainbow-delimiters-mode))
  )

(defun my/setup/theme-console ()
  (message "Configuring terminal theme")

  (load-theme 'solarized-dark t)

  (my/setup/mode-line
   :read-only-color "green"
   :modified-color "red"
   :very-long-line-color "dark goldenrod"
   :position-color "dark goldenrod"
   :percent-position-color "dark goldenrod"
   :filename-color "dark goldenrod"
   :major-mode-color "dark goldenrod"
   :minor-mode-color "dark goldenrod"
   :background-color "black"
   :foreground-color "black")

  ;; Set cursor color
  (set-cursor-color "gray")

  ;; Set region foreground highlight color
  (set-face-foreground 'region nil)

  ;; Set region background highlight color
  (set-face-background 'region "gray50")

  (set-face-attribute 'default nil
                      :background "black"
                      :foreground "white"))

(defun my/setup/modes ()
  (defun disable-modes (modes)
    (dolist (mode modes)
      (when (functionp mode)
        (funcall mode -1))))

  (defun enable-modes (modes)
    (dolist (mode modes)
      (when (functionp mode)
        (funcall mode t))))

  (disable-modes '(menu-bar-mode
                   scroll-bar-mode
                   tooltip-mode
                   tool-bar-mode
                   ;; Disable cursor blinking
                   blink-cursor-mode
                   whitespace-mode
                   set-fringe-style
                   delete-selection-mode
                   electric-pair-mode
                   guru-global-mode))

  (enable-modes '(column-number-mode
                  line-number-mode
                  show-paren-mode
                  ;; turn on syntax highlighting for all buffers
                  global-font-lock-mode
                  ;; disable mark if buffer changed
                  transient-mark-mode
                  ;; automatically revert file if it's changed on disk
                  global-auto-revert-mode
                  delete-selection-mode
                  ;; Turn on image viewing
                  auto-image-file-mode
                  ;; Save the last cursor place in a file
                  save-place-mode))

  ;; Prevent add tabs when indent of text was used.
  (setq-default indent-tabs-mode nil))

(defun my/map-terminal-key (key)
  "Map KEY from escape sequence \"\e[emacs-KEY\."
  (define-key function-key-map (concat "\e[emacs-" key) (kbd key)))

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

(defun my/setup/keys ()
  (defun my/kill-back-to-indentation ()
    (interactive)
    (let ((prev-pos (point)))
      (back-to-indentation)
      (kill-region (point) prev-pos)))

  (global-unset-key [(control ?v)])
  (global-unset-key [(meta ?v)])

  ;; kill ring
  (global-set-key [(meta ?w)] 'kill-ring-save)
  (global-set-key [(control ?y)] 'yank)

  ;; killing stuff
  (global-set-key [(meta ?d)] 'my/kill-word)
  (global-set-key [(control ?k)] 'my/kill-line)
  (global-set-key [(meta ?k)] 'my/kill-sentence)
  (global-set-key [(control ?d)] 'delete-forward-char)
  (global-set-key [(backspace)] 'delete-backward-char)
  (global-set-key [(control backspace)] 'my/backward-kill-word)
  (global-set-key [(meta backspace)] 'my/backward-kill-word)
  (global-set-key [(control meta backspace)] 'my/kill-back-to-indentation)

  ;; moving and duplicating lines
  (global-set-key [(control meta down)] 'duplicate-line)
  (global-set-key [\M-up] 'drag-stuff-up)
  (global-set-key [\M-down] 'drag-stuff-down)

  ;; moving cursor
  (global-set-key [(control meta ?e)] 'forward-sentence)
  (global-set-key [(control meta ?a)] 'backward-sentence)
  (global-set-key [(control ?p)] 'previous-line)
  (global-set-key [(control ?n)] 'next-line)
  (global-set-key [(control ?b)] 'backward-char)
  (global-set-key [(control ?f)] 'forward-char)
  (global-set-key [(meta ?b)] 'backward-word)
  (global-set-key [(meta ?f)] 'forward-word)
  (global-set-key [(control ?a)] 'back-to-indentation)
  (global-set-key [(control ?e)] 'move-end-of-line)
  (global-set-key [(meta ?g)] 'goto-line)

  ;; scrolling
  (global-set-key [(meta ?p)] 'scroll-down-line)
  (global-set-key [(meta ?n)] 'scroll-up-line)
  (global-set-key [(control meta ?p)] 'scroll-down-command)
  (global-set-key [(control meta ?n)] 'scroll-up-command)

  ;; highlighting
  (global-set-key [(meta ?-)] 'highlight-symbol-at-point)
  (global-set-key [(control ?=)] 'highlight-symbol-next)
  (global-set-key [(control ?-)] 'highlight-symbol-prev)

  ;; helm buffers and stuff
  (global-set-key [(meta ?x)] 'counsel-M-x)
  (global-set-key [(meta ?y)] 'helm-show-kill-ring)
  (global-set-key [(control ?x) ?b] 'helm-buffers-list)
  (global-set-key [(meta ?s)] 'helm-occur)

  ;; (global-set-key [(control ?x) ?f] 'helm-locate)
  ;; (global-set-key [(control ?x) (control ?f)] 'helm-find-files)
  ;; (global-set-key [(control ?.)] 'helm-gtags-find-tag)
  ;; (global-set-key [(meta ?.)] 'helm-gtags-find-rtag)
  ;; (global-set-key [(control ?,)] 'helm-gtags-previous-history)

  (ivy-mode)
  (counsel-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper-helm)

  ;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; (global-set-key (kbd "<f6>") 'ivy-resume)
  ;; (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  ;; (global-set-key (kbd "C-c g") 'counsel-git)
  ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-g") 'counsel-ag)
  (global-set-key (kbd "C-x C-l") 'counsel-locate) ;; sudo updatedb before use it
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  ;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

  (global-set-key [(control meta ?b)] 'neotree-toggle)
  (global-set-key [(control meta ?v)] 'neotree-stretch-toggle)
  ;; (global-set-key [(control ?c) ?n ?r] 'neotree-rename-node)
  ;; (global-set-key [(control ?c) ?n ?d] 'neotree-delete-node)
  ;; (global-set-key [(control ?c) ?n ?n] 'neotree-create-node)
  ;; (global-set-key [(control ?c) ?n ?c] 'neotree-copy-node)

  (global-set-key [(control ?c) ?m ?m] 'mc/mark-all-dwim)
  (global-set-key [(control ?c) ?m ?l] 'mc/edit-lines)
  (global-set-key [(control ?c) ?m ?n] 'mc/mark-next-like-this)
  (global-set-key [(control ?c) ?m ?p] 'mc/mark-previous-like-this)
  (global-set-key [(control ?c) ?m ?x] 'mc/mark-lines)
  (global-set-key [(control return)] 'company-complete)

  ;; text scaling
  (setq text-scale-mode-step 1.1)
  (global-set-key [(control ?')] 'text-scale-increase)
  (global-set-key [(control ?\;)] 'text-scale-decrease)

  (global-set-key [(control ?q)] 'imenu)

  (with-system darwin
    (setq mac-option-key-is-meta nil)
    (setq mac-option-modifier 'alt)
    (setq mac-command-key-is-meta t)
    (setq mac-command-modifier 'meta))

  (with-terminal-mode
    (dolist (sequence '("C-="
                        "C--"
                        "M-w"
                        "C-y"
                        "C-/"
                        "C-M-n"
                        "C-M-p"))
      (my/map-terminal-key sequence)))

  ;; (global-set-key [(control return)] 'helm-company))
  )
(defun my/setup/c++ ()
  ;; Commands to checking the irony settings:
  ;; irony-cdb-autosetup-compile-options
  ;; irony-cdb-menu

  (defun my/setup/shell-command (cmd)
    (compile cmd))

  (defun my/setup/c++/is-c++-mode? ()
    (eq 'c++-mode major-mode))

  (defun my/setup/c++/compile (&optional command)
    (interactive)

    (unless (my/setup/c++/is-c++-mode?)
      (error "Error! You are not in the c++-mode!"))

    (compile command)
    (switch-to-buffer-other-window "*compilation*"))

  (defun my/setup/c++/form-compilation-command (options)
    (-let* ((compiler "CPLUS_INCLUDE_PATH=/usr/local/opt/llvm/include/c++/v1:/Library/Developer/CommandLineTools/SDKs/MacOSX10.15.sdk/usr/include LIBRARY_PATH=$LIBRARY_PATH:/Library/Developer/CommandLineTools/SDKs/MacOSX10.15.sdk/usr/lib clang++")
            ;;(compiler (getenv "CXX"))
            (standard "-std=c++17")
            ;;(command (format "%s %s %s %s && time ./a.out"
            (command (format "%s %s %s %s"
                             compiler
                             (mapconcat 'identity options " ")
                             standard
                             (buffer-file-name))))
      command))

  (defun my/setup/c++/compile-debug (&optional command)
    (interactive)

    (-let [options '("-fsanitize=address"
                     "-fsanitize=undefined"
                     "-fno-sanitize-recover=all"
                     "-D_GLIBCXX_DEBUG"
                     "-D_GLIBCXX_DEBUG_PEDANTIC"
                     "-D_FORTIFY_SOURCE=2"
                     "-Wno-macro-redefined" ;; It complains to D_FORTIFY_SOURCE redefining
                     "-fstack-protector"
                     "-Og"
                     "-g")]
      (my/setup/c++/compile (my/setup/c++/form-compilation-command options))))

  (defun my/setup/c++/compile-pedantic (&optional command)
    (interactive)

    (-let [options '("-Werror"
                     "-Wall"
                     "-Wno-unused-result"
                     "-Wshadow"
                     "-Wformat=2"
                     "-Wfloat-equal"
                     ;;"-Wsign-conversion"
                     "-Wshift-overflow"
                     "-Wdouble-promotion"
                     "-pedantic-errors"
                     "-pedantic"
                     ;;"-Wold-style-cast"
                     "-Wextra"
                     "-ansi"
                     "-Weffc++"
                     "-Wstrict-aliasing"
                     "-fno-rtti"
                     "-Wnull-dereference"
                     "-Wnonnull"
                     "-Wimplicit-fallthrough"
                     "-Winit-self"
                     "-g"
                     "-O3")]
      (my/setup/c++/compile (my/setup/c++/form-compilation-command options))))

  (defun my/setup/c++/compile-performance (&optional command)
    (interactive)

    (-let [options '("-O3")]
      (my/setup/c++/compile (my/setup/c++/form-compilation-command options))))

  (defun my/bury-compile-buffer-if-successful (buffer string)
    (when (and
           (buffer-live-p buffer)
           (string-match "compilation" (buffer-name buffer))
           (string-match "finished" string)
           (not
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "warning" nil t))))
      (run-with-timer 0.5 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill)
                        (other-window 1))
                      buffer)))

  (defun my/setup/c++/compilation-finished-hook (buffer msg)
    (if (s-contains? "finished" msg)
        (my/tooltip/show "\n Compilation successful :-) \n" "green")
      (my/tooltip/show "\n Compilation failed :-( \n" "red"))
    (my/bury-compile-buffer-if-successful buffer msg))

  (defun my/setup/sirena ()
    (defun my/sirena/redmine/cut-tasks-names (start end)
      (interactive "r")

      (defun my/sirena/redmine/cut-task-name ()
        (beginning-of-line)
        (forward-word)
        (kill-sexp 3)
        (end-of-line)
        (backward-kill-word 7)
        (delete-trailing-whitespace)
        (beginning-of-line)
        (insert "#")
        (forward-word)
        (delete-region (point) (progn (skip-chars-forward " \t") (point)))
        (insert " "))

      (save-excursion
        (save-restriction
          (narrow-to-region (or start (point-min)) (or end (point-max)))
          (indent-region (point-min) (point-max))
          (goto-char (point-min))
          (my/sirena/redmine/cut-task-name)
          (while (and (forward-line) (not (>= (point) (point-max))))
            (my/sirena/redmine/cut-task-name))
          (sort-lines nil (point-min) (point-max)))))

    (defun my/sirena/open-trunk ()
      (interactive)
      (-let [path (expand-file-name (getenv "DOT_ENV_SIRENA_PATH"))]
        (message path)
        (if path
            (find-file (s-concat path "/trunk/src/rail/rail_order.cc"))
          (error "Error! No sirena trunk located!"))))

    (defun my/sirena/open-stable ()
      (interactive)
      (-let [path (expand-file-name (getenv "DOT_ENV_SIRENA_PATH"))]
        (if path
            (find-file (s-concat path "/stable/src/rail/rail_order.cc"))
          (error "Error! No sirena stable located!")))))

  (defun my/sirena/hook ()
    ;; (-let [sirena-env-vars '(("ORACLE_BASE" . "/u01/app/oracle")
    ;;                          ("ORACLE_HOME" . "/u01/app/oracle/product/12.1.0/db_1")
    ;;                          ("ORACLE_BIN" . "/u01/app/oracle/product/12.1.0/db_1/bin")
    ;;                          ("ORACLE_LIB" . "/u01/app/oracle/product/12.1.0/db_1/lib")
    ;;                          ("ORACLE_SID" . "orcl")
    ;;                          ("ORACLE_INVENTORY" . "/u01/app/oracle/product/12.1.0/db_1/inventory")
    ;;                          ("NLS_LANG" . "AMERICAN_CIS.RU8PC866")
    ;;                          ("SVN_SSH" . "ssh -i /home/idfumg/.ssh/id_rsa -l svn")
    ;;                          ("SVN_BASE" . "svn+ssh://svn/SVNroot/sirena"))]

    (defun my/sirena/make-in (compilation-dir cmd)
      (unless (my/sirena/in-project-now?)
        (error "Error! You are not in the sirena project! (%s)" buffer-file-name))

      (compile cmd))

    (defun my/sirena/run-tests (domain &optional test-name)
      (-let [test-name (if (null test-name)
                           ""
                         (s-concat "." test-name))]
        (my/sirena/make-in "src" (s-concat "XP_LIST=" domain test-name " make xp-tests"))))

    (defun my/sirena/rail-tests (&optional test-name)
      (interactive "MTest name: ")
      (my/setup/shell-command (format "sirena_test_rail %s" (or test-name ""))))

    (defun my/sirena/posauth-test ()
      (interactive)
      (my/setup/shell-command "sirena_test_posauth"))

    (defun my/sirena/airimp-test ()
      (interactive)
      (my/setup/shell-command "sirena_test_airimp"))

    (defun my/sirena/emd-test ()
      (interactive)
      (my/setup/shell-command "sirena_test_emd"))

    (defun my/sirena/obrzap-make ()
      (interactive)
      (my/setup/shell-command "sirena_make_obrzap"))

    (defun my/sirena/rail-make ()
      (interactive)
      (my/setup/shell-command "sirena_make_rail"))

    (defun my/sirena/rail-rebuild ()
      (interactive)
      (my/sirena/make-in "sirena_clean_rail && sirena_make_rail && sirena_make_obrzap"))

    (defun my/sirena/posauth-make ()
      (interactive)
      (my/setup/shell-command "sirena_make_posauth"))

    (defun my/sirena/airimp-make ()
      (interactive)
      (my/setup/shell-command "sirena_make_airimp"))

    (defun my/sirena/emd-make ()
      (interactive)
      (my/setup/shell-command "sirena_make_emd"))

    (defun my/sirena/sirenalibs-make ()
      (interactive)
      (my/setup/shell-command "sirena_make_libs"))

    (defun my/sirena/serverlib-make ()
      (interactive)
      (my/setup/shell-command "sirena_make_serverlib"))

    (defun my/sirena/c++/compilation-finished-hook (buffer msg)
      (my/setup/c++/compilation-finished-hook buffer msg)
      ;;(kill-buffer buffer)
      )

    (defun my/ansi-colorize ()
      (require 'ansi-color)
      (let ((buffer-read-only nil))
        (ansi-color-apply-on-region (point-min) (point-max))))

    ;; (add-hook 'find-file-hook 'my/setup/encodings)

    (my/setup/encodings)

    (with-sirena-project
      (setq tooltip-hide-delay 2)
      (setq compilation-ask-about-save nil)
      (add-to-list 'compilation-finish-functions 'my/sirena/c++/compilation-finished-hook)
      (global-set-key [(control ?x) ?m] 'my/sirena/rail-make)
      (global-set-key [(control ?x) ?t] 'my/sirena/rail-tests)
      (add-hook 'compilation-filter-hook 'my/ansi-colorize)
      (add-hook 'shell-filter-hook 'my/ansi-colorize)
      (add-hook 'compilation-mode-hook (lambda () (prefer-coding-system 'cp866)))
      (add-hook 'shell-mode-hook (lambda () (prefer-coding-system 'cp866)))
      )

    (with-common-project
      (when (my/setup/c++/is-c++-mode?)
        (setq tooltip-hide-delay 2)
        (setq compilation-ask-about-save nil)
        (add-to-list 'compilation-finish-functions 'my/setup/c++/compilation-finished-hook)
        (global-set-key [(control ?x) ?l] 'my/setup/c++/compile-debug)
        (global-set-key [(control ?x) ?p] 'my/setup/c++/compile-pedantic)
        (global-set-key [(control ?x) ?m] 'my/setup/c++/compile-performance)
        (add-hook 'compilation-filter-hook 'my/ansi-colorize)
        (add-hook 'shell-filter-hook 'my/ansi-colorize)
        (add-hook 'compilation-mode-hook (lambda () (prefer-coding-system 'utf-8-unix)))
        (add-hook 'shell-mode-hook (lambda () (prefer-coding-system 'utf-8-unix)))
        ))
    )

  ;; (-each sirena-env-vars (lambda (item) (setenv (car item) (cdr item))))
  ;; (my/shell/add-to-env "PATH" (cdr (assoc "ORACLE_BIN" sirena-env-vars)))
  ;; (my/shell/add-to-env "LD_LIBRARY_PATH" (cdr (assoc "ORACLE_LIB" sirena-env-vars))))))

  (defun my/c-mode-hook()
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
                                       (innamespace          . 0)))))
    (c-set-style "mycodingstyle")
    (local-set-key (kbd "C-4") 'ff-find-other-file)
    (font-lock-add-keywords 'c++-mode
                            '(("constexpr" . 'font-lock-keyword-face)
                              ("auto" . 'font-lock-keyword-face)))

    (require 'irony)
    (require 'company-irony)

    (irony-cdb-autosetup-compile-options)
    ;; (define-key irony-mode-map [remap completion-at-point] 'helm-company)
    ;; (define-key irony-mode-map [remap complete-symbol] 'helm-company)
    ;; (add-to-list 'irony-cdb-search-directory-list ".." "../..")
    (add-to-list 'irony-additional-clang-options "-std=c++14")
    (add-to-list 'company-backends 'company-irony 'company-irony-c-headers)

    ;; Do not ask which command to run when call `compile` command.
    (setq shell-file-name "bash")
    (setq shell-command-switch "-ic")
    (setq compilation-read-command nil))

  (defun my/c-mode-file-extensions-hook ()
    (add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode)))

  (add-hook 'c-mode-common-hook 'my/c-mode-hook)
  (add-hook 'c++-mode-common-hook 'my/c-mode-hook)

  (add-hook 'c-mode-common-hook 'my/sirena/hook)
  (add-hook 'c++-mode-common-hook 'my/sirena/hook)

  (add-hook 'after-init-hook 'my/c-mode-file-extensions-hook)

  ;; (defun flymake-clang-c++-init ()
  ;;   (let* ((a1 '("-fsyntax-only" "-fno-color-diagnostics" "--std=c++11" "-fPIC"))
  ;;          (a2 (get-project-include-path))
  ;;          (a3 (list (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))))
  ;;     (let ((args (apply 'append `(,a1 ,a2 ,a3))))
  ;;       (list "clang++" args))))

  ;; (defun flymake-clang-c++-load ()
  ;;   (interactive)
  ;;   (unless (eq buffer-file-name nil)
  ;;     (add-to-list 'flymake-allowed-file-name-masks
  ;;                  '("\\.cpp\\'" flymake-clang-c++-init))
  ;;     (add-to-list 'flymake-allowed-file-name-masks
  ;;                  '("\\.cc\\'" flymake-clang-c++-init))
  ;;     (add-to-list 'flymake-allowed-file-name-masks
  ;;                  '("\\.h\\'" flymake-clang-c++-init))
  ;;     (add-to-list 'flymake-allowed-file-name-masks
  ;;                  '("\\.hpp\\'" flymake-clang-c++-init)))
  ;;   (flymake-mode t))

  ;; (defun flymake-mode-enable ()
  ;;   (provide 'flymake-clang-c++)
  ;;   (flymake-mode 1)
  ;;   )
)

(defun my/setup/python ()
  (setq python-shell-interpreter "python3.12")
  (add-to-list 'interpreter-mode-alist '("python3.12" . python-mode))
  (setq py-python-command "python3.12")
  (setq py-default-interpreter "python3.12")
  (setq python-python-command "python3.12")
  (setq py-shell-local-path "/usr/local/bin/python3.12" py-use-local-default t)
  (setq python-shell-exec-path "/usr/local/bin")

  (custom-set-variables
   '(flycheck-python-flake8-executable "python3.12")
   '(flycheck-python-pycompile-executable "python3.12")
   '(flycheck-python-pylint-executable "python3.12")
   '(flycheck-python-pycompile-executable "python3.12"))

  ;; (add-hook 'flycheck-mode-hook #'flycheck-virtualenv-setup)

  (use-package pyvenv
    :ensure t
    ;; :init
    ;; (setenv "WORKON_HOME" "~/.venvs/")
    :config
    (pyvenv-mode t)

    ;; Set correct Python interpreter
    (setq pyvenv-post-activate-hooks
          (list (lambda ()
                  (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
    (setq pyvenv-post-deactivate-hooks
          (list (lambda ()
                  (setq python-shell-interpreter "python3")))))

  ;; (use-package blacken
  ;;   :init
  ;;   (setq-default blacken-fast-unsafe t)
  ;;   (setq-default blacken-line-length 80)
  ;;   )

  (use-package python-mode
    :hook
    (python-mode . pyvenv-mode)
    (python-mode . flycheck-mode)
    (python-mode . company-mode)
    ;;(python-mode . blacken-mode)
    ;;(python-mode . yas-minor-mode)
    :custom
    ;; NOTE: Set these if Python 3 is called "python3" on your system!
    (python-shell-interpreter "python3.12")
    :config
    )

  ;; (add-hook 'python-mode-hook
  ;;           (lambda()
  ;;             (run-python "/usr/bin/python")))
  )

(defun my/setup/browser ()
  (with-system gnu/linux
    (setq browse-url-generic-program "firefox")
    (setq browse-url-browser-function 'browse-url-generic)
    (setq my/browser/executable "firefox"))

  (with-system darwin
    (setq browse-url-browser-function 'browse-url-default-macosx-browser)
    (setq my/browser/executable "/usr/bin/open -a Safari"))

  (defun my/browser/exists? (browser)
    (executable-find browser))

  (defun my/browser/open (url)
    (interactive "MBrowser url: ")
    (shell-command (s-concat my/browser/executable " http://" url))))

(defun my/setup/org ()
  (setq org-src-fontify-natively t)
  (setq org-startup-with-inline-images t))

(defun my/setup/command-history ()
  (setq-default savehist-additional-variables '(search-ring regexp-search-ring)
                savehist-file "~/.emacs.d/savehist")
  (setq history-length 25)
  (savehist-mode t))

(defun my/setup/highlight ()
  (require 'highlight-symbol))

(defun my/setup/text-mode ()
  (defun text-mode-setup-hook ()
    ;; Turn on auto-fill mode in text buffers
    (turn-on-auto-fill)
    (setq colon-double-space t))

  ;; Set default buffer mode to Text mode for pleasure in raw editing.
  ;; major-mode uses when a no file extension, a shebang or something else exists.
  (setq-default major-mode 'text-mode)

  ;; Setup auto fill mode for automatically breaks a line if it's too wide.
  (add-hook 'text-mode-hook 'text-mode-setup-hook))

(defun my/setup/system-monitor ()
  (defun my/setup/system-monitor-hook ()
    (require 'symon)
    (symon-mode))

  ;;(add-hook 'after-init-hook 'my/setup/system-monitor-hook)
)

(defun my/setup/misc ()
  (setq tab-always-indent 'complete)
  (add-to-list 'completion-styles 'initials t)

  ;; delete-auto-save-files
  (setq-default delete-auto-save-files t)

  (setq-default vc-handled-backends '(SVN Git))

  ;; Remove trailing whitespaces
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Set alias of yes/no -> y/n
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Bury the *scratch* buffer, never kill it
  (defadvice kill-buffer (around kill-buffer-around-advice activate)
    (let ((buffer-to-kill (ad-get-arg 0)))
      (if (equal buffer-to-kill "*scratch*")
          (bury-buffer)
        ad-do-it)))

  (setq c++-tab-always-indent t)
  (setq company-clang-begin-after-member-access nil)
  (setq custom-safe-themes
    '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
  (setq default-tab-width 4)
  (setq echo-keystrokes 0.1)
  (setq eval-expression-print-length nil)
  (setq fill-column 80)
  (setq find-file-visit-truename nil)
  (setq font-use-system-font t)
  (setq gc-cons-threshold 20000000)
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "ru")
  (setq indent-tabs-mode nil)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-message t)
  (setq initial-major-mode (quote fundamental-mode))
  (setq jit-lock-chunk-size 1000)
  (setq jit-lock-defer-time 0.01)
  (setq jit-lock-stealth-load 100)
  (setq jit-lock-stealth-time 1)
  (setq large-file-warning-threshold (* 30 1024 1024))
  (setq line-move-visual t)
  (setq make-pointer-invisible nil)
  (setq mouse-sel-retain-highlight t)
  (setq mouse-wheel-follow-mouse t)
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-scroll-amount (quote (3 ((shift) . 1))))
  (setq query-replace-highlight t)
  (setq read-file-name-completion-ignore-case t)
  (setq require-final-newline t)
  (setq ring-bell-function (lambda nil))
  (setq scheme-program-name "guile-2.0")
  (setq scroll-step 1)
  (setq search-highlight t)
  (setq sentence-end-double-space nil)
  (setq show-trailing-whitespace t)
  (setq tab-width 4)
  (setq user-full-name "Artem Pushkin")
  (setq user-mail-address "idfumg@gmail.com")
  (setq visible-bell t)
  (setq imenu-auto-rescan t) ;; Make sure auto automatically rescan for imenu changes

  (with-system darwin
    (setq mac-command-modifier 'meta))

  (setq x-select-enable-clipboard t)
  (setq use-dialog-box nil)

  ;; startup Emacs in the fullscreen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (setq global-auto-revert-non-file-buffers t)
)

(defun my/setup/helm ()
  (defun my/setup/helm-hook ()
    (require 'helm)
    (require 'helm-config)

    (setq helm-locate-fuzzy-match t)
    (helm-mode t)

    (setq
     helm-split-window-in-side-p           t
                                        ; open helm buffer inside current window,
                                        ; not occupy whole other window
     helm-move-to-line-cycle-in-source     t
                                        ; move to end or beginning of source when
                                        ; reaching top or bottom of source.
     helm-ff-search-library-in-sexp        t
                                        ; search for library in `require' and `declare-function' sexp.
     helm-scroll-amount                    8
                                        ; scroll 8 lines other window using M-<next>/M-<prior>
     helm-ff-file-name-history-use-recentf t
     ;; Allow fuzzy matches in helm semantic
     helm-semantic-fuzzy-match t
     helm-imenu-fuzzy-match    t)

    ;; Have helm automaticaly resize the window
    (helm-autoresize-mode 1)
    (setq rtags-use-helm t)

    ;; (advice-add 'helm-ff-filter-candidate-one-by-one
    ;;             :around (lambda (fcn file)
    ;;                       (unless (string-match "\\.*\\.o$" file)
    ;;                         (funcall fcn file))))
    )

  (add-hook 'after-init-hook 'my/setup/helm-hook)
)

(defun my/setup/neotree ()
  (defun my/setup/neotree-hook ()
    (require 'neotree)

    (setq neo-smart-open t)
    (setq neo-autorefresh t)
    (setq neo-show-hidden-files t))

  (add-hook 'after-init-hook 'my/setup/neotree-hook))

(defun my/setup/vlf ()
  "Open huge files quickly."
  (defun my/setup/vlf-hook ()
    (require 'vlf)
    ;; (setq vlf-application 'dont-ask)
    )

  (add-hook 'after-init-hook 'my/setup/vlf-hook))

(defun my/setup/server ()
  (defun my/setup/server-hook ()
    (unless (window-system))
      (require 'server)
      (unless (server-running-p server-name)
        (server-start)))

  (add-hook 'after-init-hook 'my/setup/server-hook))

(defun my/setup/smooth-scrolling ()
  (defun my/setup/smooth-scrolling-hook ()
    (require 'smooth-scrolling)
    (setq smooth-scroll-margin 4))

  (add-hook 'after-init-hook 'my/setup/smooth-scrolling-hook))

(defun my/setup/multiple-cursors ()
  (defun my/setup/multiple-cursors-hook ()
    (require 'multiple-cursors)
    (require 'phi-search)
    (require 'phi-search-mc)
    (require 'mc-extras))

  (add-hook 'after-init-hook 'my/setup/multiple-cursors-hook))

(defun my/setup/company ()
  (defun my/setup/company-hook ()
    (require 'company)
    (require 'helm-company)

    (define-key company-active-map [(control ?n)] 'company-select-next-or-abort)
    (define-key company-active-map [(control ?p)] 'company-select-previous-or-abort)

    (setq company-idle-delay 0)
    (company-statistics-mode)
    (global-company-mode)

    ;; (require 'company-tabnine)
    ;; (add-to-list 'company-backends #'company-tabnine)
    (setq company-idle-delay 0)
    (setq company-show-numbers t))

  (add-hook 'after-init-hook 'my/setup/company-hook))

(defun my/setup/eshell ()
  (defun my/setup/eshell-prompt-function ()
    (concat
     (propertize "\n[" 'face nil)
     (propertize (format-time-string "%X" (current-time)) 'face '(:foreground "#859900"))
     (propertize " | " 'face nil)
     (propertize (eshell/pwd) 'face '(:foreground "#859900"))
     (propertize "]\n" 'face nil)))

  (defun my/setup/eshell-hook ()
    (setq eshell-prefer-lisp-functions t)
    (local-set-key [(meta ?p)] 'eshell-previous-input)
    (local-set-key [(meta ?n)] 'eshell-next-input))

  (defun my/eshell/clear ()
    (interactive)
    (with-current-buffer "*eshell*"
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-emit-prompt))))

  (setq eshell-prompt-function 'my/setup/eshell-prompt-function)
  (add-hook 'eshell-mode-hook 'my/setup/eshell-hook))

(defun my/setup/locate ()
  (defun my/locate/sirena-update ()
    (interactive)
    (with-temp-buffer
      ;;(cd "/sudo::/")
      (message "Running updatedb for locate...")
      (async-shell-command "updatedb -o /home/idfumg/work/sirena-locate.db -v -l 0 -U /home/idfumg -n .git -n .svn -n externallibs -n alter -n bases -n Downloads -n Documents -n Desktop -n Music -n Pictures -n Templates -n Videos -n Public -n .cache -n .ccache -n .cmake -n .dropbox -n .dropbox-dist -n .mozilla -n .rpmdb -n .subversion -n .thunderbird -n .config")))

  (setq helm-locate-command "locate %s -e -A --regex %s -d /home/idfumg/work/sirena-locate.db"))

(defun my/setup/lua ()
  (defun my/setup/lua-hook ()
    (add-to-list 'company-backends 'company-lua)
    (require 'company-lua)
    (lua-start-process)
    (global-set-key [(control ?x) ?e] 'lua-send-current-line)
    (global-set-key [(control ?x) ?r] 'lua-send-region)
    (global-set-key [(control ?x) ?a] 'lua-send-buffer))

  (add-hook 'lua-mode-hook 'my/setup/lua-hook))

(defun my/setup/popup ()
  (require 'popup)

  (defun my/setup/my-popup-menu ()
    (interactive)

    (-let* ((menu-sirena-rail-make (popup-make-item "rail-make" :value 'SirenaRailMake))
            (menu-sirena-rail-tests (popup-make-item "rail-tests" :value 'SirenaRailTests))
            (menu-sirena-rail-rebuild (popup-make-item "rail-rebuild" :value 'SirenaRailRebuild))
            (menu-sirena-obrzap-make (popup-make-item "obrzap-make" :value 'SirenaObrzapMake))
            (menu-sirena-rail-test (popup-make-item "rail-test" :value 'SirenaRailTest))
            (menu-sirena-posauth-tests (popup-make-item "posauth-tests" :value 'SirenaPosAuthTests))
            (menu-sirena-airimp-tests (popup-make-item "airimp-tests" :value 'SirenaAirimpTests))
            (menu-sirena-emd-tests (popup-make-item "emd-tests" :value 'SirenaEmdTests))
            (menu-sirena-posauth-make (popup-make-item "posauth-make" :value 'SirenaPosAuthMake))
            (menu-sirena-airimp-make (popup-make-item "airimp-make" :value 'SirenaAirimpMake))
            (menu-sirena-emd-make (popup-make-item "emd-make" :value 'SirenaEmdMake))
            (menu-sirena-sirenalibs-make (popup-make-item "sirenalibs-make" :value 'SirenaSirenalibsMake))
            (menu-sirena-serverlib-make (popup-make-item "serverlib-make" :value 'SirenaServerlibMake))
            (menu-sirena-open-trunk (popup-make-item "open-trunk" :value 'SirenaOpenTrunk))
            (menu-sirena-open-stable (popup-make-item "open-stable" :value 'SirenaOpenStable))
            (menu-sirena-redmine-cut-tasks-names (popup-make-item "redmine-cut-tasks-names" :value 'SirenaRedmineTasks))

            (menu-sirena (list "sirena"
                               menu-sirena-rail-make
                               menu-sirena-rail-tests
                               menu-sirena-rail-rebuild
                               menu-sirena-obrzap-make
                               menu-sirena-rail-test
                               menu-sirena-posauth-tests
                               menu-sirena-airimp-tests
                               menu-sirena-emd-tests
                               menu-sirena-posauth-make
                               menu-sirena-airimp-make
                               menu-sirena-emd-make
                               menu-sirena-sirenalibs-make
                               menu-sirena-serverlib-make
                               menu-sirena-open-trunk
                               menu-sirena-open-stable
                               menu-sirena-redmine-cut-tasks-names))

            (menu-gtags-update (popup-make-item "update" :value 'GtagsUpdate))
            (menu-gtags-create (popup-make-item "create" :value 'GtagsCreate))
            (menu-gtags-create-sirena (popup-make-item "create-sirena" :value 'GtagsCreateSirena))

            (menu-gtags (list "gtags"
                              menu-gtags-update
                              menu-gtags-create
                              menu-gtags-create-sirena))

            (menu-browser-search (popup-make-item "search" :value 'BrowserSearch))
            (menu-browser-open (popup-make-item "open" :value 'BrowserOpen))

            (menu-browser (list "browser"
                                menu-browser-search
                                menu-browser-open))

            (menu-system-monitor (popup-make-item "system-monitor" :value 'SystemMonitor))
            (menu-sudo-open-file (popup-make-item "sudo-open-file" :value 'SudoOpenFile))
            (menu-kill-all-other-buffers (popup-make-item "kill-all-other-buffers" :value 'KillAllOtherBuffers))
            (menu-replace-in-file (popup-make-item "replace-in-file" :value 'ReplaceInFile))
            (menu-replace-in-files (popup-make-item "replace-in-files" :value 'ReplaceInFiles))
            (menu-recompile-emacs-files (popup-make-item "recompile-emacs-files" :value 'RecompileEmacsFiles))
            (menu-json-beautify (popup-make-item "json-beautify" :value 'JsonBeautify))
            (menu-xml-beautify (popup-make-item "xml-beautify" :value 'XmlBeautify))
            (menu-external-ip (popup-make-item "external-ip" :value 'ExternalIP))
            (menu-shorten-url (popup-make-item "shorten-url" :value 'ShortenUrl))
            (menu-filter-buffer-contents (popup-make-item "filter-buffer-contents" :value 'FilterBufferContents))
            (menu-open-all-org (popup-make-item "open-all-org" :value 'OpenAllOrg))
            (menu-open-algos (popup-make-item "open-algos" :value 'OpenAlgos))
            (menu-open-dot-emacs (popup-make-item "open-dot-emacs" :value 'OpenDotEmacs))
            (menu-open-dot-bashrc (popup-make-item "open-dot-bashrc" :value 'OpenDotBashrc))
            (menu-open-synopsis-python (popup-make-item "open-synopsis-python" :value 'OpenSynopsisPython))
            (menu-open-synopsis-js (popup-make-item "open-synopsis-js" :value 'OpenSynopsisJS))
            (menu-open-synopsis-lua (popup-make-item "open-synopsis-lua" :value 'OpenSynopsisLua))
            (menu-open-synopsis-elixir (popup-make-item "open-synopsis-elixir" :value 'OpenSynopsisElixir))

            (menu-utils (list "utils"
                              menu-open-all-org
                              menu-open-algos
                              menu-open-dot-emacs
                              menu-open-dot-bashrc
                              menu-open-synopsis-python
                              menu-open-synopsis-js
                              menu-open-synopsis-lua
                              menu-open-synopsis-elixir
                              menu-kill-all-other-buffers
                              menu-sudo-open-file
                              menu-system-monitor
                              menu-replace-in-file
                              menu-replace-in-files
                              menu-recompile-emacs-files
                              menu-json-beautify
                              menu-xml-beautify
                              menu-external-ip
                              menu-shorten-url
                              menu-filter-buffer-contents))

            (result (popup-cascade-menu (list menu-sirena
                                              menu-gtags
                                              menu-browser
                                              menu-utils))))

      (pcase result
        ('SirenaRailMake (my/sirena/rail-make))
        ('SirenaRailTests (my/sirena/rail-tests))
        ('SirenaRailRebuild (my/sirena/rail-rebuild))
        ('SirenaObrzapMake (my/sirena/obrzap-make))
        ('SirenaRailTest (my/sirena/rail-test))
        ('SirenaPosAuthTests (my/sirena/posauth-test))
        ('SirenaAirimpTests (my/sirena/airimp-test))
        ('SirenaEmdTests (my/sirena/emd-test))
        ('SirenaPosAuthMake (my/sirena/posauth-make))
        ('SirenaAirimpMake (my/sirena/airimp-make))
        ('SirenaEmdMake (my/sirena/emd-make))
        ('SirenaSirenalibsMake (my/sirena/sirenalibs-make))
        ('SirenaServerlibMake (my/sirena/serverlib-make))
        ('SirenaOpenTrunk (my/sirena/open-trunk))
        ('SirenaOpenStable (my/sirena/open-stable))
        ('SirenaRedmineTasks (call-interactively 'my/sirena/redmine/cut-tasks-names))

        ('GtagsUpdate (call-interactively 'my/gtags/update))
        ('GtagsCreate (call-interactively 'my/gtags/regenerate))
        ('GtagsCreateSirena (call-interactively 'my/gtags/regenerate-sirena))

        ('BrowserSearch (call-interactively 'my/browser/search))
        ('BrowserOpen (call-interactively 'my/browser/open))

        ('SystemMonitor (symon-mode (if symon-mode nil t)))
        ('SudoOpenFile (call-interactively 'my/sudo-open-file))
        ('KillAllOtherBuffers (my/kill-all-other-buffers))
        ('ReplaceInFile (call-interactively 'my/replace-regexp-in-file))
        ('ReplaceInFiles (call-interactively 'my/replace-regexp-in-files))
        ('RecompileEmacsFiles 'my/compile/byte-recompile-init-files)
        ('JsonBeautify (call-interactively 'my/json/prettify))
        ('XmlBeautify (call-interactively 'my/xml/prettify))
        ('ExternalIP (my/get-external-ip))
        ('ShortenUrl (call-interactively 'my/shorten-url))
        ('FilterBufferContents (call-interactively 'my/filter-buffer-contents))
        ('OpenAllOrg (my/open-all-org))
        ('OpenAlgos (my/open-algos))
        ('OpenDotEmacs (my/open-dot-emacs))
        ('OpenDotBashrc (my/open-dot-bashrc))
        ('OpenSynopsisPython (my/open-synopsis-python))
        ('OpenSynopsisJS (my/open-synopsis-js))
        ('OpenSynopsisLua (my/open-synopsis-lua))
        ('OpenSynopsisElixir (my/open-synopsis-elixir))
        )

      t))

  (global-set-key [(control ?c) ?q] 'my/setup/my-popup-menu))

(defun my/setup/elixir ()
  """
  https://alchemist.readthedocs.io/en/latest
  """

  (defun my/setup/elixir-hook ()
    (require 'alchemist)
    (alchemist-mode)
    ;;(add-to-list 'company-backends 'company-alchemist)
    (global-set-key [(control ?c) ?e] 'alchemist-eval-current-line)
    (global-set-key [(control ?c) ?r] 'alchemist-eval-region)
    (global-set-key [(control ?c) ?b] 'alchemist-eval-buffer))

  (add-hook 'elixir-mode-hook 'my/setup/elixir-hook))

(defun my/setup/golang ()
  """
  https://geeksocket.in/posts/emacs-lsp-go/
  """
  ;; Company mode
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)

  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save)
    (setq tab-width 4 indent-tabs-mode 1))
  (add-hook 'go-mode-hook 'my-go-mode-hook)
  )

(defun my/setup/copilot ()
  ;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/

  (use-package copilot
    :load-path (lambda () (expand-file-name "copilot.el" user-emacs-directory))
    ;; don't show in mode line
    :diminish)

  ;; (use-package copilot
  ;;   :quelpa (copilot :fetcher github
  ;;                    :repo "zerolfx/copilot.el"
  ;;                    :branch "main"
  ;;                    :files ("dist" "*.el")))

  (defun rk/no-copilot-mode ()
    "Helper for `rk/no-copilot-modes'."
    (copilot-mode -1))

  (defvar rk/no-copilot-modes '(shell-mode
                                inferior-python-mode
                                eshell-mode
                                term-mode
                                vterm-mode
                                comint-mode
                                compilation-mode
                                debugger-mode
                                dired-mode-hook
                                compilation-mode-hook
                                flutter-mode-hook
                                minibuffer-mode-hook)
    "Modes in which copilot is inconvenient.")

  (defun rk/copilot-disable-predicate ()
    "When copilot should not automatically show completions."
    (or rk/copilot-manual-mode
        (member major-mode rk/no-copilot-modes)
        (company--active-p)))

  (add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)

  (defvar rk/copilot-manual-mode nil
    "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

  (defun rk/copilot-change-activation ()
    "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
    (interactive)
    (if (and copilot-mode rk/copilot-manual-mode)
        (progn
          (message "deactivating copilot")
          (global-copilot-mode -1)
          (setq rk/copilot-manual-mode nil))
      (if copilot-mode
          (progn
            (message "activating copilot manual mode")
            (setq rk/copilot-manual-mode t))
        (message "activating copilot mode")
        (global-copilot-mode))))

  (define-key global-map (kbd "M-C-<escape>") #'rk/copilot-change-activation)

  ;; M-C-<escape> will now cycle between three states automatic, manual and off.

  (defun rk/copilot-complete-or-accept ()
    "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
    (interactive)
    (if (copilot--overlay-visible)
        (progn
          (copilot-accept-completion)
          (open-line 1)
          (next-line))
      (copilot-complete)))

  (define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion)
  (define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
  (define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)
  (define-key global-map (kbd "M-C-<return>") #'rk/copilot-complete-or-accept)

  (defun rk/copilot-tab ()
    "Tab command that will complet with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
    (interactive)
    (or (copilot-accept-completion)
        ;;(company-yasnippet-or-completion)
        (indent-for-tab-command)))

  (define-key global-map (kbd "<tab>") #'rk/copilot-tab)

  (defun rk/copilot-quit ()
    "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
    (interactive)
    (condition-case err
        (when copilot--overlay
          (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
            (setq copilot-disable-predicates (list (lambda () t)))
            (copilot-clear-overlay)
            (run-with-idle-timer
             1.0
             nil
             (lambda ()
               (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
      (error handler)))

  (advice-add 'keyboard-quit :before #'rk/copilot-quit)
  )

(defun my/setup/lsp ()
  (add-hook 'c-mode-common-hook #'lsp-deferred)
  (add-hook 'c++-mode-common-hook #'lsp-deferred)

  ;; Go - lsp-mode
  ;; Set up before-save hooks to format buffer and add/delete imports.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook #'lsp-deferred)

  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'js-mode-hook #'lsp-deferred)
  (add-hook 'javascript-mode-hook #'lsp-deferred)
  (add-hook 'rust-mode-hook #'lsp-deferred)

  (use-package lsp-mode
    :ensure t
    :commands (lsp lsp-deffered)
    :hook ((go-mode . lsp-deferred))
    :config
    ;;(lsp-enable-which-key-integration t)
    (setq lsp-prefer-flymake nil))

  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)
)

(defun my/setup/flycheck ()
  (add-hook 'c-mode-common-hook 'flycheck-mode)
  (add-hook 'c++-mode-common-hook 'flycheck-mode)
  (add-hook 'python-mode-common-hook 'flycheck-mode)
  (add-hook 'go-mode-common-hook 'flycheck-mode)
  (add-hook 'js-mode-hook 'flycheck-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (eval-after-load 'flycheck

    '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))
)

(defun my/setup/doom-modeline ()
  (doom-modeline-mode 1)
  (setq doom-modeline-height 15)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  ;; (setq doom-modeline-icon nil)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-number-limit 99)
  (setq doom-modeline-vcs-max-length 12)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)
  (setq doom-modeline-env-python-executable "python3.12") ; or `python-shell-interpreter'
  (setq doom-modeline-python-executable "python3.12")
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc"))

(defun my/setup/row-numbers ()
  ;; column and row numbers snippet
  (column-number-mode)
  (global-display-line-numbers-mode 1)

  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
  )

(defun main ()
  (my/setup/packages)

  (my/env/load)
  (my/setup/font-size)

  ;; (require 's-buffer)
  (require 'dash)

  (my/setup/misc)
  (my/setup/modes)
  (my/setup/font)

  (with-x11-mode
   ;; (my/setup/theme-backup)
   (my/setup/theme))

  (with-terminal-mode
   (my/setup/theme-console)
   (require 'mouse)
   (xterm-mouse-mode t)
   (defun track-mouse (e))
   (setq mouse-sel-mode t))

  ;; (my/setup/doom-modeline)
  (my/setup/encodings)
  (my/setup/c++)
  (my/setup/browser)
  (my/setup/org)
  (my/setup/command-history)
  (my/setup/highlight)
  (my/setup/text-mode)
  (my/setup/system-monitor)
  ;; (my/setup/vlf)
  ;; (my/setup/helm)
  (my/setup/neotree)
  (my/setup/server)
  (my/setup/smooth-scrolling)
  (my/setup/multiple-cursors)
  (my/setup/company)
  (my/setup/keys)
  (my/setup/eshell)
  (my/setup/sirena)
  (my/setup/locate)
  (my/setup/lua)
  (my/setup/popup)
  (my/setup/elixir)
  (my/setup/golang)
  (my/setup/copilot)
  (my/setup/lsp)
  (my/setup/flycheck)
  (my/setup/row-numbers)
  (my/setup/python)
)

(main)

(defun my/gtags/regenerate (directory)
  "sudo apt-get install global"
  (interactive "DChoose directory: ")
  (-let [default-directory-saved default-directory]
    (cd directory)
    (message "\nGlobal database creating...\n")
    (message (shell-command-to-string "gtags"))
    (message "Done.")
    (cd default-directory-saved)))

(defun my/gtags/update (directory)
  (interactive "DChoose directory: ")
  (-let [default-directory-saved default-directory]
    (cd directory)
    (message "\nIncremental global database update...\n")
    (message (shell-command-to-string "global -vu"))
    (message "Done")
    (cd default-directory-saved)))

(defun my/gtags/regenerate-sirena (directory)
  (interactive "DChoose directory: ")
  (-let ((default-directory-saved default-directory)
         (cmd "find . -path ./externallibs -prune -o -name \"*.[cChHpP]*\" -print > gtags.files")
         (gtags-cmd "gtags"))
    (cd directory)
    (message "\nFind sirena c/c++ files in `%s`\n" directory)
    (message (shell-command-to-string cmd))
    (message "Global database creating...\n")
    (message (shell-command-to-string gtags-cmd))
    (message "Done.")
    (cd default-directory-saved)))

(defun my/json/prettify (start end)
  (interactive "r")
  (shell-command-on-region start
                           end
                           "python -mjson.tool"
                           (current-buffer)
                           t))

(defun my/xml/prettify (start end)
  (interactive "r")
  (shell-command-on-region start
                           end
                           "python -c 'import sys;import xml.dom.minidom;s=sys.stdin.read();print(xml.dom.minidom.parseString(s).toprettyxml())'"
                           (current-buffer)
                           t))

(defun my/compile/byte-recompile-init-files ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))

(defun my/tooltip/show (msg &optional msg-type)
  (interactive "MEnter text for tooltip: ")
  (let* ((msg-color (if msg-type msg-type "white"))
         (msg-colorized (propertize msg 'face (list :foreground msg-color))))
    (tooltip-show msg-colorized (not (display-graphic-p)))))

(defun my/vc/root? (directory)
  (interactive "DChoose directory: ")

  (unless directory
    (error "Error! Directory is nil!"))

  (-let* ((known-vcs '("svn" "git"))
          (expanded-path (expand-file-name directory))
          (expanded-path-tail (if (s-equals? (last expanded-path) "/") "." "/."))
          (path (s-concat expanded-path expanded-path-tail))
          (vcs (-filter (lambda (vc) (file-directory-p (s-concat path vc))) known-vcs)))
    (-any? 'stringp vcs)))

(defun my/shell/add-to-env (name value &optional back?)
  (-if-let (current-value (getenv name))
      (unless (s-contains? value current-value)
        (-let [delimiter (if (s-blank? current-value) "" ":")]
          (if back?
              (setenv name (s-concat current-value delimiter value))
            (setenv name (s-concat value delimiter current-value)))))
    (setenv name value)))

(defun my/replace-regexp-in-file (filename from to)
  (interactive "fChoose file: \nMFrom: \nMTo: ")
  (with-temp-buffer
    (insert-file-contents filename)
    (while (re-search-forward from nil t)
      (replace-match to nil nil))
    (when (file-writable-p filename)
      (write-region (point-min) (point-max) filename))))

(defun my/replace-regexp-in-files (directory extensions from to)
  (interactive "DChoose directory: \nMFile extensions: \nMFrom: \nMTo: ")
  (-let ((extensions-quoted (regexp-quote extensions))
         (filenames (directory-files directory t extensions)))
    (-each filenames (lambda (filename)
                       (if (not (file-directory-p filename))
                           (my/replace-regexp-in-file filename from to))))))

(defun my/indent-file (filename)
  (interactive "fChoose file: ")
  (with-current-buffer (find-file filename)
    (unless buffer-read-only
      (indent-region (point-min) (point-max))
      (when (buffer-modified-p)
        (save-buffer))
    (kill-current-buffer))))

(defun my/indent-files (directory extensions)
  (interactive "DChoose directory: \nMFile extensions: ")
  (-let ((extensions-quoted (regexp-quote extensions))
         (filenames (directory-files directory t extensions)))
    (-each filenames (lambda (filename) (my/indent-file filename)))))

(defun my/sudo-open-file (filename)
  (interactive "fFilename: ")
  (find-file (s-concat "/sudo::" filename)))

(defmacro my/benchmark (&rest body)
  (require 'timeclock)

  (let ((start-time (current-time))
        (end-time nil))
    (unwind-protect
        (eval `(progn ,@body))
      (setq end-time (current-time)))

    (- (timeclock-time-to-seconds end-time)
       (timeclock-time-to-seconds start-time))))

;;(message ".emacs evaluation time: %f" (my/benchmark (main)))

(defun my/kill-all-other-buffers ()
  (interactive)
  (-map 'kill-buffer (-remove-item (current-buffer) (buffer-list))))

(defun my/get-external-ip ()

  (require 'request)
  (require 'json)

  (request

   "https://api.myip.com"

   :parser 'json-read

   :success
   (cl-function
    (lambda (&key data &allow-other-keys)
      (-let ((ip (cdr (assoc 'ip data)))
             (country (cdr (assoc 'country data)))
             (country-code (cdr (assoc 'cc data))))
        (message "IP: %S, Country: %S, Country code: %S" ip country country-code)
        (kill-new ip))))

   :status-code
   '((400 . (lambda (&rest _) (message "Got 400.")))
     (404 . (lambda (&rest _) (message "Got 404."))))))

(defun my/shorten-url (param)
  (interactive "MUrl: ")

  (require 'request)
  (require 'json)
  ;; (require 's-buffer)

  (-let [address (s-concat "u=" param)]
    (request

     "https://www.shorturl.at/url-shortener.php"

     :type "POST"

     :data address

     :headers
     '(("Cookie" . "__cfduid=d7a6ac158be8ec8661cabd5fd2e79afc21554471179; _ga=GA1.2.1204786437.1554471181; _gid=GA1.2.2009716306.1554471181; _gat=1")
       ("Content-Type" . "application/x-www-form-urlencoded")
       ("Referer" . "https://www.shorturl.at/")
       ("Host" . "www.shorturl.at"))

     :parser
     (lambda () (buffer-string))

     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        (with-temp-buffer
          (insert data)
          (beginning-of-buffer)
          (if (string-match "input.+value=\"\\(shorturl.at.+\\) onClick" data)
              (-let [result (s-concat "http://"
                                      (buffer-substring-no-properties (+ (match-beginning 1) 1)
                                                                      (match-end 1)))]
                (message "%s" result)
                (kill-new result))))))

     :status-code
     '((400 . (lambda (&rest _) (message "Got 400.")))
       (404 . (lambda (&rest _) (message "Got 404.")))))))

(defun my/filter-buffer-contents (start end pattern)
  (interactive "r\nMEnter pattern: ")
  (when pattern
    (save-restriction
      (narrow-to-region (or start (point-ming)) (or end (point-max)))
      (goto-char (point-min))
      (-let [result ""]
        (while (not (eobp))
          (-let [data (s-trim (buffer-substring (point) (progn (forward-line 1) (point))))]
            (when (s-contains? pattern data)
              (setq result (-> (-> "\n" (s-append data)) (s-append result))))))
        (delete-region (point-min) (point-max))
        (insert result)))))

(defun my/open-file (filename)
  (-let [absolute-filename (expand-file-name filename)]
    (if (not (file-exists-p absolute-filename))
        (error "Error! Your `%s' file doesn not exists!" absolute-filename)
      (find-file absolute-filename))))

(defun my/open-all-org ()
  (my/open-file "~/Dropbox/sync/development/all.org"))

(defun my/open-algos ()
  (my/open-file "~/1/github/algorithms_collection/template.hpp"))

(defun my/open-dot-emacs ()
  (my/open-file "~/.emacs"))

(defun my/open-dot-bashrc ()
  (my/open-file "~/.bashrc"))

(defun my/open-synopsis-python ()
  (my/open-file "~/1/github/PythonSynopsis/synopsis.py"))

(defun my/open-synopsis-js ()
  (my/open-file "~/1/github/JavaScriptSynopsis/1.js"))

(defun my/open-synopsis-lua ()
  (my/open-file "~/1/github/LuaSynopsis/1.lua"))

(defun my/open-synopsis-elixir ()
  (my/open-file "~/1/github/ElixirSynopsis/learn/common/0.ex"))

(defun my/pop-kill-ring ()
  (when kill-ring
    (setq kill-ring (cdr kill-ring)))
  )

(defun my/kill-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my/backward-kill-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun my/kill-line (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (line-end-position) (line-beginning-position))
  (backward-delete-char 1)
  (next-line))

(defun my/kill-sentence ()
  (interactive)
  (kill-sentence 1)
  (my/pop-kill-ring))

(defun my/create-scratch-buffer ()
   (interactive)
   (switch-to-buffer (get-buffer-create "*scratch*"))
   (lisp-interaction-mode))

(defun my/ssh (user host port)
  "Connect to a remote host by SSH."
  (interactive "sUser: \nsHost: \nsPort (default 22): ")
  (let* ((port (if (equal port "") "22" port))
         (switches (list host "-l" user "-p" port)))
    (set-buffer (apply 'make-term "ssh" "ssh" nil switches))
    (term-mode)
    (term-char-mode)
    (switch-to-buffer "*ssh*")))

(defun my/key-sequence-binding (sequence)
  (interactive "sSequence binding:")
  (edmacro-format-keys (kbd sequence) t))

(defun my/key-sequence-to-bytes ()
  (read-key-sequence-vector "Type key sequence:"))

(provide '.emacs)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-mode t)
 '(custom-safe-themes
   '("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(flycheck-python-flake8-executable "python3.12")
 '(flycheck-python-pycompile-executable "python3.12")
 '(flycheck-python-pylint-executable "python3.12")
 '(ivy-mode t)
 '(package-selected-packages
   '(drag-stuff flycheck-golangci-lint company-lsp python-mode blacken pyvenv rainbow-delimiters js2-mode helpful helm-fd flycheck dap-mode helm-lsp help-lsp lsp-treemacs company-mode editorconfig lsp-ui lsp-mode go-mode swiper-helm counsel swiper ivy use-package
                (doom-modeline-mode 1)
                doom-modeline nord-theme dotenv-mode cquery company-tabnine neotree yaml-mode treemacs symon solarized-theme smooth-scrolling s-buffer request phi-search-mc mc-extras load-env-vars highlight-symbol helm-projectile helm-gtags helm-company dockerfile-mode company-statistics company-lua company-irony company-c-headers company-anaconda alchemist ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)

;; ~/.emacs.d/eshell
;; alias .. cd .. $*
;; alias alert notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e 's/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//')" $*
;; alias egrep egrep --color=auto $*
;; alias fgrep fgrep --color=auto $*
;; alias grep grep --color=auto $*
;; alias l ls -CF $*
;; alias la ls -A $*
;; alias ll ls -alF $*
;; alias ls ls --color=auto $*

(add-to-list 'image-types 'svg)


(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package which-func
  :init (which-func-try-to-enable)
  :diminish which-func-mode
  :config
  (setq which-func-mode t))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
