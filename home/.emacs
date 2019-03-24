;; (add-to-list 'load-path "~/.emacs.d")

(defun my/setup/packages ()
  (unless (require 'package)
    (error "Error! Can't find 'package!"))

  (package-initialize)

  (let ((repos '(("marmalade" . "http://marmalade-repo.org/packages/")
                 ("elpa" . "http://tromey.com/elpa/")
                 ("melpa" . "http://melpa.milkbox.net/packages/"))))
    (dolist (repo repos)
      (unless (assoc (car repo) package-archives)
        (add-to-list 'package-archives repo t))))

  (let ((packages '(;; package management
                    ;; use-package

                    ;; hide modes from mode-line
                    ;; diminish

                    ;; auto-complete
                    company
                    company-statistics
                    company-irony
                    company-c-headers
                    irony

                    ;; python
                    anaconda-mode company-anaconda

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

                    ;; Projectile - working with projects
                    projectile helm-projectile

                    ;; test
                    helm-gtags helm-company

                    ;; grep -> ack -> ag
                    ;; https://github.com/ggreer/the_silver_searcher
                    ag

                    ;; org-mode
                    org

                    ;; theme
                    solarized-theme

                    ;; strings manipulations
                    ;; https://github.com/magnars/s.el
                    s-buffer

                    ;; lists manipulations
                    ;; https://github.com/magnars/dash.el
                    dash

                    ;; System resources monitor in the mode line when idle
                    ;; symon
                    )))

    (let ((package-list-was-refreshed? nil))
      (dolist (package packages)
        (unless (package-installed-p package)
          (unless package-list-was-refreshed?
            (package-refresh-contents)
            (setq package-list-was-refreshed? t))
          (message "Install package %s" package)
          (package-install package))))))

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
	                    :weight 'bold
	                    :height 130)

    (set-face-attribute 'position-face nil
	                    :inherit 'face
	                    :foreground position-color
                        :family "Menlo"
	                    :weight 'bold
	                    :height 110)

    (set-face-attribute 'major-mode-face nil
                        :inherit 'face
                        :foreground major-mode-color
	                    :height 120)

    (set-face-attribute 'minor-mode-face nil
                        :inherit 'mode-face
                        :foreground minor-mode-color
                        :height 120)

    (set-face-attribute 'very-long-line-face nil
                        :inherit 'position-face
	                    :family "Menlo"
	                    :weight 'bold
	                    :height 110
                        :foreground very-long-line-color
	                    :background "gray20")

    (set-face-attribute 'percent-position-face nil
	                    :inherit 'position-face
	                    :height 110
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
  (set-face-attribute 'default nil
                      :family "Liberation Mono"
                      :height 110
                      :weight 'normal
                      :width 'normal))

(defun my/setup/theme ()
  (load-theme 'solarized-dark)

  (my/setup/mode-line
   :read-only-color "green"
   :modified-color "red"
   :very-long-line-color "dark goldenrod"
   :position-color "gray50"
   :percent-position-color "gray50"
   :filename-color "dark goldenrod"
   :major-mode-color "gray50"
   :minor-mode-color "gray50"
   :background "black"
   :foreground "white")

  (my/setup/font)

  ;; Set cursor color
  (set-cursor-color "gray")

  ;; Set region foreground highlight color
  (set-face-foreground 'region nil)

  ;; Set region background highlight color
  (set-face-background 'region "DarkSlateGray"))

(defun my/setup/theme-console ()
  (load-theme 'solarized-dark)

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
   :foreground-color "dark goldenrod")

  (my/setup/font)

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
                  auto-image-file-mode))

  ;; Prevent add tabs when indent of text was used.
  (setq-default indent-tabs-mode nil))

(defun my/setup/keys ()
  (defun my/kill-back-to-indentation ()
    (interactive)
    (let ((prev-pos (point)))
      (back-to-indentation)
      (kill-region (point) prev-pos)))

  (global-unset-key [(control ?v)])
  (global-unset-key [(meta ?v)])

  (global-set-key [(meta ?w)] 'kill-ring-save)
  (global-set-key [(meta ?ц)] 'kill-ring-save)

  (global-set-key [(control ?y)] 'yank)
  (global-set-key [(control ?н)] 'yank)

  (global-set-key [(meta ?k)] 'kill-sentence)
  (global-set-key [(meta ?л)] 'kill-sentence)

  (global-set-key [(control ?k)] 'kill-line)
  (global-set-key [(control ?л)] 'kill-line)

  (global-set-key [(control meta ?e)] 'end-of-defun)
  (global-set-key [(control meta ?у)] 'end-of-defun)

  (global-set-key [(control meta ?f)] 'forward-sexp)
  (global-set-key [(control meta ?а)] 'forward-sexp)

  (global-set-key [(control meta ?b)] 'backward-sexp)
  (global-set-key [(control meta ?и)] 'backward-sexp)

  (global-set-key [(control ?p)] 'previous-line)
  (global-set-key [(control ?з)] 'previous-line)

  (global-set-key [(control ?n)] 'next-line)
  (global-set-key [(control ?т)] 'next-line)

  (global-set-key [(meta ?p)] 'scroll-down-line)
  (global-set-key [(meta ?з)] 'scroll-down-line)

  (global-set-key [(meta ?n)] 'scroll-up-line)
  (global-set-key [(meta ?т)] 'scroll-up-line)

  (global-set-key [(control meta ?p)] 'scroll-down-command)
  (global-set-key [(control meta ?з)] 'scroll-down-command)

  (global-set-key [(control meta ?n)] 'scroll-up-command)
  (global-set-key [(control meta ?т)] 'scroll-up-command)

  (global-set-key [(control ?d)] 'delete-forward-char)
  (global-set-key [(control ?в)] 'delete-forward-char)

  (global-set-key [(meta ?d)] 'kill-word)
  (global-set-key [(meta ?в)] 'kill-word)

  (global-set-key [(control ?b)] 'backward-char)
  (global-set-key [(control ?и)] 'backward-char)

  (global-set-key [(meta ?и)] 'backward-word)
  (global-set-key [(meta ?b)] 'backward-word)

  (global-set-key [(control ?f)] 'forward-char)
  (global-set-key [(control ?а)] 'forward-char)

  (global-set-key [(meta ?f)] 'forward-word)
  (global-set-key [(meta ?а)] 'forward-word)

  (global-set-key [(control ?a)] 'back-to-indentation)
  (global-set-key [(control ?ф)] 'back-to-indentation)

  (global-set-key [(control ?e)] 'move-end-of-line)
  (global-set-key [(control ?у)] 'move-end-of-line)

  (global-set-key [(meta ?g)] 'goto-line)
  (global-set-key [(meta ?п)] 'goto-line)

  (global-set-key [(meta ?-)] 'highlight-symbol-at-point)
  (global-set-key [(control ?=)] 'highlight-symbol-next)
  (global-set-key [(control ?-)] 'highlight-symbol-prev)
  (global-set-key [(control meta backspace)] 'my/kill-back-to-indentation)
  (global-set-key [enter] 'newline-and-indent)
  (global-set-key [(meta ?x)] 'helm-M-x)
  (global-set-key [(meta ?y)] 'helm-show-kill-ring)
  (global-set-key [(control ?x) ?b] 'helm-buffers-list)
  (global-set-key [(meta ?s)] 'helm-occur)
  (global-set-key [(control ?x) ?f] 'helm-locate)
  (global-set-key [(control ?x) (control ?f)] 'helm-find-files)
  (global-set-key [(control ?.)] 'helm-gtags-find-tag)
  (global-set-key [(meta ?.)] 'helm-gtags-find-rtag)
  (global-set-key [(control ?,)] 'helm-gtags-previous-history)
  (global-set-key [(control ?c) ?n ?t] 'neotree-toggle)
  (global-set-key [(control ?c) ?n ?r] 'neotree-rename-node)
  (global-set-key [(control ?c) ?n ?d] 'neotree-delete-node)
  (global-set-key [(control ?c) ?n ?n] 'neotree-create-node)
  (global-set-key [(control ?c) ?n ?c] 'neotree-copy-node)
  (global-set-key [(control ?c) ?m ?m] 'mc/mark-all-dwim)
  (global-set-key [(control ?c) ?m ?l] 'mc/edit-lines)
  (global-set-key [(control ?c) ?m ?n] 'mc/mark-next-like-this)
  (global-set-key [(control ?c) ?m ?p] 'mc/mark-previous-like-this)
  (global-set-key [(control ?c) ?m ?x] 'mc/mark-lines)
  (global-set-key [(control return)] 'company-complete))
  ;; (global-set-key [(control return)] 'helm-company))

(defun my/setup/c++ ()
  ;; Commands to checking the irony settings:
  ;; irony-cdb-autosetup-compile-options
  ;; irony-cdb-menu

  ;; Should be sit in the project root directory.
  ;; [.clang_complete contains]
  ;; -std=c++14
  ;; -Wall
  ;; -Wextra
  ;; -I.
  ;; -I..
  ;; -I/home/idfumg/work/trunk/src/
  ;; -I/home/idfumg/work/trunk/src/rail
  ;; -I/home/idfumg/work/trunk/src/basetables
  ;; -I/home/idfumg/work/trunk/externallibs/boost/include
  ;; -I/home/idfumg/work/trunk/externallibs/check/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/edilib/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/eticklib/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/jxtlib/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/libairimp/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/libcoretypes/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/libdcs/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/libjms/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/libnsi/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/librms/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/libssim/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/libtlg/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/libtypeb/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/serverlib/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/smsinfo/include
  ;; -I/home/idfumg/work/trunk/sirenalibs/typebparser/include
  ;; -I/oracle/product/db/precomp/public
  ;; -I/oracle/product/db/rdbms/demo
  ;; -I/oracle/product/db/rdbms/public
  ;; -I/oracle/product/db/rdbms/public
  ;; -I/usr/include
  ;; -I/usr/include/c++/6.1.1
  ;; -I/usr/include/c++/6.1.1/x86_64-pc-linux-gnu
  ;; -I/usr/include/c++/6.1.1/x86_64-pc-linux-gnu/backward
  ;; -I/usr/include/libxml2
  ;; -I/usr/include/openssl
  ;; -I/usr/lib/clang/3.8.1/include
  ;; -I/usr/local/include

  (defun my/sirena/open-trunk ()
    (interactive)
    (-let [path (getenv "SIRENA_TRUNK_PATH")]
      (if path
          (find-file (s-concat path "/src/rail/rail_order.cc"))
        (error "Error! No sirena trunk located!"))))

  (defun my/sirena/open-stable ()
    (interactive)
    (-let [path (getenv "SIRENA_STABLE_PATH")]
      (if path
          (find-file (s-concat path "/src/rail/rail_order.cc"))
        (error "Error! No sirena stable located!"))))

  (defun my/sirena/hook ()
    (defun my/sirena/sirena-svn? (directory)
      (-let [svn-url (shell-command-to-string (s-concat "svn info " directory " --show-item url"))]
        (s-contains? "svn+ssh://svn/SVNroot/sirena" svn-url)))

    (defun my/sirena/in-project-now? ()
      (interactive)
      (-when-let (svn-root (my/vc/get-root (file-name-directory (buffer-file-name))))
        (my/sirena/sirena-svn? svn-root)))

    (defun my/sirena/make-in (compilation-dir cmd)
      (unless (my/sirena/in-project-now?)
        (error "Error! You are not in the sirena project! (%s)" buffer-file-name))

      (-let* ((current-directory (file-name-directory (buffer-file-name)))
              (svn-root (my/vc/get-root current-directory)))
        (cd svn-root)
        (cd compilation-dir)
        (compile cmd)
        (cd current-directory)))

    (defun my/sirena/run-tests (domain &optional test-name)
      (-let [test-name (if (null test-name)
                           ""
                         (s-concat "." test-name))]
        (my/sirena/make-in "src" (s-concat "XP_LIST=" domain test-name " make xp-tests"))))

    (defun my/sirena/rail-tests ()
      (interactive)
      (my/sirena/run-tests "rail"))

    (defun my/sirena/rail-test (test-name)
      (interactive "MTest name: ")
      (my/sirena/run-tests "rail" test-name))

    (defun my/sirena/posauth-test ()
      (interactive)
      (my/sirena/run-tests "pos_auth"))

    (defun my/sirena/airimp-test ()
      (interactive)
      (my/sirena/run-tests "airimp"))

    (defun my/sirena/emd-test ()
      (interactive)
      (my/sirena/run-tests "emd"))

    (defun my/sirena/obrzap-make ()
      (interactive)
      (my/sirena/make-in "src" "make -sj12"))

    (defun my/sirena/rail-make ()
      (interactive)
      (my/sirena/make-in "src/rail" "make -sj12"))

    (defun my/sirena/rail-rebuild ()
      (interactive)
      (my/sirena/make-in "src/rail" "make clean && make -sj12"))

    (defun my/sirena/posauth-make ()
      (interactive)
      (my/sirena/make-in "src/pos_auth" "make -sj12"))

    (defun my/sirena/airimp-make ()
      (interactive)
      (my/sirena/make-in "src/airimp" "make -sj12"))

    (defun my/sirena/emd-make ()
      (interactive)
      (my/sirena/make-in "src/emd" "make -sj12"))

    (defun my/sirena/sirenalibs-make ()
      (interactive)
      (my/sirena/make-in "sirenalibs" "make -sj12"))

    (defun my/sirena/serverlib-make ()
      (interactive)
      (my/sirena/make-in "sirenalibs/serverlib" "make -sj12"))

    (defun my/sirena/compilation-finished-hook (buffer msg)
      (if (s-contains? "finished" msg)
          (progn
            (kill-buffer buffer)
            (my/tooltip/show "\n Compilation successful :-) \n" "green"))
        (my/tooltip/show "\n Compilation failed :-( \n" "red")))

    (-let [sirena-env-vars '(("ORACLE_BASE" . "/u01/app/oracle")
                             ("ORACLE_HOME" . "/u01/app/oracle/product/12.1.0/db_1")
                             ("ORACLE_BIN" . "/u01/app/oracle/product/12.1.0/db_1/bin")
                             ("ORACLE_LIB" . "/u01/app/oracle/product/12.1.0/db_1/lib")
                             ("ORACLE_SID" . "orcl")
                             ("ORACLE_INVENTORY" . "/u01/app/oracle/product/12.1.0/db_1/inventory")
                             ("NLS_LANG" . "AMERICAN_CIS.RU8PC866"))]

      (when (my/sirena/in-project-now?)
        (setq tooltip-hide-delay 2)
        (setq compilation-ask-about-save nil)
        (add-to-list 'compilation-finish-functions 'my/sirena/compilation-finished-hook)
        (global-set-key [(control ?x) ?m] 'my/sirena/rail-make)
        (add-hook 'compilation-mode-hook (lambda () (prefer-coding-system 'cp866)))
        (add-hook 'shell-mode-hook (lambda () (prefer-coding-system 'cp866)))
        (-each sirena-env-vars (lambda (item) (setenv (car item) (cdr item))))
        (my/shell/add-to-env "PATH" (cdr (assoc "ORACLE_BIN" sirena-env-vars)))
        (my/shell/add-to-env "LD_LIBRARY_PATH" (cdr (assoc "ORACLE_LIB" sirena-env-vars))))))

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
    (add-to-list 'company-backends 'company-irony 'company-c-headers)

    ;; Do not ask which command to run when call `compile` command.
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

  (add-hook 'after-init-hook 'my/c-mode-file-extensions-hook))

(defun my/setup/python ()
  ;; (add-hook 'python-mode-hook
  ;;           (lambda()
  ;;             (run-python "/usr/bin/python")))
  )

(defun my/setup/browser ()
  (setq-default browse-url-chromium-program "/usr/bin/chromium"
                browse-url-firefox-program "/usr/bin/firefox"
                browse-url-generic-program "/usr/bin/firefox"
                browse-url-browser-function 'browse-url-generic)

  (setq-local my/browser/executable "firefox")

  (defun my/browser/exists? (browser)
    (executable-find browser))

  (defun my/browser/search (search-string)
    (interactive "MBrowser search: ")
    (unless (my/browser/exists? my/browser/executable)
      (error "Error! %s does not exists in exec-path!" my/browser/executable))
    (shell-command (s-concat my/browser/executable " --search " search-string)))

  (defun my/browser/open (url)
    (interactive "MBrowser url: ")
    (unless (my/browser/exists? my/browser/executable)
      (error "Error! %s does not exists in exec-path!" my/browser/executable))
    (shell-command (s-concat my/browser/executable " " url))))

(defun my/setup/org ()
  (setq org-src-fontify-natively t)
  (setq org-startup-with-inline-images t))

(defun my/setup/command-history ()
  (setq-default savehist-additional-variables '(search-ring regexp-search-ring)
                savehist-file "~/.emacs.d/savehist"
                save-place t)
  (savehist-mode t))

(defun my/setup/encoding ()
  (setq-default default-input-method 'russian-computer)
  (set-language-environment 'UTF-8)
  (prefer-coding-system 'cp866)
  (prefer-coding-system 'utf-8-unix)
  ;; (set-default-coding-systems 'cp866)
  )

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

  (add-hook 'after-init-hook 'my/setup/system-monitor-hook))

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

  ;; startup Emacs in the fullscreen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  )

(defun my/setup/helm ()
  (defun my/setup/helm-hook ()
    (require 'helm)
    (require 'helm-config)

    (setq helm-locate-fuzzy-match t)
    (helm-mode t))

  (add-hook 'after-init-hook 'my/setup/helm-hook))

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
    (global-company-mode))

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
    (local-set-key [(meta ?з)] 'eshell-previous-input)
    (local-set-key [(meta ?n)] 'eshell-next-input)
    (local-set-key [(meta ?т)] 'eshell-next-input))

  (defun my/eshell/clear ()
    (interactive)
    (with-current-buffer "*eshell*"
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-emit-prompt))))

  (setq eshell-prompt-function 'my/setup/eshell-prompt-function)
  (add-hook 'eshell-mode-hook 'my/setup/eshell-hook))

(defun main ()
  (my/setup/packages)
  (require 's-buffer)
  (require 'dash)
  (my/setup/misc)
  (my/setup/modes)
  (if (display-graphic-p) (my/setup/theme) (my/setup/theme-console))
  (my/setup/c++)
  (my/setup/python)
  (my/setup/browser)
  (my/setup/org)
  (my/setup/command-history)
  (my/setup/encoding)
  (my/setup/highlight)
  (my/setup/text-mode)
  ;; (my/setup/system-monitor)
  ;; (my/setup/vlf)
  (my/setup/helm)
  (my/setup/neotree)
  (my/setup/server)
  (my/setup/smooth-scrolling)
  (my/setup/multiple-cursors)
  (my/setup/company)
  (my/setup/keys)
  (my/setup/eshell))

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

(defun my/json/beautify (start end)
  (interactive "r")
  (shell-command-on-region start
                           end
                           "python -mjson.tool"
                           (current-buffer)
                           t))

(defun my/xml/beautify (start end)
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
  (-let* ((known-vcs '("svn" "git"))
          (expanded-path (expand-file-name directory))
          (expanded-path-tail (if (s-equals? (last expanded-path) "/") "." "/."))
          (path (s-concat expanded-path expanded-path-tail))
          (vcs (-filter (lambda (vc) (file-directory-p (s-concat path vc))) known-vcs)))
    (-any? 'stringp vcs)))

(defun my/vc/get-root (directory)
  (interactive "DChoose directory: ")
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

(defun my/shell/add-to-env (name value &optional back?)
  (-if-let (current-value (getenv name))
      (unless (s-contains? value current-value)
        (-let [delimiter (if (s-blank? current-value) "" ":")]
          (if back?
              (setenv name (s-concat current-value delimiter value))
            (setenv name (s-concat value delimiter current-value)))))
    (setenv name value)))

(defun my/file/replace-regexp-in-file (filename from to)
  (interactive "fChoose directory: \nMFrom: \nMTo: ")
  (with-temp-buffer
    (insert-file-contents filename)
    (while (re-search-forward from nil t)
      (replace-match to nil nil))
    (when (file-writable-p filename)
      (write-region (point-min) (point-max) filename))))

(defun my/file/replace-regexp-in-files (directory extensions from to)
  (interactive "DChoose directory: \nMFile extensions: \nMFrom: \nMTo: ")
  (-let ((extensions-quoted (regexp-quote extensions))
         (filenames (directory-files directory t extensions)))
    (-each filenames (lambda (filename) (my/files/replace-regexp-in-file filename from to)))))

(defun my/file/sudo-open-file (filename)
  (interactive "fFilename: ")
  (find-file (s-concat "/sudo::" filename)))

(defmacro my/utils/benchmark (&rest body)
  (require 'timeclock)

  (let ((start-time (current-time))
        (end-time nil))
    (unwind-protect
        (eval `(progn ,@body))
      (setq end-time (current-time)))

    (- (timeclock-time-to-seconds end-time)
       (timeclock-time-to-seconds start-time))))

;;(message ".emacs evaluation time: %f" (my/utils/benchmark (main)))

(defun my/buffer/kill-all-other-buffers ()
  (interactive)
  (-map 'kill-buffer (-remove-item (current-buffer) (buffer-list))))

(provide '.emacs)
