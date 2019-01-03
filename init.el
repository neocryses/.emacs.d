;; init.el --- Emacs configuration file -*- lexical-binding: t -*-

;;; Commentary:

;; This is a personal configuration file for Emacs.

;;; Code:

;; Avoid unwanted package-initialize
(setq package--init-file-ensured t)

;; Remember the original value file name handler
(defvar prev-file-name-handler-alist file-name-handler-alist)

;; Startup optimization
(setq file-name-handler-alist nil
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Change back the file name handler and garbage collection to a sensible value
(add-hook 'after-init-hook
          (lambda () (setq file-name-handler-alist prev-file-name-handler-alist
                           ;; gc-cons-threshold 800000
                           gc-cons-threshold 16777216
                           gc-cons-percentage 0.1)))

;; There's a bug with the macOS's Emacs where it doesn't recognize SSL certificates.
;; To avoid this issue, libressl should be installed via Homebrew.
;; Following code will load certificates using libressl.
;; Alternative solution is to build Emacs with gnutls.
;; (when (and (eq system-type 'darwin)
;;            (not (gnutls-available-p)))
;;   (with-eval-after-load 'gnutls
;;     (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem")))

;; Load all .el files in a given directory
(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

;; Load all .el files in "conf" directory
(load-directory (expand-file-name "conf" user-emacs-directory))

;; Add local package directory to the load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (load "local" 'noerror)

;; Set package-enable-at-startup nil to avoid loading packages twice.
(setq package-enable-at-startup nil)

;; Set up the archive URL according to the availability of SSL.
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (setq package-archives `(("org" . ,(concat proto "://orgmode.org/elpa/"))
                           ("melpa" . ,(concat proto "://melpa.org/packages/"))
                           ("gnu" . ,(concat proto "://elpa.gnu.org/packages/")))))

(require 'package)
(package-initialize)

;; Install use-package if it doesn't exist
;; Following code is for installing use-package via package.el.
;; It is currently disabled as I'm using straight.el instead.
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

;; (use-package leuven-theme
;;   :ensure t
;;   :config
;;   (load-theme 'leuven t))

;; (defvar lawlist-redisplay-unhighlight-region-function
;;   (lambda (rol) (when (overlayp rol) (delete-overlay rol))))

;; (setq redisplay-highlight-region-function
;;       '(lambda (start end window rol)
;;          (if (not (overlayp rol))
;;              (let ((nrol (make-overlay start end)))
;;                (funcall lawlist-redisplay-unhighlight-region-function rol)
;;                (overlay-put nrol 'window window)
;;                (overlay-put nrol 'face 'region)
;;                (overlay-put nrol 'priority '(1002 . 1002))
;;                nrol)
;;            (unless (and (eq (overlay-buffer rol) (current-buffer))
;;                         (eq (overlay-start rol) start)
;;                         (eq (overlay-end rol) end))
;;              (move-overlay rol start end (current-buffer)))
;;            rol)))

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; (load-theme 'wombat t)

;; Disable unnecessary GUI elements
(menu-bar-mode -1)
(when (boundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Suppress some error messages
(defun supress-error (data context signal)
  "Suppress some of the unnecessary error messages"
  (when (not (memq (car data) '(beginning-of-line
                                beginning-of-buffer
                                end-of-line
                                end-of-buffer)))
    (command-error-default-function data context signal)))
(setq command-error-function 'supress-error)

;; Whether frames should be resized implicitly.
(setq-default frame-inhibit-implied-resize t)

;; Core packages
(use-package esup
  :ensure t)

(use-package no-littering
  :ensure t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package delight :ensure t)

(use-package general
  :ensure t
  :config
  (setq global-leader "SPC"
        major-mode-leader ","))

(use-package which-key
  :ensure t
  :defer 1
  :delight which-key-mode
  :commands which-key-mode
  :config
  (which-key-mode 1)
  :general
  (:states '(normal visual)
   :prefix global-leader
   ;; "" '(:ignore t :which-key "global prefix")
   "f" '(:ignore t :which-key "files")))

(use-package hydra
  :ensure t
  :defer t
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

(use-package cp5022x
  :ensure t
  :demand
  :config
  ;; Encoding settings
  (prefer-coding-system 'utf-8)
  ;; Windows specific encoding settings
  (when (eq system-type 'windows-nt)
    (set-file-name-coding-system 'cp932)
    (set-keyboard-coding-system 'cp932)
    (set-terminal-coding-system 'cp932))
  ;; Encoding priority
  (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                        'katakana-jisx0201 'iso-8859-1 'unicode)
  (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932))

;; Allow for location specific settings
(defvar home-computer '("Shos-MacBook.local")
  "List of home computer names")
(defvar work-computer '("IWA0030131")
  "List of work computer names")

;; Settings for Emacs' core features.

;; Set the startup behavior
(setq initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message "")

;; Make the scratch buffer persistent
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

;; Disable the site default settings. See (info "(emacs) Init File")
(setq inhibit-default-init t)

;; Time locale setting
;; Set to "C" for English time locale
(setq system-time-locale "C")

;; Always select the help window
(setq help-window-select t)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Avoid creating lockfiles
(setq create-lockfiles nil)

;; Indicate empty lines in a buffer
(setq-default indicate-empty-lines t)

;; Always display the width of the line
(setq line-number-display-limit nil
      line-number-display-limit-width 2000000)

;; Spacing between texts
(setq-default line-spacing 0.1)

;; Send the mouse clicks to emacs instead of the terminal.
(xterm-mouse-mode 1)

;; Mimic vim's scroll margin behavior
(setq scroll-conservatively 101
      scroll-margin 5
      scroll-preserve-screen-position t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))

;; Set the default indent width
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Prefer to delete files by moving to trash
(setq delete-by-moving-to-trash t)

;; Avoid having underlines too close to the characters
(setq x-underline-at-descent-line t)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; Set the font for macOS
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(font . "Ricty Discord-12"))
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super))

;; Set the font for Windows
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Myrica M-10")
  (set-frame-font "Myrica M-10" nil t)) 

;; Temporary fix for screen blinking while inserting text with ime for macOS
(when (eq system-type 'darwin)
  (defun enable-redisplay-dont-pause ()
    "Set the value of redisplay-dont-pause to t."
    (setq redisplay-dont-pause t))
  (defun disable-redisplay-dont-pause ()
    "Set the value of redisplay-dont-pause to nil."
    (setq redisplay-dont-pause nil))
  (add-hook 'evil-insert-state-entry-hook
            #'disable-redisplay-dont-pause)
  (add-hook 'evil-insert-state-exit-hook
            #'enable-redisplay-dont-pause))

;; Advice to show the last-command on the command line
;; (defadvice call-interactively (after show-last-command activate)
;;   "Shows the interactive command that was just run in the message area."
;;   (unless (eq major-mode 'minibuffer-inactive-mode)
;;     (message "Ran %s" (ad-get-arg 0))))

;; Packages
(use-package aggressive-indent
  :ensure t
  :delight aggressive-indent-mode
  :init
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :config
  (mapc (lambda (command)
          (add-to-list 'aggressive-indent-protected-commands command))
        '(evil-paste-after
          evil-paste-before
          evil-visual-paste)))

(use-package anzu
  :ensure t
  :defer t)

(use-package autorevert
  :ensure t
  :defer t
  :config
  (setq auto-revert-interval 0.5)
  (global-auto-revert-mode 1))

(use-package avy
  :ensure t
  :commands avy-goto-char
  :general
  (:states '(normal visual)
   :prefix global-leader
   "ss" 'avy-goto-char))

(use-package company
  :ensure t
  :delight company-mode
  :commands (company-mode company-indent-or-complete-common)
  :hook (prog-mode . company-mode)
  :general
  (:keymaps '(company-active-map)
   [tab] 'company-complete-common-or-cycle
   [backtab] 'company-select-previous
   "C-n" 'company-select-next
   "C-p" 'company-select-previous))

(use-package diff
  :ensure nil
  :defer t)

(use-package dired
  :ensure nil
  :defer t
  :commands dired
  :general
  (:keymaps '(dired-mode-map)
   :states '(normal)
   "q" 'quit-window
   "j" 'dired-next-line
   "k" 'dired-previous-line
   [mouse-2] 'dired-mouse-find-file-other-window
   [follow-link] 'mouse-face
   "#" 'dired-flag-auto-save-files
   "." 'dired-clean-directory
   "~" 'dired-flag-backup-files
   "A" 'dired-do-find-regexp
   "C" 'dired-async-do-copy
   "B" 'dired-do-byte-compile
   "D" 'dired-do-delete
   "gG" 'dired-do-chgrp
   "H" 'dired-async-do-hardlink
   "L" 'dired-do-load
   "M" 'dired-do-chmod
   "O" 'dired-do-chown
   "R" 'dired-async-do-rename
   "S" 'dired-async-do-symlink
   "T" 'dired-do-touch
   "X" 'dired-do-shell-command
   "Z" 'dired-do-compress
   "c" 'dired-do-compress-to
   "!" 'dired-do-shell-command
   "&" 'dired-do-async-shell-command
   "=" 'dired-diff
   ;; Tree Dired commands
   (kbd "M-C-?") 'dired-unmark-all-files
   (kbd "M-C-d") 'dired-tree-down
   (kbd "M-C-u") 'dired-tree-up
   (kbd "M-C-n") 'dired-next-subdir
   (kbd "M-C-p") 'dired-prev-subdir
   ;; move to marked files
   (kbd "M-{") 'dired-prev-marked-file
   (kbd "M-}") 'dired-next-marked-file
   ;; Make all regexp commands share a `%' prefix:
   ;; We used to get to the submap via a symbol dired-regexp-prefix,
   ;; but that seems to serve little purpose, and copy-keymap
   ;; does a better job without it.
   "%" nil
   "%u" 'dired-upcase
   "%l" 'dired-downcase
   "%d" 'dired-flag-files-regexp
   "%g" 'dired-mark-files-containing-regexp
   "%m" 'dired-mark-files-regexp
   "%r" 'dired-do-rename-regexp
   "%C" 'dired-do-copy-regexp
   "%H" 'dired-do-hardlink-regexp
   "%R" 'dired-do-rename-regexp
   "%S" 'dired-do-symlink-regexp
   "%&" 'dired-flag-garbage-files
   ;; mark
   "*" nil
   "**" 'dired-mark-executables
   "*/" 'dired-mark-directories
   "*@" 'dired-mark-symlinks
   "*%" 'dired-mark-files-regexp
   "*(" 'dired-mark-sexp
   "*." 'dired-mark-extension
   "*O" 'dired-mark-omitted
   "*c" 'dired-change-marks
   "*s" 'dired-mark-subdir-files
   "*m" 'dired-mark
   "*u" 'dired-unmark
   "*?" 'dired-unmark-all-files
   "*!" 'dired-unmark-all-marks
   "U" 'dired-unmark-all-marks
   (kbd "* <delete>") 'dired-unmark-backward
   (kbd "* C-n") 'dired-next-marked-file
   (kbd "* C-p") 'dired-prev-marked-file
   "*t" 'dired-toggle-marks
   ;; Lower keys for commands not operating on all the marked files
   "a" 'dired-find-alternate-file
   "d" 'dired-flag-file-deletion
   "gf" 'dired-find-file
   (kbd "C-m") 'dired-find-file
   "gr" 'revert-buffer
   "i" 'dired-toggle-read-only
   "I" 'dired-maybe-insert-subdir
   "J" 'dired-goto-file
   "K" 'dired-do-kill-lines
   "r" 'dired-do-redisplay
   "m" 'dired-mark
   "t" 'dired-toggle-marks
   "u" 'dired-unmark                    ; also "*u"
   "W" 'browse-url-of-dired-file
   "x" 'dired-do-flagged-delete
   "gy" 'dired-show-file-type ;; FIXME: This could probably go on a better key.
   "Y" 'dired-copy-filename-as-kill
   "+" 'dired-create-directory
   ;; open
   (kbd "<return>") 'dired-find-file
   (kbd "S-<return>") 'dired-find-file-other-window
   (kbd "M-<return>") 'dired-display-file
   "gO" 'dired-find-file-other-window
   "go" 'dired-view-file
   ;; sort
   "o" 'dired-sort-toggle-or-edit
   ;; moving
   "gj" 'dired-next-dirline
   "gk" 'dired-prev-dirline
   "[" 'dired-prev-dirline
   "]" 'dired-next-dirline
   "<" 'dired-prev-dirline
   ">" 'dired-next-dirline
   "^" 'dired-up-directory
   " " 'dired-next-line
   [?\S-\ ] 'dired-previous-line
   [remap next-line] 'dired-next-line
   [remap previous-line] 'dired-previous-line
   ;; hiding
   "g$" 'dired-hide-subdir ;; FIXME: This can probably live on a better binding.
   (kbd "M-$") 'dired-hide-all
   "(" 'dired-hide-details-mode
   ;; isearch
   (kbd "M-s a C-s")   'dired-do-isearch
   (kbd "M-s a M-C-s") 'dired-do-isearch-regexp
   (kbd "M-s f C-s")   'dired-isearch-filenames
   (kbd "M-s f M-C-s") 'dired-isearch-filenames-regexp
   ;; misc
   [remap read-only-mode] 'dired-toggle-read-only
   ;; `toggle-read-only' is an obsolete alias for `read-only-mode'
   [remap toggle-read-only] 'dired-toggle-read-only
   "g?" 'dired-summary
   (kbd "<delete>") 'dired-unmark-backward
   [remap undo] 'dired-undo
   [remap advertised-undo] 'dired-undo
   ;; thumbnail manipulation (image-dired)
   (kbd "C-t d") 'image-dired-display-thumbs
   (kbd "C-t t") 'image-dired-tag-files
   (kbd "C-t r") 'image-dired-delete-tag
   (kbd "C-t j") 'image-dired-jump-thumbnail-buffer
   (kbd "C-t i") 'image-dired-dired-display-image
   (kbd "C-t x") 'image-dired-dired-display-external
   (kbd "C-t a") 'image-dired-display-thumbs-append
   (kbd "C-t .") 'image-dired-display-thumb
   (kbd "C-t c") 'image-dired-dired-comment-files
   (kbd "C-t f") 'image-dired-mark-tagged-files
   (kbd "C-t C-t") 'image-dired-dired-toggle-marked-thumbs
   (kbd "C-t e") 'image-dired-dired-edit-comment-and-tags
   ;; encryption and decryption (epa-dired)
   ";d" 'epa-dired-do-decrypt
   ";v" 'epa-dired-do-verify
   ";s" 'epa-dired-do-sign
   ";e" 'epa-dired-do-encrypt))

(use-package dired-single
  :ensure t
  :after dired
  :general
  (:keymaps '(dired-mode-map)
   :states '(normal)
   [return] 'dired-single-buffer
   [mouse-1] 'dired-single-buffer-mouse
   "^" 'dired-single-up-directory))

(use-package dired-x
  :ensure nil
  :defer t
  :after dired)

(use-package display-line-numbers
  :ensure nil
  :unless (version< emacs-version "26.0")
  :hook ((prog-mode . display-line-numbers-mode)
         (org-mode . (lambda () (setq display-line-numbers 'visual)))
         (markdown-mode . (lambda () (setq display-line-numbers 'visual))))
  :init
  (setq display-line-numbers-grow-only t
        display-line-numbers-type 'relative)
  ;; (add-hook 'org-mode-hook
  ;;           (lambda () (setq display-line-numbers 'visual)))
  )

(use-package ediff
  :ensure nil
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package eldoc
  :ensure nil
  :delight eldoc-mode)

(use-package elisp-mode
  :ensure nil
  :config
  (defun emacs-lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  (this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ;; car of form doesn't seem to be a symbol, or is a keyword
       ((and (elt state 2)
             (or (not (looking-at "\\sw\\|\\s_"))
                 (looking-at ":")))
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
            (progn (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point)
                                       calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
       ((and (save-excursion
               (goto-char indent-point)
               (skip-syntax-forward " ")
               (not (looking-at ":")))
             (save-excursion
               (goto-char orig-point)
               (looking-at ":")))
        (save-excursion
          (goto-char (+ 2 (elt state 1)))
          (current-column)))
       (t
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (function-get (intern-soft function)
                                         'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state))))))))

  ;; Fix emacs-lisp indentation on keywords
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq-local lisp-indent-function #'emacs-lisp-indent-function))))

(use-package evil
  :ensure t
  :demand
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode 1)
  (add-hook 'c-mode-common-hook (lambda () (modify-syntax-entry ?_ "w")))

;;; esc quits
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  (defun supress-message (orig-fun &rest args)
    "Fix the handling of the coding for external commands"
    (let ((inhibit-message t))
      (apply orig-fun args)))
  (advice-add 'push-mark :around #'supress-message)

  ;; (defhydra hydra-search (:post (evil-ex-nohighlight))
  ;;   "search"
  ;;   ("/" evil-ex-search-forward)
  ;;   ("?" evil-ex-search-backward)
  ;;   ("n" evil-ex-search-next)
  ;;   ("N" evil-ex-search-previous))
  :general
  ;; Exit out with ESC
  (:states '(normal visual)
   [escape] 'keyboard-quit)
  (:keymaps '(minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map)
   [escape] 'minibuffer-keyboard-quit)

  ;; Hydra search
  ;; (:states '(normal visual)
  ;;  "/" #'hydra-search/evil-ex-search-forward
  ;;  "?" #'hydra-search/evil-ex-search-backward
  ;;  "n" #'hydra-search/evil-ex-search-next
  ;;  "N" #'hydra-search/evil-ex-search-previous)

  ;; Disable selection marking with "C-SPC"
  (:keymaps '(global)
   "C-SPC" nil)

  (:keymaps '(evil-ex-completion-map)
   "C-b" 'backward-char
   "C-a" 'beginning-of-line
   "C-e" 'end-of-line
   "C-k" 'kill-line)
  
  ;; Exit out of insert state with "jk"
  (:states '(insert)
   "j" (general-key-dispatch 'self-insert-command
         :timeout 0.25
         "k" 'evil-normal-state))

  (:states '(normal visual)
   :prefix global-leader
   "tn" 'evil-ex-nohighlight))

(use-package evil-anzu
  :ensure t
  :defer t)

(use-package evil-commentary
  :ensure t
  :delight evil-commentary-mode
  :hook (prog-mode . evil-commentary-mode))

(use-package evil-ediff
  :ensure t
  :after ediff
  :defer t
  :hook (ediff-mode . evil-ediff-init))

(use-package evil-indent-plus
  :ensure t
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-surround
  :ensure t
  :defer t
  :config
  (global-evil-surround-mode 1)
  :general
  (:states '(visual)
   "s" 'evil-surround-region))

(use-package evil-textobj-entire
  :ensure t)

(use-package evil-textobj-line
  :ensure t)

(use-package evil-visualstar
  :ensure t
  :defer t
  :config
  (global-evil-visualstar-mode))

(use-package evil-magit
  :ensure t
  :after magit)

(use-package evil-org
  :ensure t
  :after org
  :delight evil-org-mode
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . evil-org-set-key-theme))
  :init
  (setq evil-org-special-o/O '(table-row item))
  ;; (setf evil-org-key-theme '(textobjects insert navigation additional shift todo heading))
  (setf evil-org-key-theme '(navigation calendar additional shift return)))

(use-package ffap
  :ensure nil
  :defer t
  :config
  ;; Don't try to ping things that look like domain names
  (setq ffap-machine-p-known 'reject))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :config
  (defun disable-fylcheck-in-org-src-block ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (add-hook 'org-src-mode-hook #'disable-fylcheck-in-org-src-block))

(use-package flyspell
  :ensure t
  :defer t
  :commands (flyspell-mode flyspell-buffer))

(use-package helm
  :ensure t
  :defer t
  :delight helm-mode
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-display-header-line nil)
  (setq helm-split-window-inside-p t)
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)

  (set-face-attribute 'helm-source-header nil :height 1.0)

  (defvar helm-side-position 'bottom
    "Position to show the `helm' mini-buffer.")

  (defvar helm-display-help-buffer-rule '("*.*Helm.*Help.**"))
  (defvar helm-display-buffer-rule
    `("*.*helm.**"
      (display-buffer-in-side-window)
      (inhibit-same-window . t)
      (side . ,helm-side-position)
      (window-width . 0.6)
      (window-height . 0.4)))

  (defun display-helm-window (buffer &optional resume)
    "Sensible way to display the Helm window"
    (let ((display-buffer-alist
           (list helm-display-help-buffer-rule
                 helm-display-buffer-rule)))
      (helm-default-display-buffer buffer)))

  (setq helm-display-function 'display-helm-window)

  (defun hide-cursor-in-helm-buffer ()
    "Hide the cursor in helm buffers."
    (with-helm-buffer
      (setq cursor-in-non-selected-windows nil)))
  (add-hook 'helm-after-initialize-hook #'hide-cursor-in-helm-buffer)

  ;; Raise garbage collection threshold while minibuffer is open
  (defun minibuffer-setup-gc () (setq gc-cons-threshold 402653184
                                      gc-cons-percentage 0.6))
  (defun minibuffer-exit-gc () (setq gc-cons-threshold 800000
                                     gc-cons-percentage 0.1))
  (add-hook 'minibuffer-setup-hook #'minibuffer-setup-gc)
  (add-hook 'minibuffer-exit-hook #'minibuffer-exit-gc)

  :general
  ("M-x" 'helm-M-x)
  (:keymaps '(helm-map)
   "TAB" 'helm-maybe-exit-minibuffer
   "C-h" 'helm-next-source
   "C-j" 'helm-next-line
   "C-k" 'helm-previous-line
   "C-l" 'helm-maybe-exit-minibuffer
   "M-x" 'helm-select-action
   "C-r" 'evil-paste-from-register)
  (:states '(normal)
   :prefix global-leader
   "SPC" 'helm-M-x
   "ff" 'helm-find-files
   "fr" 'helm-recentf
   "bb" 'helm-buffers-list
   "ha" 'helm-apropos)

  ;; Org-mode specific
  (:states '(normal visual)
   :keymaps '(org-mode-map)
   :prefix major-mode-leader
   "/" 'helm-org-in-buffer-headings))

(use-package helm-descbinds
  :ensure t
  :defer t
  :commands helm-descbinds)

(use-package helm-files
  :ensure nil
  :defer t
  :general
  (:keymaps '(helm-find-files-map helm-read-file-map)
   "TAB" 'helm-execute-persistent-action
   "C-h" 'helm-find-files-up-one-level
   "C-l" 'helm-execute-persistent-action
   "C-r" 'evil-paste-from-register))

(use-package helm-projectile
  :ensure t
  :defer t
  :general
  (:states  '(normal)
   :prefix global-leader
   "pp" 'helm-projectile-switch-project
   "pf" 'helm-projectile-find-file))

(use-package helm-swoop
  :ensure t
  :after helm
  :general
  (:states '(normal)
   :prefix global-leader
   "/" 'helm-swoop))

(use-package helm-org-rifle
  :ensure t
  :after '(helm org)
  :general
  (:states '(normal)
   :prefix global-leader
   "or" 'helm-org-rifle))

(use-package htmlfontify
  :ensure t
  :defer t
  :config
  ;; Fix IE not recognising correct encoding
  (setq hfy-meta-tags
        (format "<meta name=\"generator\" content=\"emacs %s; htmlfontify %0.2f\" charset=\"utf-8\" />"
                emacs-version htmlfontify-version)))

(use-package htmlize
  :ensure t
  :defer t)

(use-package ispell
  :ensure t
  :defer t)

;; (use-package leuven-theme
;;   :ensure t
;;   :config
;;   (load-theme 'leuven t))

;; (use-package doom-themes
;;   :ensure t
;;   :init
;;   (load-theme 'doom-one t))


(use-package lispy
  :ensure t
  :delight lispy-mode
  :hook (emacs-lisp-mode . lispy-mode)
  :config
  (lispy-set-key-theme '(paredit c-digits)))

(use-package lispyville
  :ensure t
  :delight lispyville-mode
  :hook (lispy-mode . lispyville-mode))

(use-package magit
  :ensure t
  :general
  (:states '(normal)
   :prefix global-leader
   "gs" 'magit-status))

(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package navi-mode
  :ensure t
  :defer t)

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'ascii)
  :general
  (:states '(normal)
   :prefix global-leader
   "tt" 'neotree-toggle))

(use-package nlinum-relative
  :ensure t
  :when (version< emacs-version "26.0")
  :hook ((prog-mode . nlinum-relative-mode)
         (nlinum-relative-mode . nlinum-relative-setup-evil))
  :config
  (setq nlinum-relative-redisplay-delay 0.0))

(use-package nlinum-hl
  :ensure t
  :when (version< emacs-version "26.0")
  :after nlinum)

(use-package ob-sql-mode
  :ensure t
  :defer t
  :after org)

(use-package openwith
  :ensure t
  :defer t
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("doc" "docx"))
               "word"
               '(file))
         (list (openwith-make-extension-regexp
                '("ppt" "pptx"))
               "powerpoint"
               '(file))
         (list (openwith-make-extension-regexp
                '("xls" "xlsx"))
               "excel"
               '(file))
         ))
  (openwith-mode 1))

(use-package org
  :ensure org-plus-contrib
  :defer t
  :preface
  ;; (unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))
  ;;   (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))
  :config
  (setq org-src-preserve-indentation nil
        org-export-with-sub-superscripts nil
        org-edit-src-content-indentation 0
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-hide-leading-stars t)
  (add-to-list 'org-file-apps '("\\.xls\\'" . default))
  (add-hook 'org-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
  (setq org-capture-templates
        '(("n" "Notes" entry
           (file "~/org/notes.org") "* %?\n")
          ("t" "Todo" entry
           (file "~/org/tasks.org") "* TODO %?\n SCHEDULED: %^t\n")))
  (when (member (system-name) work-computer)
    (add-to-list 'org-capture-templates
                 '("i" "Inquiry" entry
                   (file "~/org/inquiry.org") "* %?\n")))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)")))
  :general
  (:states '(normal)
   :prefix global-leader
   "ac" 'org-capture)
  (:states '(normal)
   :keymaps '(org-mode-map)
   "-" 'org-cycle-list-bullet
   "<" 'org-metaleft
   ">" 'org-metaright)
  (:states '(normal visual)
   :keymaps '(org-mode-map)
   :prefix major-mode-leader
   "." 'org-time-stamp
   "I" 'org-clock-in
   "l" 'org-open-at-point
   "O" 'org-clock-out
   "vo" 'org-overview
   "vc" 'org-content
   "va" 'outline-show-all))

(use-package origami
  :ensure t
  :commands origami-mode
  :hook (prog-mode . origami-mode))

(use-package outorg
  :ensure t
  :defer t
  :after org
  :init
  (defvar outline-minor-mode-prefix "\M-#"))

(use-package outshine
  :ensure t
  :hook (outline-minor-mode . outshine-hook-function))

(use-package ox-md
  :ensure nil
  :defer t
  :after org)

(use-package paren
  :ensure nil
  :hook (prog-mode . show-paren-mode))

(use-package projectile
  :ensure t
  :defer t
  :delight projectile-mode
  :config
  (when (eq system-type 'windows-nt)
    (defun windows-command-coding (orig-fun &rest args)
      "Fix the handling of the coding for external commands"
      (let ((coding-system-for-read 'utf-8)
            (coding-system-for-write 'utf-8))
        (apply orig-fun args)))
    (advice-add 'projectile-files-via-ext-command :around #'windows-command-coding))
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-generic-command "fd . -0")
  (setq projectile-svn-command 'projectile-generic-command))

(use-package rainbow-mode
  :ensure t
  :delight rainbow-mode
  :defer t
  :config
  (rainbow-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config)

(use-package ranger
  :ensure t
  :disabled
  :defer t
  :config
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-eagerly t)
  :general
  ;; (:keymaps '(normal)
  ;;  "-" 'deer)
  (:states '(normal)
   :prefix global-leader
   "ar" 'ranger
   "ad" 'deer))

(use-package recentf
  :ensure nil
  :after no-littering
  :config
  (setq recentf-max-menu-items 100)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package rg
  :ensure t
  :commands (rg))

(use-package shackle
  :ensure t
  :defer t
  :config
  (setq shackle-default-alignment 'below
        shackle-default-size 8
        shackle-rules '(("*compilation*" :size 0.25 :noselect t)
                        ("*info*" :size 0.5 :select t)
                        ("*Backtrace*" :size 20 :noselect t)
                        ("*Warnings*"  :size 12 :noselect t :autofit t)
                        ("*Messages*"  :size 12 :noselect t)
                        ("*Help*" :size 0.4)
                        (apropos-mode :size 0.3)))
  (shackle-mode 1))

(use-package simple
  :ensure nil
  :defer t
  :config
  (column-number-mode 1))

(use-package sql-indent
  :ensure t
  :defer t)

(use-package terminal-here
  :ensure t
  :config
  (defun terminal-here-default-terminal-command (_dir)
    "Pick a good default command to use for DIR."
    (cond
     ((eq system-type 'darwin)
      (list "open" "-a" "iTerm.app" "."))

     ;; From http://stackoverflow.com/a/13509208/874671
     ((memq system-type '(windows-nt ms-dos cygwin))
      ;; (list "cmd.exe" "/C" "start" "nyagos.exe")
      (list "cmd.exe" "/C" "start" "nyagos.exe"))

     ;; Probably X11!
     (t '("x-terminal-emulator"))))
  (setq terminal-here-terminal-command 'terminal-here-default-terminal-command)
  :general
  (:states '(normal)
   :prefix global-leader
   "at" 'terminal-here-launch))

(use-package text-mode
  :ensure nil
  :init
  (setq-default major-mode 'text-mode))

(use-package tramp
  :ensure t
  :defer t)

(use-package treemacs
  :ensure t
  :disabled
  :config
  (setq treemacs-no-png-images t)
  :general
  (:states '(normal)
   :prefix global-leader
   "tn" 'treemacs-toggle))

(use-package undo-tree
  :ensure t
  :delight undo-tree-mode
  :defer t
  :config
  (setq undo-tree-auto-save-history t)
  :general
  (:states '(normal)
   :prefix global-leader
   "tu" 'undo-tree-visualize))

(use-package vdiff
  :ensure t
  :defer t
  :config
  (setq vdiff-auto-refine t))

(use-package vlf
  :ensure t
  :demand
  :config
  (require 'vlf-setup))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :config
  (setq-default web-mode-markup-indent-offset tab-width
                web-mode-css-indent-offset tab-width
                web-mode-code-indent-offset tab-width))

;;; init.el ends here
