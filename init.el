;; init.el --- Emacs configuration file -*- lexical-binding: t -*-

(setq package--init-file-ensured t) ;; Avoid unwanted package-initialize

;;; Startup optimization

;; Remember the original value file name handler
(defvar prev-file-name-handler-alist file-name-handler-alist)

;; Disable file-name-handler and increase gc-cons-threshold
(setq file-name-handler-alist nil
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Change back the file name handler and garbage collection to a sensible value
(add-hook 'after-init-hook
          (lambda () (setq file-name-handler-alist prev-file-name-handler-alist
                           ;; gc-cons-threshold 800000
                           gc-cons-threshold 16777216
                           gc-cons-percentage 0.1)))

;;; Core configuration

;;;; Constants

(defconst *is-win* (memq system-type '(windows-nt ms-dos)))
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

;;;; Variables

(defvar work-computer-list '("pn1v94")
  "List of work computers")

;;;; Package management

;;;;; Package.el

;; Set package-enable-at-startup nil to avoid loading packages twice.
(setq package-enable-at-startup nil)

;; Set up the archive URL according to the availability of SSL.
(let* ((no-ssl (and *is-win*
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (setq package-archives `(("org" . ,(concat proto "://orgmode.org/elpa/"))
                           ("melpa" . ,(concat proto "://melpa.org/packages/"))
                           ("gnu" . ,(concat proto "://elpa.gnu.org/packages/")))))

(require 'package)
(package-initialize)

;;;;; Use-package

;; Install use-package if it doesn't exist
;; Following code is for installing use-package via package.el.
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

;;;; Suppress messages

;; (defun supress-error (data context signal)
;;   "Suppress some of the unnecessary error messages"
;;   (when (not (memq (car data) '(beginning-of-line
;;                                 beginning-of-buffer
;;                                 end-of-line
;;                                 end-of-buffer
;;                                 text-read-only)))
;;     (command-error-default-function data context signal)))
;; (setq command-error-function 'supress-error)

(defun supress-message (orig-fun &rest args)
  "Fix the handling of the coding for external commands"
  (let ((inhibit-message t))
    (apply orig-fun args)))
(advice-add 'push-mark :around #'supress-message)

;;;; Dealing with very large files

(defun find-file-large-file-hook ()
  "If a file is over a given size, make the file open in
Fundamental-mode, and disable the undo"
  (when (> (buffer-size) (* 1024 1024))
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-fle-hook 'find-file-large-file-hook)

;;;; Utility macro

(defmacro let-coding-for-rw (fun coding)
  "Utility macro to create functions to deal with coding system problems.

The macro defines function to be used with advice-add which
let binds read-coding and write-coding variables according to CODING.

It will then evaluate advice-add around FUN with the defined function."
  (let* ((read-coding (car coding))
         (write-coding (cdr coding))
         (read-name (symbol-name read-coding))
         (write-name (symbol-name write-coding))
         (suffix "\\(mac\\|dos\\|unix\\)")
         (bind-name
          (if (eql 1 (length (delete-dups
                              '(read-name write-name))))
              read-name
            (concat read-name "-"
                    (if (string-match-p suffix read-name)
                        (replace-regexp-in-string
                         (replace-regexp-in-string suffix "" read-name)
                         "" write-name)
                      write-name))))
         (fun-name (intern (format "let-%s-for-rw" bind-name)))
         (doc (format "Advice function to let bind `coding-system-for-read' and `coding-system-for-write'
to %s and %s respectively for ORIG-FUN."
                      read-name write-name)))
    `(progn (unless (fboundp ',fun-name)
              (defun ,fun-name (orig-fun &rest args)
                ,doc
                (let ((coding-system-for-read ',read-coding)
                      (coding-system-for-write ',write-coding))
                  (apply orig-fun args))))
            (advice-add ,fun :around #',fun-name))))

;; (let ((suffix "\\(mac\\|dos\\|unix\\)")
;;       (read-name "utf-8-dos")
;;       (write-name "utf-8"))
;;   (if (eql 1 (length (delete-dups
;;                       (list read-name write-name))))
;;       read-name
;;     (concat read-name "-"
;;             (if (string-match-p suffix read-name)
;;                 (replace-regexp-in-string
;;                  (replace-regexp-in-string suffix "" read-name)
;;                  "" write-name)
;;               write-name))))

;; (let ((suffix "\\(mac\\|dos\\|unix\\)")
;;       (read-name "utf-8-dos")
;;       (write-name "utf-8"))
;;   (if (string-match-p suffix write-name)
;;       "matched!"
;;     "no match."))

;; (mapcar (lambda (name) (funcall #'replace-regexp-in-string
;;                                 "-\\(mac\\|dos\\|unix\\)" "" name)) '("utf-8-dos" "utf-8-unix"))
;; '("utf-8" (symbol-name 'utf-8-unix))
;; (delq nil (delete-dups (list "foo" "bar" nil "moo" "bar" "moo" nil "affe")))
;; (length (delete-dups '("foo" "foo")))

;;; Libraries

;;;; Benchmarking

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package esup :ensure t)

;;;; Delight

(use-package delight :ensure t)

;;;; General

(use-package general
  :ensure t
  :config
  (setq global-leader "SPC"
        major-mode-leader ","))

;;;; Which-Key

(use-package which-key
  :ensure t
  :defer .3
  :delight which-key-mode
  :commands which-key-mode
  :config
  (which-key-mode 1)
  :general
  (:states '(normal visual)
   :keymaps '(global-map)
   :prefix global-leader
   ;; "" '(:ignore t :which-key "global prefix")
   "f" '(:ignore t :which-key "Files")
   "g" '(:ignore t :which-key "Git")
   "o" '(:ignore t :which-key "Org")
   "h" '(:ignore t :which-key "Help")
   "b" '(:ignore t :which-key "Buffer")
   "n" '(:ignore t :which-key "Narrow")
   "w" '(:ignore t :which-key "Windows")
   "t" '(:ignore t :which-key "Toggle")
   "p" '(:ignore t :which-key "Project")))

;;;; Hydra

(use-package hydra
  :ensure t
  :defer .3
  :config
  ;; (defhydra hydra-zoom (global-map "<f2>")
  ;;   "zoom"
  ;;   ("g" text-scale-increase "in")
  ;;   ("l" text-scale-decrease "out"))

  (defhydra hydra-zoom (:color pink
                        :hint nil)
    "
_+_: in
_-_: out
"
    ("+" text-scale-increase)
    ("-" text-scale-decrease)
    ("q" nil "quit hydra"))

  (defhydra hydra-windows (:color pink
                           :hint nil)
    "
_h_: Move left   _H_: Move window left  _<_: Decrease width   _c_: Close window
_j_: Move down   _J_: Move window down  _>_: Increase width
_k_: Move up     _K_: Move window up    _-_: Decrease height
_l_: Move right  _L_: Move window right _+_: Increase height
"
    ("h" evil-window-left)
    ("j" evil-window-down)
    ("k" evil-window-up)
    ("l" evil-window-right)
    ("H" evil-window-move-far-left)
    ("J" evil-window-move-very-bottom)
    ("K" evil-window-move-very-top)
    ("L" evil-window-move-far-right)
    ("<" evil-window-decrease-width)
    (">" evil-window-increase-width)
    ("-" evil-window-decrease-height)
    ("+" evil-window-increase-height)
    ("s" evil-window-split)
    ("v" evil-window-vsplit)
    ("c" evil-window-delete)
    ("q" nil "quit hydra"))
  :general
  (:states '(normal visual)
   :keymaps '(override)
   :prefix global-leader
   "z" 'hydra-zoom/body
   "w." 'hydra-windows/body))

;;;; Cp5022x

;; Uncomment following to find what program is being run by process-file
;; (defvar global-program-process nil
;;   "Test var")
;; (defun debug-program-process (orig-fun &rest args)
;;   "Test"
;;   (setq global-program-process args)
;;   (apply orig-fun args))
;; (advice-add 'process-file :around #'debug-program-process)

(use-package cp5022x
  :ensure t
  :demand
  :config
  (set-language-environment "Japanese")
  (setq locale-coding-system 'utf-8-unix)
  ;; Encoding settings
  (prefer-coding-system 'utf-8)
  ;; Windows specific encoding settings
  (when *is-win*
    (set-file-name-coding-system 'cp932)
    (set-keyboard-coding-system 'cp932)
    (set-terminal-coding-system 'cp932)
    ;; (setq default-process-coding-system '(undecided-dos . utf-8-unix))
    (setq default-process-coding-system '(undecided-dos . cp932-unix))
    (add-to-list 'process-coding-system-alist '("rg" . (utf-8-dos . utf-8-unix)))
    (add-to-list 'process-coding-system-alist '("gtags" . (cp932-dos . cp932-unix))))
  ;; Encoding priority
  (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                        'katakana-jisx0201 'iso-8859-1 'unicode)
  (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932))

;;;; Paradox

(use-package paradox
  :ensure t
  :defer 1
  :config
  (paradox-enable))

;;;; No-Littering

(use-package no-littering
  :ensure t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;;;; Server

(use-package server
  :ensure nil
  :disabled
  :when *is-win*
  :delight server-mode
  :unless (or noninteractive)
  :hook (after-init . server-start))

;;;; Settings

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

;; Disable bidi-display
(setq-default bidi-display-reordering nil)

;; Respect fontset settings for symbols
(setq use-default-font-for-symbols nil)
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
      scroll-preserve-screen-position t
      hscroll-step 1
      hscroll-margin 5
      auto-hscroll-mode t)
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

;; (when (eq system-type 'darwin)
;;   (setq mac-option-modifier 'meta
;;         mac-command-modifier 'super))

;; Advice to show the last-command on the command line
;; Uncomment following lines to enable.
;; (defadvice call-interactively (after show-last-command activate)
;;   "Shows the interactive command that was just run in the message area."
;;   (unless (eq major-mode 'minibuffer-inactive-mode)
;;     (message "Ran %s" (ad-get-arg 0))))

;;; UI settings

;; Whether frames should be resized implicitly.
(setq-default frame-inhibit-implied-resize t)

;;;; Font settings

;; (defvar default-font-family nil
;;   "Font to be used for English characters")

;; (defvar ja-default-font-family nil
;;   "Font to be used for Japanese characters")

;; (defvar default-font-size nil
;;   "Default font size")

;; (defvar ja-default-font-rescale nil
;;   "Rescaling value for Japanese characters")

;; ;; TODO: Introduce a variable for font-family to be used
;; ;; for the display of special characters.
;; ;; Current settings works for mac, but the font I use for
;; ;; windows has the whitespace-mode's ?\u0183 as double width.

;; (cond (*is-win*
;;        (setq default-font-family "Myrica M")
;;        (setq default-font-size 10)
;;        (setq ja-default-font-family default-font-family)
;;        (setq ja-default-font-rescale 1.0))
;;       (*is-mac*
;;        (progn
;;          (setq default-font-family "Operator Mono SSm")
;;          (setq default-font-size 11)
;;          (setq ja-default-font-family "Ricty Discord")
;;          (setq ja-default-font-rescale 1.3))))

;; (defun set-font (&optional frame)
;;   (when frame
;;     (select-frame frame))
;;   (when (display-graphic-p)
;;     (let* ((font-family default-font-family)
;;            (font-size default-font-size)
;;            (font-height (* font-size 10))
;;            (ja-font-family ja-default-font-family))
;;       (set-face-attribute 'default nil :family font-family :height font-height)
;;       (let ((name (frame-parameter nil 'font))
;;             (font-spec (font-spec :family font-family))
;;             (characters '((?\u00A0 . ?\u00FF) ; Latin-1
;;                           (?\u0100 . ?\u017F) ; Latin Extended-A
;;                           (?\u0180 . ?\u024F) ; Latin Extended-B
;;                           (?\u0250 . ?\u02AF) ; IPA Extensions
;;                           (?\u0370 . ?\u03FF))) ; Greek and Coptic
;;             (ja-font-spec (font-spec :family ja-font-family))
;;             (ja-characters '(katakana-jisx0201
;;                              cp932-2-byte
;;                              japanese-jisx0212
;;                              japanese-jisx0213-2
;;                              japanese-jisx0213.2004-1)))
;;         (dolist (ja-character ja-characters)
;;           (set-fontset-font name ja-character ja-font-spec))
;;         (dolist (character characters)
;;           (set-fontset-font name character font-spec))
;;         (add-to-list 'face-font-rescale-alist (cons ja-font-family ja-default-font-rescale))))))
(defvar default-font-family nil
  "Font to be used for English characters")

(defvar ja-default-font-family nil
  "Font to be used for Japanese characters")

(defvar default-font-size nil
  "Default font size")

(defvar ja-default-font-rescale nil
  "Rescaling value for Japanese characters")

;; TODO: Introduce a variable for font-family to be used
;; for the display of special characters.
;; Current settings works for mac, but the font I use for
;; windows has the whitespace-mode's ?\u00B7 as double width.

(cond (*is-win*
       (setq default-font-family "Myrica M")
       (setq default-font-size 10)
       (setq ja-default-font-family default-font-family)
       (setq ja-default-font-rescale 1.0)
       (setq symbol-default-font-family "Courier New")
       (setq symbol-default-font-rescale 0.9))
      (*is-mac*
       (setq default-font-family "Operator Mono SSm")
       (setq default-font-size 11)
       (setq ja-default-font-family "Ricty Discord")
       (setq ja-default-font-rescale 1.3)
       (setq symbol-default-font-family default-font-family)
       (setq symbol-default-font-rescale 1.0)))

(defun set-font (&optional frame)
  (when frame
    (select-frame frame))
  (when (display-graphic-p)
    (let* ((font-family default-font-family)
           (font-size default-font-size)
           (font-height (* font-size 10))
           (ja-font-family ja-default-font-family)
           (symbol-font-family symbol-default-font-family))
      (set-face-attribute 'default nil :family font-family :height font-height)
      (let ((name (frame-parameter nil 'font))
            (font-spec (font-spec :family font-family))
            (characters '((?\u00A0 . ?\u00FF) ; Latin-1
                          (?\u0100 . ?\u017F) ; Latin Extended-A
                          (?\u0180 . ?\u024F) ; Latin Extended-B
                          (?\u0250 . ?\u02AF) ; IPA Extensions
                          (?\u0370 . ?\u03FF))) ; Greek and Coptic
            (ja-font-spec (font-spec :family ja-font-family))
            (ja-characters '(katakana-jisx0201
                             cp932-2-byte
                             japanese-jisx0212
                             japanese-jisx0213-2
                             japanese-jisx0213.2004-1))
            (symbol-font-spec (font-spec :family symbol-font-family))
            (symbol-characters '(?\u00B7)))
        (dolist (ja-character ja-characters)
          (set-fontset-font name ja-character ja-font-spec))
        (dolist (character characters)
          (set-fontset-font name character font-spec))
        (dolist (symbol-character symbol-characters)
          (set-fontset-font name symbol-character symbol-font-spec))
        (add-to-list 'face-font-rescale-alist
                     (cons ja-font-family ja-default-font-rescale))
        (add-to-list 'face-font-rescale-alist
                     (cons symbol-font-family symbol-default-font-rescale))
        ))))
(add-hook 'after-init-hook #'set-font)
(add-hook 'after-make-frame-functions #'set-font)

;;;; Disable unnecessary GUI elements

(menu-bar-mode -1)
(when (boundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;;; Remember window size

(defun save-framegeometry ()
  "Gets the current frame's geometry and saves to ~/.emacs.d/framegeometry."
  (let ((framegeometry-left (frame-parameter (selected-frame) 'left))
        (framegeometry-top (frame-parameter (selected-frame) 'top))
        (framegeometry-width (frame-parameter (selected-frame) 'width))
        (framegeometry-height (frame-parameter (selected-frame) 'height))
        (framegeometry-file (expand-file-name "~/.emacs.d/framegeometry")))

    (when (not (number-or-marker-p framegeometry-left))
      (setq framegeometry-left 0))
    (when (not (number-or-marker-p framegeometry-top))
      (setq framegeometry-top 0))
    (when (not (number-or-marker-p framegeometry-width))
      (setq framegeometry-width 0))
    (when (not (number-or-marker-p framegeometry-height))
      (setq framegeometry-height 0))

    (with-temp-buffer
      (insert
       ";;; This is the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       "      '(\n"
       (format "        (top . %d)\n" (max framegeometry-top 0))
       (format "        (left . %d)\n" (max framegeometry-left 0))
       (format "        (width . %d)\n" (max framegeometry-width 0))
       (format "        (height . %d)))\n" (max framegeometry-height 0)))
      (when (file-writable-p framegeometry-file)
        (write-file framegeometry-file)))))

(defun load-framegeometry ()
  "Loads ~/.emacs.d/framegeometry which should load the previous frame's geometry."
  (let ((framegeometry-file (expand-file-name "~/.emacs.d/framegeometry")))
    (when (file-readable-p framegeometry-file)
      (load-file framegeometry-file))))

;; Special work to do ONLY when there is a window system being used
(if window-system
    (progn
      (add-hook 'after-init-hook 'load-framegeometry)
      (add-hook 'kill-emacs-hook 'save-framegeometry)))

;;;; Themes

(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one-light t))

;;;; Indent Guide

(use-package highlight-indent-guides
  :ensure t
  :defer .3
  :config
  (setq highlight-indent-guides-method 'character))

;;; Packages

;;;; Evil

(use-package evil
  :ensure t
  :defer .3
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-auto-balance-windows nil)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-search-vim-style-regexp t)
  ;; (setq evil-cross-lines t)

  :config
  (when evil-want-C-u-scroll
    (define-key universal-argument-map (kbd "C-u") nil))

  (evil-mode 1)
  (modify-syntax-entry ?_ "w")
  ;; (add-hook 'c-mode-common-hook (lambda () (modify-syntax-entry ?_ "w")))

  (defcustom cursor-move-hook nil
    "A hook that runs when the cursor moves."
    :type 'hook)

  (defvar pre-command-point nil
    "Holds the cursor position before a command is run.")

  ;; Following code replicates vim-slash's behavior
  ;; Excluding it's star-search behavior.
  ;; TODO: Implement vim-slash's star-search behavior.
  (defun record-pre-command-point ()
    "Records current point of cursor for later use"
    (setq pre-command-point (point)))
  (add-hook 'pre-command-hook #'record-pre-command-point)

  (defun run-cursor-move-hook ()
    "Run cursor-move-hook"
    (when (not (eq pre-command-point (point)))
      (run-hooks 'cursor-move-hook)))
  (add-hook 'post-command-hook #'run-cursor-move-hook)

  (defun evil-ex-nohighlight-cursor-move ()
    "Disable a function to disable evil-ex-search hightlight if it is active"
    (let ((target-commands '(exit-minibuffer
                             evil-ex-search-forward
                             evil-ex-search-backward
                             evil-ex-search-word-forward
                             evil-ex-search-word-backward
                             evil-ex-search-unbounded-word-forward
                             evil-ex-search-unbounded-word-backward
                             evil-ex-search-next
                             evil-ex-search-previous)))
      (when (and (evil-ex-hl-active-p 'evil-ex-search)
                 (not (memq this-command target-commands)))
        (evil-ex-nohighlight))))
  (add-hook 'cursor-move-hook #'evil-ex-nohighlight-cursor-move)

  (evil-define-text-object evil-entire-entire-buffer (count &optional beg end type)
    "Select entire buffer"
    (evil-range (point-min) (point-max)))

  ;; Temporary fix for screen blinking while inserting text with ime for macOS
  (when *is-mac*
    (defun enable-redisplay-dont-pause ()
      "Set the value of redisplay-dont-pause to t."
      (setq redisplay-dont-pause t))
    (defun disable-redisplay-dont-pause ()
      "Set the value of redisplay-dont-pause to nil."
      (setq redisplay-dont-pause nil))
    (add-hook 'evil-insert-state-entry-hook
              #'disable-redisplay-dont-pause)
    (add-hook 'evil-insert-state-exit-hook
              #'enable-redisplay-dont-pause)
    (add-hook 'minibuffer-setup-hook
              #'disable-redisplay-dont-pause)
    (add-hook 'minibuffer-exit-hook
              #'enable-redisplay-dont-pause))

  (evil-define-command evil-ex-vresize (arg)
    "The ex :vresize command.

If ARG is a signed positive integer, increase the current window
width by ARG.

If ARG is a signed negative integer, decrease the current window
width by ARG.

If ARG is a positive integer without explicit sign, set the current
window width to ARG.

If ARG is empty, maximize the current window width."
    (interactive "<a>")
    (if (or (not arg) (= 0 (length arg)))
        (evil-window-set-width nil)
      (let ((n (string-to-number arg)))
        (if (> n 0)
            (if (= ?+ (aref arg 0))
                (evil-window-increase-width n)
              (evil-window-set-width n))
          (evil-window-decrease-width (- n))))))
  (evil-ex-define-cmd "vres[ize]" 'evil-ex-vresize)

  (evil-ex-define-cmd "bd[elete]" 'kill-current-buffer)

  (defmacro define-and-bind-quoted-text-object (name key start-regex end-regex)
    (let ((inner-name (make-symbol (concat "evil-inner-" name)))
          (outer-name (make-symbol (concat "evil-a-" name))))
      `(progn
         (evil-define-text-object ,inner-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex beg end type count nil))
         (evil-define-text-object ,outer-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex beg end type count t))
         (define-key evil-inner-text-objects-map ,key #',inner-name)
         (define-key evil-outer-text-objects-map ,key #',outer-name))))

  (define-and-bind-quoted-text-object "plus" "+" "+" "+")
  (define-and-bind-quoted-text-object "pipe" "|" "|" "|")
  (define-and-bind-quoted-text-object "slash" "/" "/" "/")
  (define-and-bind-quoted-text-object "asterisk" "*" "*" "*")
  (define-and-bind-quoted-text-object "dollar" "$" "\\$" "\\$") ;; sometimes your have to escape the regex

  ;; esc quits
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

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

  ;; Disable selection marking with "C-SPC"
  ;; (:keymaps '(global)
  ;;  "C-SPC" nil)

  (:keymaps '(evil-ex-completion-map)
   "C-b" 'backward-char
   "C-a" 'beginning-of-line
   "C-e" 'end-of-line
   "C-k" 'kill-line)

  (:keymaps '(evil-outer-text-objects-map)
   "e" 'evil-entire-entire-buffer)

  (:keymaps '(evil-inner-text-objects-map)
   "e" 'evil-entire-entire-buffer)

  ;; Exit out of insert state with "jk"
  (:states '(insert)
   "j" (general-key-dispatch 'self-insert-command
         :timeout 0.25
         "k" 'evil-normal-state))

  (:states '(normal visual)
   :prefix global-leader
   "tn" 'evil-ex-nohighlight))

(use-package evil-matchit
  :ensure t
  :defer .3
  :config
  (global-evil-matchit-mode 1))

;; This package needs evil-want-keybinding set to nil on evil init
(use-package evil-collection
  :after evil
  :ensure t
  :defer .3
  :config
  (evil-collection-init '(simple calendar comint custom ediff occur xref simple term)))

(use-package evil-goggles
  :ensure t
  :delight evil-goggles-mode
  :defer .3
  :config
  (add-to-list 'evil-goggles--commands '(lispyville-yank :face evil-goggles-yank-face :switch evil-goggles-enable-yank :advice evil-goggles--generic-async-advice))
  (add-to-list 'evil-goggles--commands '(lispyville-change :face evil-goggles-change-face :switch evil-goggles-enable-change :advice evil-goggles--generic-blocking-advice))
  (add-to-list 'evil-goggles--commands '(lispyville-delete :face evil-goggles-delete-face :switch evil-goggles-enable-delete :advice evil-goggles--generic-blocking-advice))
  (evil-goggles-mode))

(use-package evil-commentary
  :after evil
  :ensure t
  :delight evil-commentary-mode
  :hook (prog-mode . evil-commentary-mode))

(use-package evil-indent-plus
  :after evil
  :ensure t
  :defer .3
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-surround
  :after evil
  :ensure t
  :defer .3
  :config
  (add-to-list 'evil-surround-operator-alist
               '(lispyville-change . change))
  (add-to-list 'evil-surround-operator-alist
               '(lispyville-delete . delete))
  (global-evil-surround-mode 1)
  :general
  (:states '(visual)
   "s" 'evil-surround-region))

(use-package evil-textobj-entire
  :disabled
  :ensure t)

(use-package evil-textobj-line
  :ensure t
  :defer .3)

(use-package evil-visualstar
  :ensure t
  :defer t
  :config
  (setq evil-visualstar/persistent t)
  (global-evil-visualstar-mode))

;;;; Indentation

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
          evil-visual-paste))
  (add-to-list 'aggressive-indent-excluded-modes 'ediff-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'vdiff-mode))

;;;; Searching

(use-package anzu
  :ensure t
  :defer t)

(use-package evil-anzu
  :after evil
  :ensure t
  :defer t)

(use-package rg
  :disabled
  :ensure t
  :commands (rg)
  :config
  (when *is-win*
    (setq rg-command-line-flags '("--encoding ms932"))))

(use-package deadgrep
  :ensure t
  :commands (deadgrep)
  ;; :config
  ;; (when *is-win*
  ;;   (let-coding-for-rw 'deadgrep--start (utf-8-dos . utf-8-unix)))
  :general
  (:keymaps '(deadgrep-mode-map)
   :states '(normal visual)
   [return] 'deadgrep-visit-result
   "gr" 'deadgrep-restart
   "C-j" 'deadgrep-forward
   "C-k" 'deadgrep-backward
   [tab] 'deadgrep-toggle-file-results
   "q" 'quit-window
   "ZZ" 'quit-window
   "ZQ" 'evil-quit))

;;;; Scrolling

(use-package smooth-scrolling
  :disabled
  :ensure t
  :commands (smooth-scrolling-mode))

;;;; Window management

(use-package shackle
  :ensure t
  :defer .3
  :config
  (setq shackle-default-alignment 'below
        shackle-default-size 0.33
        shackle-rules '(("*compilation*" :size 0.25 :select nil)
                        ("*info*" :size 0.5 :select t)
                        ("*Backtrace*" :size 33 :select nil)
                        ("*Warnings*"  :size 33 :select nil :autofit t)
                        ("*Messages*"  :size 33 :select nil)
                        ;; ("*Help*" :same t :inhibit-window-quit t :size 20 :select t)
                        ("*Help*" :popup t :size 0.33 :align 'below)
                        ))
  (shackle-mode 1))

;;;; Diff

(use-package diff
  :ensure nil
  :defer t)

(use-package ediff
  :ensure nil
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package vdiff
  :ensure t
  :defer t
  :config
  (setq vdiff-auto-refine t))

;;;; File Explorer

(use-package dired
  :ensure nil
  :defer t
  :commands dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (defun dired-buffer-file-parent ()
    "Opens dired in the parent directory of the current buffer."
    (interactive)
    (dired default-directory))

  (setq dired-use-ls-dired nil)

  :general
  (:states '(normal visual)
   "-" 'dired-buffer-file-parent)

  (:keymaps '(dired-mode-map)
   :states '(normal visual)
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
   "M-C-?" 'dired-unmark-all-files
   "M-C-d" 'dired-tree-down
   "M-C-u" 'dired-tree-up
   "M-C-n" 'dired-next-subdir
   "M-C-p" 'dired-prev-subdir
   ;; move to marked files
   "M-{" 'dired-prev-marked-file
   "M-}" 'dired-next-marked-file
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
   "* <delete>" 'dired-unmark-backward
   "* C-n" 'dired-next-marked-file
   "* C-p" 'dired-prev-marked-file
   "*t" 'dired-toggle-marks
   ;; Lower keys for commands not operating on all the marked files
   "a" 'dired-find-alternate-file
   "d" 'dired-flag-file-deletion
   "gf" 'dired-find-file
   "C-m" 'dired-find-file
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
   ;; "<return>" 'dired-find-file
   "S-<return>" 'dired-find-file-other-window
   "M-<return>" 'dired-display-file
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
   ;; "^" 'dired-up-directory
   " " 'dired-next-line
   [?\S-\ ] 'dired-previous-line
   [remap next-line] 'dired-next-line
   [remap previous-line] 'dired-previous-line
   ;; hiding
   "g$" 'dired-hide-subdir ;; FIXME: This can probably live on a better binding.
   "M-$" 'dired-hide-all
   "(" 'dired-hide-details-mode
   ;; isearch
   "M-s a C-s"   'dired-do-isearch
   "M-s a M-C-s" 'dired-do-isearch-regexp
   "M-s f C-s"   'dired-isearch-filenames
   "M-s f M-C-s" 'dired-isearch-filenames-regexp
   ;; misc
   [remap read-only-mode] 'dired-toggle-read-only
   ;; `toggle-read-only' is an obsolete alias for `read-only-mode'
   [remap toggle-read-only] 'dired-toggle-read-only
   "g?" 'dired-summary
   "<delete>" 'dired-unmark-backward
   [remap undo] 'dired-undo
   [remap advertised-undo] 'dired-undo
   ;; thumbnail manipulation (image-dired)
   "C-t d" 'image-dired-display-thumbs
   "C-t t" 'image-dired-tag-files
   "C-t r" 'image-dired-delete-tag
   "C-t j" 'image-dired-jump-thumbnail-buffer
   "C-t i" 'image-dired-dired-display-image
   "C-t x" 'image-dired-dired-display-external
   "C-t a" 'image-dired-display-thumbs-append
   "C-t ." 'image-dired-display-thumb
   "C-t c" 'image-dired-dired-comment-files
   "C-t f" 'image-dired-mark-tagged-files
   "C-t C-t" 'image-dired-dired-toggle-marked-thumbs
   "C-t e" 'image-dired-dired-edit-comment-and-tags
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
   :states '(normal visual)
   [return] 'dired-single-buffer
   [mouse-1] 'dired-single-buffer-mouse
   ;; "^" 'dired-single-up-directory
   "-" 'dired-single-up-directory))

(use-package dired-x
  :ensure nil
  :defer t
  :after dired)

(use-package ranger
  :ensure t
  :disabled
  :defer t
  :config
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-eagerly nil)
  :general
  ;; (:keymaps '(normal)
  ;;  "-" 'deer)
  (:states '(normal visual)
   :prefix global-leader
   "ar" 'ranger
   "ad" 'deer))

(use-package treemacs
  :ensure t
  :defer t
  :disabled
  :config
  (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
        treemacs-deferred-git-apply-delay   0.5
        treemacs-display-in-side-window     t
        treemacs-file-event-delay           5000
        treemacs-file-follow-delay          0.2
        treemacs-follow-after-init          t
        treemacs-follow-recenter-distance   0.1
        treemacs-git-command-pipe           ""
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-indentation                2
        treemacs-indentation-string         " "
        treemacs-is-never-other-window      nil
        treemacs-max-git-entries            5000
        treemacs-no-png-images              t
        treemacs-no-delete-other-windows    t
        treemacs-project-follow-cleanup     nil
        treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-cursor                nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           nil
        treemacs-silent-refresh             nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-space-between-root-nodes   t
        treemacs-tag-follow-cleanup         t
        treemacs-tag-follow-delay           1.5
        treemacs-width                      35)
  :general
  (:states '(normal visual)
   :prefix global-leader
   "tn" 'treemacs))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t
  :disabled)

(use-package neotree
  :disabled
  :ensure t
  :config
  (setq neo-theme 'ascii)
  :general
  (:states '(normal visual)
   :prefix global-leader
   "tt" 'neotree-toggle))

;;;; Line Numbers

;; There is a major bug with display-line-numbers and cjk letters
;; Enabling display-line-numbers-mode will cause horizontal scrolling with
;; cjk letters to spaz out on hscroll-step 1.
(use-package display-line-numbers
  :ensure nil
  :unless (version< emacs-version "26.0")
  :hook ((prog-mode . display-line-numbers-mode)
         (org-mode . (lambda () (setq display-line-numbers nil)))
         (markdown-mode . (lambda () (setq display-line-numbers 'visual))))
  :init
  (setq display-line-numbers-grow-only t
        display-line-numbers-type 'relative))

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

;;;; Folding

(use-package origami
  :ensure t
  :disabled
  :commands origami-mode
  :hook (prog-mode . origami-mode))

;;;; Completion

(use-package company
  :ensure t
  :delight company-mode
  :commands (company-mode company-indent-or-complete-common)
  :hook (prog-mode . company-mode)
  :config
  (setq company-ddabbrev-code-everywhere t)
  (setq company-dabbrev-code-modes t)
  (setq company-dabbrev-code-other-buffers 'all)
  (setq company-dabbrev-ignore-buffers "\\`\\'")
  (setq company-idle-delay 0.5)
  (add-to-list 'company-backends '(company-capf company-dabbrev))

  :general
  (:keymaps '(company-active-map)
   [tab] 'company-complete-common-or-cycle
   [backtab] (lambda () (interactive)(company-complete-common-or-cycle -1))
   "C-n" 'company-select-next
   "C-p" 'company-select-previous))

(use-package company-box
  :disabled
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package company-posframe
  :disabled
  :ensure t
  :config
  (company-posframe-mode 1))

;;;; Linting

(use-package flycheck
  :disabled
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

(use-package ispell
  :ensure t
  :defer t)

;;;; Helm

(use-package helm
  :ensure t
  :defer .3
  :delight helm-mode
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-display-header-line nil)
  (setq helm-split-window-inside-p t)
  (setq helm-ff-kill-or-find-buffer-fname-fn 'ignore)
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
  (defun minibuffer-setup-gc ()
    (setq gc-cons-threshold 402653184
          gc-cons-percentage 0.6))
  (defun minibuffer-exit-gc ()
    (setq gc-cons-threshold 800000
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
  (:states '(normal visual)
   :keymaps '(override)
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

(use-package helm-files
  :ensure nil
  :defer .3
  :general
  (:keymaps '(helm-find-files-map helm-read-file-map)
   "TAB" 'helm-execute-persistent-action
   "C-h" 'helm-find-files-up-one-level
   "C-l" 'helm-execute-persistent-action
   "C-r" 'evil-paste-from-register))

(use-package helm-ag
  :ensure t
  :defer .3
  :commands (helm-ag
             helm-ag-this-file
             helm-do-ag
             helm-do-ag-this-file
             helm-ag-project-root
             helm-do-ag-project-root
             helm-ag-buffers
             helm-do-ag-buffers
             helm-ag-pop-stack
             helm-ag-clear-stack)
  :config
  (setq helm-ag-base-command "rg --vimgrep --no-heading")
  ;; TODO: needs cleanup
  (when *is-win*
    (setq helm-ag-base-command "rg --vimgrep --no-heading --encoding ms932")))

(use-package helm-descbinds
  :ensure t
  :defer .3
  :commands helm-descbinds)

(use-package helm-swoop
  :ensure t
  :defer .3
  :config
  (setq helm-swoop-use-line-number-face t
        helm-swoop-speed-or-color t)
  :general
  (:states '(normal visual)
   :keymaps '(override)
   :prefix global-leader
   "/" 'helm-swoop-without-pre-input))

;;;; Navigation

(use-package avy
  :ensure t
  :defer t
  :commands avy-goto-char
  :general
  (:states '(normal visual)
   :prefix global-leader
   "ss" 'avy-goto-char))

(use-package helm-gtags
  :ensure t
  :defer .3
  :config
  (when *is-win*
    (defun helm-gtags--exec-global-command-fix (orig-fun &rest args)
      "Fix the handling of the coding for external commands"
      (let ((buffer-file-coding-system 'cp932-dos))
        (apply orig-fun args)))
    (advice-add 'helm-gtags--exec-global-command :around #'helm-gtags--exec-global-command-fix)

    ;; (defun helm-gtags--exec-global-command (type input &optional detail)
    ;;   (let ((args (helm-gtags--construct-command type input)))
    ;;     (helm-gtags--find-tag-directory)
    ;;     (helm-gtags--save-current-context)
    ;;     (with-current-buffer (helm-candidate-buffer 'global)
    ;;       (let ((default-directory (helm-gtags--base-directory))
    ;;             (input (car (last args))))
    ;;         (unless (zerop (apply #'process-file "global" nil '(t nil) nil args))
    ;;           (error (format "%s: not found" input)))
    ;;         ;; --path options does not support searching under GTAGSLIBPATH
    ;;         (when (eq type 'find-file)
    ;;           (helm-gtags--print-path-in-gtagslibpath args))
    ;;         (helm-gtags--remove-carrige-returns)
    ;;         (when detail
    ;;           (helm-gtags--show-detail))))))
    ))

(use-package ggtags
  :ensure t
  :defer .3
  :config
  (ggtags-mode 1))

;;;; Projects

(use-package projectile
  :ensure t
  :defer t
  :delight projectile-mode
  :config

  (when *is-win*
    (let-coding-for-rw 'projectile-files-via-ext-command (utf-8-dos . utf-8-unix))

    (defun win-to-unix-path (path)
      "Convert windows style path to unix style path"
      (subst-char-in-string ?\\ ?/ path))

    (defun win-to-unix-path-list (orig-fun &rest args)
      "Convert list of windows style path to list of unix style path"
      (mapcar 'win-to-unix-path
              (apply orig-fun args)))

    ;; Add depth property to make it outermost
    (advice-add 'projectile-files-via-ext-command :around #'win-to-unix-path-list))

  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-generic-command "fd . -0")
  (setq projectile-svn-command 'projectile-generic-command))

(use-package helm-projectile
  :ensure t
  :defer t
  :general
  (:states  '(normal visual)
   :prefix global-leader
   "pp" 'helm-projectile-switch-project
   "pf" 'helm-projectile-find-file))

;;;; Outline

(use-package outshine
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . outshine-mode)
  :commands outshine-mode)

;;;; Fontification

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

;;;; Version Control System

(use-package magit
  :ensure t
  :config
  (setq magit-refresh-status-buffer nil
        magit-commit-show-diff nil
        magit-revert-buffers 1)
  :general
  (:states '(normal visual)
   :prefix global-leader
   "gs" 'magit-status))

(use-package evil-magit
  :after magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :delight git-gutter-mode
  :defer 1
  :init
  (setq git-gutter:update-interval 1)
  :config
  (global-git-gutter-mode +1)
  :general
  (:states '(normal visual)
   "]c" 'git-gutter:next-hunk
   "[c" 'git-gutter:previous-hunk))

;;;; Debug

(use-package bug-hunter
  :ensure t
  :defer t
  :commands bug-hunter-init-file)

;;;; Miscelleneous

(use-package autorevert
  :ensure t
  :defer t
  :config
  (setq auto-revert-interval 0.5)
  (global-auto-revert-mode 1))

(use-package files
  :ensure nil
  :config
  ;; Disable auto insertion of final newline for work computers
  ;; Because for some unknown reason, people in Japan never likes to
  ;; end their files with newline...
  (when (member (system-name) work-computer-list)
    (setq require-final-newline nil)))

(use-package simple
  :ensure nil
  :defer t
  :config
  (column-number-mode 1)
  (setq save-interprogram-paste-before-kill t))

(use-package button
  :ensure nil
  :init
  ;; TODO: Following command doesn't work unfortunately
  (defun eab/push-button-on-file-same-window ()
    (interactive)
    (let ((cwc (current-window-configuration))
          (hb (current-buffer))
          (file? (button-get (button-at (point)) 'help-args)))
      (funcall
       `(lambda ()
          (defun eab/push-button-on-file-same-window-internal ()
            (if (> (length ',file?) 1)
                (let ((cb (current-buffer)))
                  (set-window-configuration ,cwc)
                  (switch-to-buffer cb)
                  (kill-buffer ,hb)))))))
    (call-interactively 'push-button)
    (run-with-timer 0.01 nil 'eab/push-button-on-file-same-window-internal))
  )

(use-package whitespace
  :ensure nil
  :defer .3
  :config
  ;; (when *is-win*
  ;;   ;; (set-face-attribute 'whitespace-space nil :family "ＭＳ ゴシック")
  ;;   (setf (alist-get 'space-mark whitespace-display-mappings
  ;;                    nil nil #'equal) '(?\  [?.] [?.])))
  )

(use-package calc
  :ensure nil
  ;; :defer t
  :commands (calc)
  :config
  (require 'calc-ext)

  :general
  (:states '(normal visual)
   "-" 'dired-buffer-file-parent)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   "0" 'calcDigit-start
   "1" 'calcDigit-start
   "2" 'calcDigit-start
   "3" 'calcDigit-start
   "4" 'calcDigit-start
   "5" 'calcDigit-start
   "6" 'calcDigit-start
   "7" 'calcDigit-start
   "8" 'calcDigit-start
   "9" 'calcDigit-start

   (kbd "<tab>") 'calc-roll-down
   (kbd "S-<return>") 'calc-over
   (kbd "<return>") 'calc-enter
   (kbd "SPC") 'calc-enter

   (kbd "C-x C-t") 'calc-transpose-lines
   (kbd "C-M-d") 'calc-pop-above
   (kbd "C-M-i") 'calc-roll-up
   (kbd "M-RET") 'calc-last-args
   (kbd "C-M-w") 'kill-ring-save
   (kbd "M-%") 'calc-percent
   (kbd "M-k") 'calc-copy-as-kill
   (kbd "M-w") 'calc-copy-region-as-kill
   (kbd "M-DEL") 'calc-pop-above
   (kbd "M-m t") 'calc-total-algebraic-mode
   (kbd "<delete>") 'calc-pop
   (kbd "<mouse-2>") 'calc-yank
   "x" 'calc-pop ; was "C-d".  TODO: Conflicts with calc-execute-extended-command.
   "d" 'calc-kill                       ; was "C-k"
   "u" 'calc-undo                       ; was "U"
   "X" 'calc-call-last-kbd-macro        ; "@" is already used.
   "pp" 'calc-yank                      ; was "C-y"
   "pP" 'calc-copy-to-buffer            ; was "y"

   (kbd "C-p") 'calc-precision          ; was "p"

   "?" 'calc-help
   ;; "h" 'calc-help-prefix ; TODO: Rebind?
   "i" 'calc-info

   "\"" 'calc-auto-algebraic-entry
   "$" 'calc-auto-algebraic-entry       ; TODO: No need for this one?
   "'" 'calc-algebraic-entry

   "!" 'calc-factorial
   "#" 'calcDigit-start
   "%" 'calc-mod
   "&" 'calc-inv
   "(" 'calc-begin-complex
   ")" 'calc-end-complex
   "*" 'calc-times
   "+" 'calc-plus
   "," 'calc-comma
   "-" 'calc-minus
   "." 'calcDigit-start
   "/" 'calc-divide
   ":" 'calc-fdiv
   ";" 'calc-semi          ; TODO: Shall we really override `evil-ex'?
   "<" 'calc-scroll-left
   "=" 'calc-evaluate
   ">" 'calc-scroll-right
   "@" 'calcDigit-start
   "A" 'calc-abs
   "B" 'calc-log
   "C" 'calc-cos
   ;; "D" 'calc-redo             ; TODO: What's the purpose of this?  Bind to C-r?
   "E" 'calc-exp
   "F" 'calc-floor
   "G" 'calc-argument
   "H" 'calc-hyperbolic
   "I" 'calc-inverse
   "J" 'calc-conj
   "K" 'calc-keep-args
   "L" 'calc-ln
   "M" 'calc-more-recursion-depth
   "N" 'calc-eval-num
   "O" 'calc-option
   "P" 'calc-pi
   "Q" 'calc-sqrt
   "R" 'calc-round
   "S" 'calc-sin
   "T" 'calc-tan
   "[" 'calc-begin-vector
   "]" 'calc-end-vector
   "\\" 'calc-idiv
   "^" 'calc-power
   "_" 'calcDigit-start
   "`" 'calc-edit
   "e" 'calcDigit-start
   "n" 'calc-change-sign
   "o" 'calc-realign
   "w" 'calc-why
   "x" 'calc-execute-extended-command ; TODO: Conflicts with calc-pop.
   "|" 'calc-concat
   "{" 'calc-scroll-down                ; TODO: Not necessary?
   "}" 'calc-scroll-up                  ; TODO: Not necessary?
   "~" 'calc-num-prefix

   ;; quit
   ;; "zz" 'calc-z-prefix-help
   ;; "ZQ" 'quit-window ; TODO: Rebind "Z"?
   ;; "ZZ" 'quit-window ; TODO: Rebind "Z"?
   "q" 'calc-quit)


  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "V"
   "#" 'calc-set-cardinality
   "&" 'calc-inv
   "(" 'calc-vector-parens
   ")" 'calc-matrix-brackets
   "+" 'calc-remove-duplicates
   "," 'calc-vector-commas
   "-" 'calc-set-difference
   "." 'calc-full-vectors
   "/" 'calc-break-vectors
   ":" 'calc-set-span
   "<" 'calc-matrix-left-justify
   "=" 'calc-matrix-center-justify
   ">" 'calc-matrix-right-justify
   "?" 'calc-v-prefix-help
   "A" 'calc-apply
   "C" 'calc-cross
   "D" 'calc-mdet
   "E" 'calc-set-enumerate
   "F" 'calc-set-floor
   "G" 'calc-grade
   "H" 'calc-histogram
   "I" 'calc-inner-product
   "J" 'calc-conj-transpose
   "K" 'calc-kron
   "L" 'calc-mlud
   "M" 'calc-map
   "N" 'calc-cnorm
   "O" 'calc-outer-product
   "R" 'calc-reduce
   "S" 'calc-sort
   "T" 'calc-mtrace
   "U" 'calc-accumulate
   "V" 'calc-set-union
   "X" 'calc-set-xor
   "[" 'calc-vector-brackets
   "]" 'calc-matrix-brackets
   "^" 'calc-set-intersect
   "a" 'calc-arrange-vector
   "b" 'calc-build-vector
   "c" 'calc-mcol
   "d" 'calc-diag
   "e" 'calc-expand-vector
   "f" 'calc-vector-find
   "h" 'calc-head
   "i" 'calc-ident
   "k" 'calc-cons
   "l" 'calc-vlength
   "m" 'calc-mask-vector
   "n" 'calc-rnorm
   "p" 'calc-pack
   "r" 'calc-mrow
   "s" 'calc-subvector
   "t" 'calc-transpose
   "u" 'calc-unpack
   "v" 'calc-reverse-vector
   "x" 'calc-index
   "{" 'calc-vector-braces
   "}" 'calc-matrix-brackets
   "~" 'calc-set-complement)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "V"
   "#" 'calc-set-cardinality
   "&" 'calc-inv
   "(" 'calc-vector-parens
   ")" 'calc-matrix-brackets
   "+" 'calc-remove-duplicates
   "," 'calc-vector-commas
   "-" 'calc-set-difference
   "." 'calc-full-vectors
   "/" 'calc-break-vectors
   ":" 'calc-set-span
   "<" 'calc-matrix-left-justify
   "=" 'calc-matrix-center-justify
   ">" 'calc-matrix-right-justify
   "?" 'calc-v-prefix-help
   "A" 'calc-apply
   "C" 'calc-cross
   "D" 'calc-mdet
   "E" 'calc-set-enumerate
   "F" 'calc-set-floor
   "G" 'calc-grade
   "H" 'calc-histogram
   "I" 'calc-inner-product
   "J" 'calc-conj-transpose
   "K" 'calc-kron
   "L" 'calc-mlud
   "M" 'calc-map
   "N" 'calc-cnorm
   "O" 'calc-outer-product
   "R" 'calc-reduce
   "S" 'calc-sort
   "T" 'calc-mtrace
   "U" 'calc-accumulate
   "V" 'calc-set-union
   "X" 'calc-set-xor
   "[" 'calc-vector-brackets
   "]" 'calc-matrix-brackets
   "^" 'calc-set-intersect
   "a" 'calc-arrange-vector
   "b" 'calc-build-vector
   "c" 'calc-mcol
   "d" 'calc-diag
   "e" 'calc-expand-vector
   "f" 'calc-vector-find
   "h" 'calc-head
   "i" 'calc-ident
   "k" 'calc-cons
   "l" 'calc-vlength
   "m" 'calc-mask-vector
   "n" 'calc-rnorm
   "p" 'calc-pack
   "r" 'calc-mrow
   "s" 'calc-subvector
   "t" 'calc-transpose
   "u" 'calc-unpack
   "v" 'calc-reverse-vector
   "x" 'calc-index
   "{" 'calc-vector-braces
   "}" 'calc-matrix-brackets
   "~" 'calc-set-complement)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "Z"
   "#" 'calc-kbd-query
   "'" 'calc-kbd-pop
   "(" 'calc-kbd-for
   ")" 'calc-kbd-end-for
   "/" 'calc-kbd-break
   ":" 'calc-kbd-else
   "<" 'calc-kbd-repeat
   "=" 'calc-kbd-report
   ">" 'calc-kbd-end-repeat
   "?" 'calc-shift-Z-prefix-help
   "C" 'calc-user-define-composition
   "D" 'calc-user-define
   "E" 'calc-user-define-edit
   "F" 'calc-user-define-formula
   "G" 'calc-get-user-defn
   "I" 'calc-user-define-invocation
   "K" 'calc-user-define-kbd-macro
   "P" 'calc-user-define-permanent
   "S" 'calc-edit-user-syntax
   "T" 'calc-timing
   "U" 'calc-user-undefine
   "[" 'calc-kbd-if
   "]" 'calc-kbd-end-if
   "`" 'calc-kbd-push
   "{" 'calc-kbd-loop
   "|" 'calc-kbd-else-if
   "}" 'calc-kbd-end-loop)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "Z"
   "#" 'calc-kbd-query
   "'" 'calc-kbd-pop
   "(" 'calc-kbd-for
   ")" 'calc-kbd-end-for
   "/" 'calc-kbd-break
   ":" 'calc-kbd-else
   "<" 'calc-kbd-repeat
   "=" 'calc-kbd-report
   ">" 'calc-kbd-end-repeat
   "?" 'calc-shift-Z-prefix-help
   "C" 'calc-user-define-composition
   "D" 'calc-user-define
   "E" 'calc-user-define-edit
   "F" 'calc-user-define-formula
   "G" 'calc-get-user-defn
   "I" 'calc-user-define-invocation
   "K" 'calc-user-define-kbd-macro
   "P" 'calc-user-define-permanent
   "S" 'calc-edit-user-syntax
   "T" 'calc-timing
   "U" 'calc-user-undefine
   "[" 'calc-kbd-if
   "]" 'calc-kbd-end-if
   "`" 'calc-kbd-push
   "{" 'calc-kbd-loop
   "|" 'calc-kbd-else-if
   "}" 'calc-kbd-end-loop)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "a"
   "M-!" 'calc-logical-not
   "M-\"" 'calc-expand-formula
   "M-#" 'calc-not-equal-to
   "M-%" 'calc-poly-rem
   "M-&" 'calc-logical-and
   "M-*" 'calc-product
   "M-+" 'calc-summation
   "M--" 'calc-alt-summation
   "M-." 'calc-remove-equal
   "M-/" 'calc-poly-div-rem
   "M-:" 'calc-logical-if
   "M-<" 'calc-less-than
   "M-=" 'calc-equal-to
   "M->" 'calc-greater-than
   "M-?" 'calc-a-prefix-help
   "M-A" 'calc-abs
   "M-F" 'calc-curve-fit
   "M-I" 'calc-num-integral
   "M-M" 'calc-map-equation
   "M-N" 'calc-find-minimum
   "M-P" 'calc-poly-roots
   "M-R" 'calc-find-root
   "M-S" 'calc-solve-for
   "M-T" 'calc-tabulate
   "M-X" 'calc-find-maximum
   "M-[" 'calc-less-equal
   "M-\\" 'calc-poly-div
   "M-]" 'calc-greater-equal
   "M-_" 'calc-subscript
   "M-a" 'calc-apart
   "M-b" 'calc-substitute
   "M-c" 'calc-collect
   "M-d" 'calc-derivative
   "M-e" 'calc-simplify-extended
   "M-f" 'calc-factor
   "M-g" 'calc-poly-gcd
   "M-i" 'calc-integral
   "M-m" 'calc-match
   "M-n" 'calc-normalize-rat
   "M-p" 'calc-poly-interp
   "M-r" 'calc-rewrite
   "M-s" 'calc-simplify
   "M-t" 'calc-taylor
   "M-v" 'calc-alg-evaluate
   "M-x" 'calc-expand
   "M-{" 'calc-in-set
   "M-|" 'calc-logical-or)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "b"
   "#" 'calc-fin-nper
   "%" 'calc-percent-change
   "?" 'calc-b-prefix-help
   "B" 'calc-log
   "D" 'calc-fin-ddb
   "F" 'calc-fin-fv
   "I" 'calc-fin-irr
   "L" 'calc-lshift-arith
   "M" 'calc-fin-pmt
   "N" 'calc-fin-npv
   "P" 'calc-fin-pv
   "R" 'calc-rshift-arith
   "S" 'calc-fin-sln
   "T" 'calc-fin-rate
   "Y" 'calc-fin-syd
   "a" 'calc-and
   "c" 'calc-clip
   "d" 'calc-diff
   "l" 'calc-lshift-binary
   "n" 'calc-not
   "o" 'calc-or
   "p" 'calc-pack-bits
   "r" 'calc-rshift-binary
   "t" 'calc-rotate-binary
   "u" 'calc-unpack-bits
   "w" 'calc-word-size
   "x" 'calc-xor
   )

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "c"
   "M-%" 'calc-convert-percent
   "M-0" 'calc-clean-num
   "M-1" 'calc-clean-num
   "M-2" 'calc-clean-num
   "M-3" 'calc-clean-num
   "M-4" 'calc-clean-num
   "M-5" 'calc-clean-num
   "M-6" 'calc-clean-num
   "M-7" 'calc-clean-num
   "M-8" 'calc-clean-num
   "M-9" 'calc-clean-num
   "M-?" 'calc-c-prefix-help
   "M-C" 'calc-cos
   "M-F" 'calc-fraction
   "M-c" 'calc-clean
   "M-d" 'calc-to-degrees
   "M-f" 'calc-float
   "M-h" 'calc-to-hms
   "M-p" 'calc-polar
   "M-r" 'calc-to-radians)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "b"
   "\""            'calc-display-strings
   "'"             'calc-display-raw
   ","             'calc-group-char
   "."             'calc-point-char
   "0"             'calc-decimal-radix
   "2"             'calc-binary-radix
   "6"             'calc-hex-radix
   "8"             'calc-octal-radix
   "<"             'calc-left-justify
   "="             'calc-center-justify
   ">"             'calc-right-justify
   "?"             'calc-d-prefix-help
   "@"             'calc-toggle-banner
   "A"             'calc-giac-language
   "B"             'calc-big-language
   "C"             'calc-c-language
   "D"             'calc-redo
   "E"             'calc-eqn-language
   "F"             'calc-fortran-language
   "L"             'calc-latex-language
   "M"             'calc-mathematica-language
   "N"             'calc-normal-language
   "O"             'calc-flat-language
   "P"             'calc-pascal-language
   "T"             'calc-tex-language
   "U"             'calc-unformatted-language
   "W"             'calc-maple-language
   "X"             'calc-maxima-language
   "Y"             'calc-yacas-language
   "["             'calc-truncate-up
   "]"             'calc-truncate-down
   "b"             'calc-line-breaking
   "c"             'calc-complex-notation
   "d"             'calc-date-notation
   "e"             'calc-eng-notation
   "f"             'calc-fix-notation
   "g"             'calc-group-digits
   "h"             'calc-hms-notation
   "i"             'calc-i-notation
   "j"             'calc-j-notation
   "l"             'calc-line-numbering
   "n"             'calc-normal-notation
   "o"             'calc-over-notation
   "p"             'calc-show-plain
   "r"             'calc-radix
   "s"             'calc-sci-notation
   "t"             'calc-truncate-stack
   "w"             'calc-auto-why
   "z"             'calc-leading-zeros
   "{"             'calc-left-label
   "}"             'calc-right-label)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "f"
   "M-?" 'calc-f-prefix-help
   "M-A" 'calc-abssqr
   "M-B" 'calc-inc-beta
   "M-E" 'calc-expm1
   "M-F" 'calc-floor
   "M-G" 'calc-inc-gamma
   "M-I" 'calc-ilog
   "M-L" 'calc-lnp1
   "M-M" 'calc-mant-part
   "M-Q" 'calc-isqrt
   "M-S" 'calc-scale-float
   "M-T" 'calc-arctan2
   "M-X" 'calc-xpon-part
   "M-[" 'calc-decrement
   "M-]" 'calc-increment
   "M-b" 'calc-beta
   "M-e" 'calc-erf
   "M-g" 'calc-gamma
   "M-h" 'calc-hypot
   "M-i" 'calc-im
   "M-j" 'calc-bessel-J
   "M-n" 'calc-min
   "M-r" 'calc-re
   "M-s" 'calc-sign
   "M-x" 'calc-max
   "M-y" 'calc-bessel-Y)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "g"
   "C-M-l" 'calc-graph-log-z
   "C-M-r" 'calc-graph-range-z
   "C-M-t" 'calc-graph-title-z
   "M-?" 'calc-g-prefix-help
   "M-A" 'calc-graph-add-3d
   "M-C" 'calc-graph-command
   "M-D" 'calc-graph-device
   "M-F" 'calc-graph-fast-3d
   "M-G" 'calc-argument
   "M-H" 'calc-graph-hide
   "M-K" 'calc-graph-kill
   "M-L" 'calc-graph-log-y
   "M-N" 'calc-graph-num-points
   "M-O" 'calc-graph-output
   "M-P" 'calc-graph-print
   "M-R" 'calc-graph-range-y
   "M-S" 'calc-graph-point-style
   "M-T" 'calc-graph-title-y
   "M-V" 'calc-graph-view-trail
   "M-X" 'calc-graph-geometry
   "M-Z" 'calc-graph-zero-y
   "M-a" 'calc-graph-add
   "M-b" 'calc-graph-border
   "M-c" 'calc-graph-clear
   "M-d" 'calc-graph-delete
   "M-f" 'calc-graph-fast
   "M-g" 'calc-graph-grid
   "M-h" 'calc-graph-header
   "M-j" 'calc-graph-juggle
   "M-k" 'calc-graph-key
   "M-l" 'calc-graph-log-x
   "M-n" 'calc-graph-name
   "M-p" 'calc-graph-plot
   "M-q" 'calc-graph-quit
   "M-r" 'calc-graph-range-x
   "M-s" 'calc-graph-line-style
   "M-t" 'calc-graph-title-x
   "M-v" 'calc-graph-view-commands
   "M-x" 'calc-graph-display
   "M-z" 'calc-graph-zero-x)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "zj"
   "C-M-h" 'calc-del-selection
   "C-M-j" 'calc-copy-selection
   "M-RET" 'calc-copy-selection
   "M-\"" 'calc-sel-expand-formula
   "M-&" 'calc-sel-invert
   "M-'" 'calc-enter-selection
   "M-*" 'calc-sel-mult-both-sides
   "M-+" 'calc-sel-add-both-sides
   "M--" 'calc-sel-sub-both-sides
   "M-/" 'calc-sel-div-both-sides
   "M-0" 'calc-select-part
   "M-1" 'calc-select-part
   "M-2" 'calc-select-part
   "M-3" 'calc-select-part
   "M-4" 'calc-select-part
   "M-5" 'calc-select-part
   "M-6" 'calc-select-part
   "M-7" 'calc-select-part
   "M-8" 'calc-select-part
   "M-9" 'calc-select-part
   "M-?" 'calc-j-prefix-help
   "M-C" 'calc-sel-commute
   "M-D" 'calc-sel-distribute
   "M-E" 'calc-sel-jump-equals
   "M-I" 'calc-sel-isolate
   "M-J" 'calc-conj
   "M-L" 'calc-commute-left
   "M-M" 'calc-sel-merge
   "M-N" 'calc-sel-negate
   "M-O" 'calc-select-once-maybe
   "M-R" 'calc-commute-right
   "M-S" 'calc-select-here-maybe
   "M-U" 'calc-sel-unpack
   "M-`" 'calc-edit-selection
   "M-a" 'calc-select-additional
   "M-b" 'calc-break-selections
   "M-c" 'calc-clear-selections
   "M-d" 'calc-show-selections
   "M-e" 'calc-enable-selections
   "M-l" 'calc-select-less
   "M-m" 'calc-select-more
   "M-n" 'calc-select-next
   "M-o" 'calc-select-once
   "M-p" 'calc-select-previous
   "M-r" 'calc-rewrite-selection
   "M-s" 'calc-select-here
   "M-u" 'calc-unselect
   "M-v" 'calc-sel-evaluate
   "M-DEL" 'calc-del-selection)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "zk"
   "M-?" 'calc-k-prefix-help
   "M-B" 'calc-utpb
   "M-C" 'calc-utpc
   "M-E" 'calc-extended-gcd
   "M-F" 'calc-utpf
   "M-K" 'calc-keep-args
   "M-N" 'calc-utpn
   "M-P" 'calc-utpp
   "M-T" 'calc-utpt
   "M-a" 'calc-random-again
   "M-b" 'calc-bernoulli-number
   "M-c" 'calc-choose
   "M-d" 'calc-double-factorial
   "M-e" 'calc-euler-number
   "M-f" 'calc-prime-factors
   "M-g" 'calc-gcd
   "M-h" 'calc-shuffle
   "M-l" 'calc-lcm
   "M-m" 'calc-moebius
   "M-n" 'calc-next-prime
   "M-p" 'calc-prime-test
   "M-r" 'calc-random
   "M-s" 'calc-stirling-number
   "M-t" 'calc-totient)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "zl"
   "*" 'calc-lu-times
   "+" 'calc-lu-plus
   "-" 'calc-lu-minus
   "/" 'calc-lu-divide
   "?" 'calc-l-prefix-help
   "d" 'calc-db
   "f" 'calc-freq
   "m" 'calc-midi
   "n" 'calc-np
   "q" 'calc-lu-quant
   "s" 'calc-spn)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "m"
   "?" 'calc-m-prefix-help
   "A" 'calc-alg-simplify-mode
   "B" 'calc-bin-simplify-mode
   "C" 'calc-auto-recompute
   "D" 'calc-default-simplify-mode
   "E" 'calc-ext-simplify-mode
   "F" 'calc-settings-file-name
   "I" 'calc-basic-simplify-mode
   "M" 'calc-more-recursion-depth
   "N" 'calc-num-simplify-mode
   "O" 'calc-no-simplify-mode
   "R" 'calc-mode-record-mode
   "S" 'calc-shift-prefix
   "U" 'calc-units-simplify-mode
   "X" 'calc-load-everything
   "a" 'calc-algebraic-mode
   "d" 'calc-degrees-mode
   "e" 'calc-embedded-preserve-modes
   "f" 'calc-frac-mode
   "g" 'calc-get-modes
   "h" 'calc-hms-mode
   "i" 'calc-infinite-mode
   "m" 'calc-save-modes
   "p" 'calc-polar-mode
   "r" 'calc-radians-mode
   "s" 'calc-symbolic-mode
   "t" 'calc-total-algebraic-mode
   "v" 'calc-matrix-mode
   "w" 'calc-working
   "x" 'calc-always-load-extensions)


  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "r"
   "M-0" 'calc-recall-quick
   "M-1" 'calc-recall-quick
   "M-2" 'calc-recall-quick
   "M-3" 'calc-recall-quick
   "M-4" 'calc-recall-quick
   "M-5" 'calc-recall-quick
   "M-6" 'calc-recall-quick
   "M-7" 'calc-recall-quick
   "M-8" 'calc-recall-quick
   "M-9" 'calc-recall-quick
   "M-?" 'calc-r-prefix-help
   "M-i" 'calc-insert-register
   "M-s" 'calc-copy-to-register)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "s"
   "&" 'calc-store-inv
   "*" 'calc-store-times
   "+" 'calc-store-plus
   "-" 'calc-store-minus
   "/" 'calc-store-div
   "0" 'calc-store-quick
   "1" 'calc-store-quick
   "2" 'calc-store-quick
   "3" 'calc-store-quick
   "4" 'calc-store-quick
   "5" 'calc-store-quick
   "6" 'calc-store-quick
   "7" 'calc-store-quick
   "8" 'calc-store-quick
   "9" 'calc-store-quick
   ":" 'calc-assign
   "=" 'calc-evalto
   "?" 'calc-s-prefix-help
   "A" 'calc-edit-AlgSimpRules
   "D" 'calc-edit-Decls
   "E" 'calc-edit-EvalRules
   "F" 'calc-edit-FitRules
   "G" 'calc-edit-GenCount
   "H" 'calc-edit-Holidays
   "I" 'calc-edit-IntegLimit
   "L" 'calc-edit-LineStyles
   "P" 'calc-edit-PointStyles
   "R" 'calc-edit-PlotRejects
   "S" 'calc-sin
   "T" 'calc-edit-TimeZone
   "U" 'calc-edit-Units
   "X" 'calc-edit-ExtSimpRules
   "[" 'calc-store-decr
   "]" 'calc-store-incr
   "^" 'calc-store-power
   "c" 'calc-copy-variable
   "d" 'calc-declare-variable
   "e" 'calc-edit-variable
   "i" 'calc-insert-variables
   "k" 'calc-copy-special-constant
   "l" 'calc-let
   "m" 'calc-store-map
   "n" 'calc-store-neg
   "p" 'calc-permanent-variable
   "r" 'calc-recall
   "s" 'calc-store
   "t" 'calc-store-into
   "u" 'calc-unstore
   "x" 'calc-store-exchange
   "|" 'calc-store-concat)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "t"
   "M-+" 'calc-business-days-plus
   "M--" 'calc-business-days-minus
   "M-." 'calc-full-trail-vectors
   "M-0" 'calc-store-into-quick
   "M-1" 'calc-store-into-quick
   "M-2" 'calc-store-into-quick
   "M-3" 'calc-store-into-quick
   "M-4" 'calc-store-into-quick
   "M-5" 'calc-store-into-quick
   "M-6" 'calc-store-into-quick
   "M-7" 'calc-store-into-quick
   "M-8" 'calc-store-into-quick
   "M-9" 'calc-store-into-quick
   "M-<" 'calc-trail-scroll-left
   "M->" 'calc-trail-scroll-right
   "M-?" 'calc-t-prefix-help
   "M-C" 'calc-convert-time-zones
   "M-D" 'calc-date
   "M-I" 'calc-inc-month
   "M-J" 'calc-julian
   "M-M" 'calc-new-month
   "M-N" 'calc-now
   "M-P" 'calc-date-part
   "M-T" 'calc-tan
   "M-U" 'calc-unix-time
   "M-W" 'calc-new-week
   "M-Y" 'calc-new-year
   "M-Z" 'calc-time-zone
   "M-[" 'calc-trail-first
   "M-]" 'calc-trail-last
   "M-b" 'calc-trail-backward
   "M-d" 'calc-trail-display
   "M-f" 'calc-trail-forward
   "M-h" 'calc-trail-here
   "M-i" 'calc-trail-in
   "M-k" 'calc-trail-kill
   "M-m" 'calc-trail-marker
   "M-n" 'calc-trail-next
   "M-o" 'calc-trail-out
   "M-p" 'calc-trail-previous
   "M-r" 'calc-trail-isearch-backward
   "M-s" 'calc-trail-isearch-forward
   "M-y" 'calc-trail-yank
   "M-{" 'calc-trail-backward
   "M-}" 'calc-trail-forward)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "U"
   "M-#" 'calc-vector-count
   "M-*" 'calc-vector-product
   "M-+" 'calc-vector-sum
   "M-0" 'calc-quick-units
   "M-1" 'calc-quick-units
   "M-2" 'calc-quick-units
   "M-3" 'calc-quick-units
   "M-4" 'calc-quick-units
   "M-5" 'calc-quick-units
   "M-6" 'calc-quick-units
   "M-7" 'calc-quick-units
   "M-8" 'calc-quick-units
   "M-9" 'calc-quick-units
   "M-?" 'calc-u-prefix-help
   "M-C" 'calc-vector-covariance
   "M-G" 'calc-vector-geometric-mean
   "M-M" 'calc-vector-mean
   "M-N" 'calc-vector-min
   "M-R" 'calc-vector-rms
   "M-S" 'calc-vector-sdev
   "M-U" 'calc-undo
   "M-V" 'calc-view-units-table
   "M-X" 'calc-vector-max
   "M-a" 'calc-autorange-units
   "M-b" 'calc-base-units
   "M-c" 'calc-convert-units
   "M-d" 'calc-define-unit
   "M-e" 'calc-explain-units
   "M-g" 'calc-get-unit-definition
   "M-n" 'calc-convert-exact-units
   "M-p" 'calc-permanent-units
   "M-r" 'calc-remove-units
   "M-s" 'calc-simplify-units
   "M-t" 'calc-convert-temperature
   "M-u" 'calc-undefine-unit
   "M-v" 'calc-enter-units-table
   "M-x" 'calc-extract-units)

  (:keymaps '(calc-mode-map)
   :states '(normal visual)
   :prefix "v"
   "#" 'calc-set-cardinality
   "&" 'calc-inv
   "(" 'calc-vector-parens
   ")" 'calc-matrix-brackets
   "+" 'calc-remove-duplicates
   "," 'calc-vector-commas
   "-" 'calc-set-difference
   "." 'calc-full-vectors
   "/" 'calc-break-vectors
   ":" 'calc-set-span
   "<" 'calc-matrix-left-justify
   "=" 'calc-matrix-center-justify
   ">" 'calc-matrix-right-justify
   "?" 'calc-v-prefix-help
   "A" 'calc-apply
   "C" 'calc-cross
   "D" 'calc-mdet
   "E" 'calc-set-enumerate
   "F" 'calc-set-floor
   "G" 'calc-grade
   "H" 'calc-histogram
   "I" 'calc-inner-product
   "J" 'calc-conj-transpose
   "K" 'calc-kron
   "L" 'calc-mlud
   "M" 'calc-map
   "N" 'calc-cnorm
   "O" 'calc-outer-product
   "R" 'calc-reduce
   "S" 'calc-sort
   "T" 'calc-mtrace
   "U" 'calc-accumulate
   "V" 'calc-set-union
   "X" 'calc-set-xor
   "[" 'calc-vector-brackets
   "]" 'calc-matrix-brackets
   "^" 'calc-set-intersect
   "a" 'calc-arrange-vector
   "b" 'calc-build-vector
   "c" 'calc-mcol
   "d" 'calc-diag
   "e" 'calc-expand-vector
   "f" 'calc-vector-find
   "h" 'calc-head
   "i" 'calc-ident
   "k" 'calc-cons
   "l" 'calc-vlength
   "m" 'calc-mask-vector
   "n" 'calc-rnorm
   "p" 'calc-pack
   "r" 'calc-mrow
   "s" 'calc-subvector
   "t" 'calc-transpose
   "u" 'calc-unpack
   "v" 'calc-reverse-vector
   "x" 'calc-index
   "{" 'calc-vector-braces
   "}" 'calc-matrix-brackets
   "~" 'calc-set-complement)

  ;; "zz" (lookup-key calc-mode-map (kbd "z"))

  (:keymaps '(calc-mode-map)
   :states '(visual)
   "d" 'calc-kill-region))

(use-package openwith
  :ensure t
  :defer .3
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

(use-package exec-path-from-shell
  :ensure t
  :when *is-mac*
  :defer 1
  :config
  (exec-path-from-shell-initialize))

(use-package ffap
  :ensure nil
  :defer t
  :config
  ;; Don't try to ping things that look like domain names
  (setq ffap-machine-p-known 'reject))

(use-package terminal-here
  :ensure t
  :config
  (defun terminal-here-default-terminal-command (_dir)
    "Pick a good default command to use for DIR."
    (cond
     (*is-mac*
      (list "open" "-a" "iTerm.app" "."))

     ;; From http://stackoverflow.com/a/13509208/874671
     (*is-win*
      ;; (list "cmd.exe" "/C" "start" "nyagos.exe")
      (list "cmd.exe" "/C" "start" "nyagos.exe"))

     ;; Probably X11!
     (t '("x-terminal-emulator"))))
  (setq terminal-here-terminal-command 'terminal-here-default-terminal-command)
  :general
  (:states '(normal visual)
   :prefix global-leader
   "at" 'terminal-here-launch))

(use-package tramp
  :ensure t
  :defer t)

(use-package undo-tree
  :ensure t
  :delight undo-tree-mode
  :defer t
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region nil)
  :general
  (:states '(normal visual)
   :prefix global-leader
   "tu" 'undo-tree-visualize))

(use-package eldoc
  :ensure nil
  :delight eldoc-mode)

;; (use-package leuven-theme
;;   :ensure t
;;   :config
;;   (load-theme 'leuven t))

(use-package elec-pair
  :disabled t
  :ensure nil
  :hook (prog-mode . electric-pair-mode)
  :config
  (defun inhibit-electric-pair-mode (char)
    (minibufferp))
  (setq electric-pair-inhibit-predicate #'inhibit-electric-pair-mode))

;; (use-package outorg
;;   :ensure t
;;   :defer t
;;   :after org
;;   :init
;;   (defvar outline-minor-mode-prefix "\M-#"))

(use-package paren
  :ensure nil
  :hook (prog-mode . show-paren-mode))

(use-package recentf
  :ensure nil
  :defer t
  :after no-littering
  :init
  (setq recentf-max-menu-items 100)
  (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                         (recentf-mode))))
  :config
  (setq recentf-max-menu-items 100)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package vlf
  :ensure t
  :defer t
  :commands vlf-mode)

(use-package try
  :ensure t
  :commands (try))

;;;; Emacs lisp

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

(use-package lispy
  :ensure t
  :delight lispy-mode
  :hook (emacs-lisp-mode . lispy-mode))

(use-package lispyville
  :ensure t
  :delight lispyville-mode
  :hook (lispy-mode . lispyville-mode))

;;;; Org

;; TODO: Maybe add org-download
(use-package org
  :ensure org-plus-contrib
  :defer .3
  :preface
  ;; (unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))
  ;;   (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))
  :init
  :config
  (setq org-directory "~/org")
  (setq org-src-preserve-indentation nil
        org-export-with-sub-superscripts nil
        org-edit-src-content-indentation 0
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-hide-leading-stars t
        org-startup-folded nil
        org-agenda-inhibit-startup t)
  (add-to-list 'org-file-apps '("\\.xls\\'" . default))
  (add-hook 'org-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
  (setq org-capture-templates
        '(("n" "Notes" entry
           (file "~/org/notes.org") "* %?\n")
          ("t" "Todo" entry
           (file "~/org/tasks.org") "* TODO %?\n SCHEDULED: %^t\n")))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)")))
  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (calc . t)
     (plantuml . t)
     (shell . t)
     (sql . t)))
  (add-hook 'dired-mode-hook 'org-download-enable)
  :general
  (:states '(normal visual)
   :prefix global-leader
   "oc" 'org-capture)
  (:states '(normal)
   :keymaps '(org-mode-map)
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

(use-package org-download
  :ensure t
  :defer
  :hook ((org-mode . org-download-enable))
  :config
  (setq-default org-download-image-dir "~/org/pictures"))

(use-package ob-sql-mode
  :ensure t
  :defer t
  :after org)

(use-package ox-md
  :ensure nil
  :defer t
  :after org)

(use-package org-bullets
  :ensure t
  :disabled
  :hook (org-mode . org-bullets-mode))

(use-package navi-mode
  :ensure t
  :defer t)

(use-package helm-navi
  :ensure t
  :commands helm-navi
  :config
  (defun helm-navi--get-candidates-in-buffer (buffer &optional regexp)
    "Return Outshine heading candidates in BUFFER.
Optional argument REGEXP is a regular expression to match, a
function to return a regular expression, or
`outshine-promotion-headings' by default."
    ;; Much of this code is copied from helm-org.el
    (with-current-buffer buffer
      ;; Make sure outshine is loaded
      (unless outshine-promotion-headings
        (error "Outshine is not activated in buffer \"%s\".  Activate `outline-minor-mode', or consult Outshine's documentation for further instructions if necessary." (buffer-name buffer)))
      (let* ((heading-regexp (pcase regexp
                               ((pred functionp) (funcall regexp))
                               ((pred stringp) regexp)
                               ((pred null) (concat "^\\("
                                                    (mapconcat (lambda (s)
                                                                 (s-trim (car s)))
                                                               outshine-promotion-headings
                                                               "\\|")
                                                    "\\)"
                                                    "\s+\\(.*\\)$"))))
             (match-fn (if helm-navi-fontify
                           #'match-string
                         #'match-string-no-properties))
             (search-fn (lambda ()
                          (re-search-forward heading-regexp nil t))))
        (save-excursion
          (save-restriction
            (goto-char (point-min))
            (cl-loop while (funcall search-fn)
                     for beg = (point-at-bol)
                     for end = (point-at-eol)
                     when (and helm-navi-fontify
                               (null (text-property-any
                                      beg end 'fontified t)))
                     do (jit-lock-fontify-now beg end)
                     for level = (length (match-string-no-properties 1))
                     for heading = (if regexp
                                       (funcall match-fn 0)
                                     (concat (match-string 1) " " (funcall match-fn 2)))
                     if (or regexp
                            (and (>= level helm-org-headings-min-depth)
                                 (<= level helm-org-headings-max-depth)))
                     collect `(,heading . ,(point-marker))))))))
  (defun helm-navi--get-regexp ()
    "Return regexp for all headings and keywords in current buffer."
    (concat (navi-make-regexp-alternatives
             (navi-get-regexp (car
                               (split-string
                                (symbol-name major-mode)
                                "-mode" 'OMIT-NULLS))
                              :ALL)
             (mapconcat (lambda (s)
                          (s-trim (car s)))
                        outshine-promotion-headings
                        "\\|"))
            ".*$")))

(use-package helm-org-rifle
  :ensure t
  :config
  (setq helm-org-rifle-close-unopened-file-buffers nil)
  :general
  (:states '(normal visual)
   :prefix global-leader
   "or" 'helm-org-rifle-org-directory))

(use-package evil-org
  :ensure t
  :after org
  :delight evil-org-mode
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . evil-org-set-key-theme))
  :init
  (setq evil-org-special-o/O nil)
  ;; (setf evil-org-key-theme '(textobjects insert navigation additional shift todo heading))
  (setf evil-org-key-theme '(navigation calendar additional shift return)))

;;;; Text

(use-package text-mode
  :ensure nil
  :init
  (setq-default major-mode 'text-mode))

;;;; Web

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

;;;; Javascript

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)))

;;;; Markdown

(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;;;; SQL

(use-package sql-indent
  :ensure t
  :defer .3)

;;;; Fish

(use-package fish-mode
  :ensure t
  :defer t
  :mode (("\\.fish\\'" . fish-mode)))

;;;; Plantuml

(use-package plantuml-mode
  :ensure t
  :defer t
  :mode (("\\.pu\\'" . plantuml-mode)
         ("\\.plantuml\\'" . plantuml-mode)))

;;;; Vimscript

(use-package vimrc-mode
  :ensure t
  :defer t
  :mode (("\\.vim\\(rc\\)?\\'" . vimrc-mode))
  :config
  (setq vimrc-font-lock-keywords
        `(
          ;; Line comment
          ("^[\t ]*\\(\"\\)\\(.*\\)$"
           (1 font-lock-comment-delimiter-face)
           (2 font-lock-comment-face))

          ;; Trailing comment
          ("[\t ]+\\(\"\\)\\([^\"\r\n]*\\)$"
           (1 font-lock-comment-delimiter-face)
           (2 font-lock-comment-face))

          ;; String start:
          ("\\(\"[^\n\r\"]*\"\\)\\|\\('[^\n\r]*'\\)"
           (0 font-lock-string-face)) ;; String end;

          ;; Function-name start:
          ("^[ \t]*\\(fun\\(?:ction\\)?\\)!?[ \t]+\\([a-zA-Z0-9_:#]+\\)?"
           (2 font-lock-function-name-face nil t)) ;; Function-name end;
          ("\\(\\([a-zA-Z]*:\\)?[a-zA-Z]*\\)("
           (1 font-lock-function-name-face nil t)) ;; Function-name end;

          ;; Variables
          ("\\<[bwglsav]:[a-zA-Z_][a-zA-Z0-9#_]*\\>"
           (0 font-lock-variable-name-face))
          ("\\(let[ \t]+\\)\\<\\([a-zA-Z_][a-zA-Z0-9#_]*\\)\\>"
           (2 font-lock-variable-name-face))

          ;; Options which can be prefixed with `no'
          (,(concat "[^_]\\<\\(\\(?:no\\)?"
                    (regexp-opt '("autochdir" "acd"
                                  "allowrevins" "ari"
                                  "altkeymap" "akm"
                                  "antialias" "anti"
                                  "arabic" "arab"
                                  "arabicshape" "arshape"
                                  "autoindent" "ai"
                                  "autoread" "ar"
                                  "autowrite" "aw"
                                  "autowriteall" "awa"
                                  "backup" "bk"
                                  "ballooneval" "beval"
                                  "binary" "bin"
                                  "bioskey" "biosk"
                                  "bomb"
                                  "buflisted" "bl"
                                  "cindent" "cin"
                                  "compatible" "cp"
                                  "confirm" "cf"
                                  "conskey" "consk"
                                  "copyindent" "ci"
                                  "cscopetag" "cst"
                                  "cscopeverbose" "csverb"
                                  "cursorbind" "crb"
                                  "cursorcolumn" "cuc"
                                  "cursorline" "cul"
                                  "delcombine" "deco"
                                  "diff"
                                  "digraph" "dg"
                                  "edcompatible" "ed"
                                  "endofline" "eol"
                                  "equalalways" "ea"
                                  "errorbells" "eb"
                                  "esckeys" "ek"
                                  "expandtab" "et"
                                  "exrc" "ex"
                                  "fkmap" "fk"
                                  "foldenable" "fen"
                                  "gdefault" "gd"
                                  "guipty"
                                  "hidden" "hid"
                                  "hkmap" "hk"
                                  "hkmapp" "hkp"
                                  "hlsearch" "hls"
                                  "icon"
                                  "ignorecase" "ic"
                                  "imcmdline" "imc"
                                  "imdisable" "imd"
                                  "incsearch" "is"
                                  "infercase" "inf"
                                  "insertmode"  "im"
                                  "joinspaces" "js"
                                  "lazyredraw" "lz"
                                  "linebreak" "lbr"
                                  "lisp"
                                  "list"
                                  "loadplugins" "lpl"
                                  "macatsui"
                                  "magic"
                                  "modeline" "ml"
                                  "modifiable" "ma"
                                  "modified" "mod"
                                  "more"
                                  "mousefocus" "mousef"
                                  "mousehide" "mh"
                                  "number" "nu"
                                  "opendevice" "odev"
                                  "paste"
                                  "preserveindent" "pi"
                                  "previewwindow" "pvw"
                                  "prompt"
                                  "readonly" "ro"
                                  "relativenumber" "rnu"
                                  "remap"
                                  "restorescreen" "rs"
                                  "revins" "ri"
                                  "rightleft" "rl"
                                  "ruler" "ru"
                                  "scrollbind" "scb"
                                  "secure"
                                  "shellslash" "ssl"
                                  "shelltemp" "stmp"
                                  "shiftround" "sr"
                                  "shortname" "sn"
                                  "showcmd" "sc"
                                  "showfulltag" "sft"
                                  "showmatch" "sm"
                                  "showmode" "smd"
                                  "smartcase" "scs"
                                  "smartindent" "si"
                                  "smarttab" "sta"
                                  "spell"
                                  "splitbelow" "sb"
                                  "splitright" "spr"
                                  "startofline" "sol"
                                  "swapfile" "swf"
                                  "tagbsearch" "tbs"
                                  "tagrelative" "tr"
                                  "tagstack" "tgst"
                                  "termbidi" "tbidi"
                                  "terse"
                                  "textauto" "ta"
                                  "textmode" "tx"
                                  "tildeop" "top"
                                  "timeout" "to"
                                  "title"
                                  "ttimeout"
                                  "ttybuiltin" "tbi"
                                  "ttyfast" "tf"
                                  "visualbell" "vb"
                                  "warn"
                                  "weirdinvert" "wiv"
                                  "wildmenu" "wmnu"
                                  "winfixheight" "wfh"
                                  "winfixwidth" "wfw"
                                  "wrap"
                                  "wrapscan" "ws"
                                  "write"
                                  "writeany" "wa"
                                  "writebackup" "wb"
                                  ) t)
                    "\\)\\>[^_]" )
           1 '(face vimrc-option))

          ;; The rest of the options
          (,(concat "[^_]"
                    (regexp-opt '("aleph" "al"
                                  "ambiwidth" "ambw"
                                  "arabicshape" "arshape"
                                  "background" "bg"
                                  "backspace" "bs"
                                  "backupcopy" "bkc"
                                  "backupdir" "bdir"
                                  "backupext" "bex"
                                  "backupskip" "bsk"
                                  "balloondelay" "bdlay"
                                  "balloonexpr" "bexpr"
                                  "breakat" "brk"
                                  "breakindent" "bri"
                                  "breakindentopt" "briopt"
                                  "browsedir" "bsdir"
                                  "bufhidden" "bh"
                                  "buftype" "bt"
                                  "casemap" "cmp"
                                  "cdpath" "cd"
                                  "cedit"
                                  "charconvert" "ccv"
                                  "cinkeys" "cink"
                                  "cinoptions" "cino"
                                  "cinwords" "cinw"
                                  "clipboard" "cb"
                                  "cmdheight" "ch"
                                  "cmdwinheight" "cwh"
                                  "colorcolumn" "cc"
                                  "columns" "co"
                                  "comments" "com"
                                  "commentstring" "cms"
                                  "complete"
                                  "completefunc" "cfu"
                                  "completeopt" "cot"
                                  "concealcursor" "cocu"
                                  "conceallevel" "cole"
                                  "cpoptions" "cpo"
                                  "cryptmethod" "cm"
                                  "cscopepathcomp" "cspc"
                                  "cscopeprg" "csprg"
                                  "cscopequickfix" "csqf"
                                  "cscopetagorder" "csto"
                                  "cscopeverbose" "csverb"
                                  "debug"
                                  "define" "def"
                                  "dictionary" "dict"
                                  "dex" "diffexpr"
                                  "dip" "diffopt"
                                  "directory" "dir"
                                  "display" "dy"
                                  "eadirection" "ead"
                                  "encoding" "enc"
                                  "equalprg" "ep"
                                  "errorfile" "ef"
                                  "errorformat" "efm"
                                  "eventignore" "ei"
                                  "fileencoding" "fenc"
                                  "fe"
                                  "fileencodings" "fencs"
                                  "fileformat" "ff"
                                  "fileformats" "ffs"
                                  "filetype" "ft"
                                  "fillchars" "fcs"
                                  "fkmap" "fk"
                                  "foldclose" "fcl"
                                  "foldcolumn" "fdc"
                                  "foldexpr" "fde"
                                  "foldignore" "fdi"
                                  "foldlevel" "fdl"
                                  "foldlevelstart" "fdls"
                                  "foldmarker" "fmr"
                                  "foldmethod" "fdm"
                                  "foldminlines" "fml"
                                  "foldnestmax" "fdn"
                                  "foldopen" "fdo"
                                  "foldtext" "fdt"
                                  "formatoptions" "fo"
                                  "formatlistpat" "flp"
                                  "formatprg" "fp"
                                  "formatexpr" "fex"
                                  "fsync" "fs"
                                  "grepformat" "gfm"
                                  "grepprg" "gp"
                                  "guicursor" "gcr"
                                  "guifont" "gfn"
                                  "guifontset" "gfs"
                                  "guifontwide" "gfw"
                                  "guiheadroom" "ghr"
                                  "guioptions" "go"
                                  "guitablabel" "gtl"
                                  "guitabtooltip" "gtt"
                                  "helpfile" "hf"
                                  "helpheight" "hh"
                                  "helplang" "hlg"
                                  "highlight" "hl"
                                  "history" "hi"
                                  "iconstring"
                                  "imactivatefunc" "imaf"
                                  "imactivatekey" "imak"
                                  "iminsert" "imi"
                                  "imsearch" "ims"
                                  "imstatusfunc" "imsf"
                                  "include" "inc"
                                  "includeexpr" "inex"
                                  "indentexpr" "inde"
                                  "indentkeys" "indk"
                                  "isfname" "isf"
                                  "isident" "isi"
                                  "iskeyword" "isk"
                                  "isprint" "isp"
                                  "key"
                                  "keymap" "kmp"
                                  "keymodel" "km"
                                  "keywordprg" "kp"
                                  "langmap" "lmap"
                                  "langmenu" "lm"
                                  "laststatus" "ls"
                                  "lines"
                                  "linespace" "lsp"
                                  "lispwords" "lw"
                                  "listchars" "lcs"
                                  "makeef" "mef"
                                  "makeprg" "mp"
                                  "matchpairs" "mps"
                                  "matchtime" "mat"
                                  "maxcombine" "mco"
                                  "maxfuncdepth" "mfd"
                                  "maxmapdepth" "mmd"
                                  "maxmem" "mm"
                                  "maxmempattern" "mmp"
                                  "maxmemtot" "mmt"
                                  "menuitems" "mis"
                                  "mkspellmem" "msm"
                                  "modelines" "mls"
                                  "mouse"
                                  "mousemodel" "mousem"
                                  "mouseshape" "mouses"
                                  "mousetime" "mouset"
                                  "mzquantum" "mzq"
                                  "nrformats" "nf"
                                  "numberwidth" "nuw"
                                  "omnifunc" "ofu"
                                  "operatorfunc" "opfunc"
                                  "osfiletype" "oft"
                                  "paragraphs" "para"
                                  "pastetoggle" "pt"
                                  "pex" "patchexpr"
                                  "patchmode" "pm"
                                  "path" "pa"
                                  "previewheight" "pvh"
                                  "printdevice" "pdev"
                                  "printencoding" "penc"
                                  "printexpr" "pexpr"
                                  "printfont" "pfn"
                                  "printheader" "pheader"
                                  "printmbcharset" "pmbcs"
                                  "printmbfont" "pmbfn"
                                  "printoptions" "popt"
                                  "pumheight" "ph"
                                  "quoteescape" "qe"
                                  "redrawtime" "rdt"
                                  "regexpengine" "re"
                                  "report"
                                  "rightleftcmd" "rlc"
                                  "rulerformat" "ruf"
                                  "runtimepath" "rtp"
                                  "scroll" "scr"
                                  "scrolljump" "sj"
                                  "scrolloff" "so"
                                  "scrollopt" "sbo"
                                  "sections" "sect"
                                  "selection" "sel"
                                  "selectmode" "slm"
                                  "sessionoptions" "ssop"
                                  "shell" "sh"
                                  "shellcmdflag" "shcf"
                                  "shellpipe" "sp"
                                  "shellquote" "shq"
                                  "shellredir" "srr"
                                  "shelltype" "st"
                                  "shellxescape" "sxe"
                                  "shellxquote" "sxq"
                                  "shiftwidth" "sw"
                                  "shortmess" "shm"
                                  "showbreak" "sbr"
                                  "showtabline" "stal"
                                  "sidescroll" "ss"
                                  "sidescrolloff" "siso"
                                  "softtabstop" "sts"
                                  "spellcapcheck" "spc"
                                  "spellfile" "spf"
                                  "spelllang" "spl"
                                  "spellsuggest" "sps"
                                  "statusline" "stl"
                                  "suffixes" "su"
                                  "suffixesadd" "sua"
                                  "swapsync" "sws"
                                  "switchbuf" "swb"
                                  "synmaxcol" "smc"
                                  "syntax" "syn"
                                  "tabline" "tal"
                                  "tabpagemax" "tpm"
                                  "tabstop" "ts"
                                  "taglength" "tl"
                                  "tags" "tag"
                                  "term"
                                  "termbidi" "tbidi"
                                  "termencoding" "tenc"
                                  "textwidth" "tw"
                                  "thesaurus" "tsr"
                                  "timeoutlen" "tm"
                                  "ttimeoutlen" "ttm"
                                  "titlelen"
                                  "titleold"
                                  "titlestring"
                                  "toolbar" "tb"
                                  "toolbariconsize" "tbis"
                                  "ttymouse" "ttym"
                                  "ttyscroll" "tsl"
                                  "ttytype" "tty"
                                  "undodir" "udir"
                                  "undolevels" "ul"
                                  "undoreload" "ur"
                                  "updatecount" "uc"
                                  "updatetime" "ut"
                                  "verbose" "vbs"
                                  "verbosefile" "vfile"
                                  "viewdir" "vdir"
                                  "viewoptions" "vop"
                                  "viminfo" "vi"
                                  "virtualedit" "ve"
                                  "whichwrap" "ww"
                                  "wildchar" "wc"
                                  "wildcharm" "wcm"
                                  "wildignore" "wig"
                                  "wildmode" "wim"
                                  "wildoptions" "wop"
                                  "winaltkeys" "wak"
                                  "window" "wi"
                                  "winheight" "wh"
                                  "winminheight" "wmh"
                                  "winminwidth" "wmw"
                                  "winwidth" "wiw"
                                  "wrapmargin" "wm"
                                  "wrapscan" "ws"
                                  "writedelay" "wd"
                                  ) 'words)
                    "[^_]")
           1 '(face vimrc-option))

          ;; Ex commands
          (,(concat "\\(^\\|[^_]\\)"
                    (regexp-opt '("Next" "N"
                                  "Print" "P"
                                  "X"
                                  "abclear" "abc"
                                  "aboveleft" "abo"
                                  "all" "al"
                                  "am" "amenu" "an" "anoremenu"
                                  "argadd" "arga"
                                  "argdelete" "argd"
                                  "argdo"
                                  "argedit" "arge"
                                  "argglobal" "argg"
                                  "arglocal" "argl"
                                  "args" "ar"
                                  "argument" "argu"
                                  "ascii" "as"
                                  "augroup" "aug"
                                  "autocmd" "au"
                                  "bNext" "bN"
                                  "badd" "bad"
                                  "ball" "ba"
                                  "bdelete" "bd"
                                  "belowright" "bel"
                                  "bfirst" "bf"
                                  "blast" "bl"
                                  "bmodified" "bm"
                                  "bnext" "bn"
                                  "botright" "bo"
                                  "bprevious" "bp"
                                  "break" "brea"
                                  "breakadd" "breaka"
                                  "breakdel" "breakd"
                                  "breaklist" "breakl"
                                  "brewind" "br"
                                  "browse" "bro"
                                  "bufdo"
                                  "buffer" "b"
                                  "bunload" "bun"
                                  "bwipeout" "bw"
                                  "cNext" "cN"
                                  "cNfile" "cNf"
                                  "cabclear" "cabc"
                                  "caddbuffer" "caddb"
                                  "caddexpr" "cad"
                                  "caddfile" "caddf"
                                  "call" "cal"
                                  "catch" "cat"
                                  "cbuffer" "cb"
                                  "cc"
                                  "cclose" "ccl"
                                  "cd"
                                  "center" "ce"
                                  "cexpr" "cex"
                                  "cfile" "cf"
                                  "cfirst" "cfir"
                                  "cgetbuffer" "cgetb"
                                  "cgetexpr" "cgete"
                                  "cgetfile" "cg"
                                  "change" "c"
                                  "changes"
                                  "chdir" "chd"
                                  "checkpath" "ckpath"
                                  "checktime" "checkt"
                                  "clast" "cla"
                                  "clist" "cl"
                                  "close" "clo"
                                  "cm" "cmap"
                                  "cmapc" "cmapclear"
                                  "cme" "cmenu" "cnoreme" "cnoremenu"
                                  "cnewer" "cnew"
                                  "cnext" "cn"
                                  "cnfile" "cnf"
                                  "cno" "cnoremap"
                                  "colder" "col"
                                  "colo" "colorscheme"
                                  "comclear" "comc"
                                  "command" "com"
                                  "compiler" "comp"
                                  "confirm" "conf"
                                  "continue" "con"
                                  "copen" "cope"
                                  "copy" "co"
                                  "cpfile" "cpf"
                                  "cprevious" "cp"
                                  "cquit" "cq"
                                  "crewind" "cr"
                                  "cu" "cunmap"
                                  "cunabbrev" "cuna"
                                  "cwindow" "cw"
                                  "debuggreedy" "debugg"
                                  "delcommand" "delc"
                                  "delete" "d"
                                  "delfunction" "delf"
                                  "delmarks" "delm"
                                  "diffget" "diffg"
                                  "diffoff"
                                  "diffpatch"
                                  "diffput" "diffpu"
                                  "diffsplit"
                                  "diffthis"
                                  "diffupdate" "diffu"
                                  "digraphs" "dig"
                                  "display" "di"
                                  "djump" "dj"
                                  "dlist" "dl"
                                  "drop" "dr"
                                  "dsearch" "ds"
                                  "dsplit" "dsp"
                                  "earlier"
                                  "echoerr" "echoe"
                                  "echomsg" "echom"
                                  "echon"
                                  "edit" "e"
                                  "else" "el"
                                  "elseif" "elsei"
                                  "em" "emenu"
                                  "endf" "endfunction"
                                  "enew" "ene"
                                  "ex"
                                  "execute" "exe"
                                  "exit" "exi"
                                  "file" "fi" "f"
                                  "files" "buffers" "ls"
                                  "filetype" "filet"
                                  "finally" "fina"
                                  "find" "fin"
                                  "finish" "fini"
                                  "first" "fir"
                                  "fixdel" "fix"
                                  "fold" "fo"
                                  "foldclose" "foldc"
                                  "folddoclosed" "folddoc"
                                  "folddoopen" "foldd"
                                  "foldopen" "foldo"
                                  "for" "endfo" "endfor"
                                  "fu" "fun" "function"
                                  "goto" "go"
                                  "grep" "gr"
                                  "grepadd" "grepa"
                                  "hardcopy" "ha"
                                  "hide" "hid"
                                  "history" "his"
                                  "iabclear" "iabc"
                                  "if" "endif" "en"
                                  "ijump" "ij"
                                  "ilist" "il"
                                  "im" "imap"
                                  "imapc" "imapclear"
                                  "ime" "imenu" "inoreme" "inoremenu"
                                  "ino" "inoremap"
                                  "isearch" "is"
                                  "isplit" "isp"
                                  "iu" "iunmap"
                                  "iunabbrev" "iuna"
                                  "join" "j"
                                  "jumps" "ju"
                                  "k"
                                  "keepalt"
                                  "keepjumps" "keepj"
                                  "keepmarks" "kee"
                                  "lNext" "lN"
                                  "lNfile" "lNf"
                                  "laddbuffer" "laddb"
                                  "laddexpr" "lad"
                                  "laddfile" "laddf"
                                  "language" "lan"
                                  "last" "la"
                                  "later"
                                  "lbuffer" "lb"
                                  "lcd" "lc"
                                  "lchdir" "lch"
                                  "lclose" "lcl"
                                  "le" "left"
                                  "leftabove" "lefta"
                                  "let"
                                  "lexpr" "lex"
                                  "lfile" "lf"
                                  "lfirst" "lfir"
                                  "lgetbuffer" "lgetb"
                                  "lgetexpr" "lgete"
                                  "lgetfile" "lg"
                                  "lgrep" "lgr"
                                  "lgrepadd" "lgrepa"
                                  "list" "l"
                                  "ll"
                                  "llast" "lla"
                                  "llist" "lli"
                                  "lm" "lmap"
                                  "lmake" "lmak"
                                  "lmapc" "lmapclear"
                                  "ln" "lnoremap"
                                  "lnewer" "lnew"
                                  "lnext" "lne"
                                  "lnfile" "lnf"
                                  "loadview" "lo"
                                  "lockmarks" "loc"
                                  "lockvar" "lockv" "lockv"
                                  "lolder" "lol"
                                  "lopen" "lop"
                                  "lpfile" "lpf"
                                  "lprevious" "lp"
                                  "lrewind" "lr"
                                  "ltag" "lt"
                                  "lu" "lunmap"
                                  "lua" "lua"
                                  "luado" "luado"
                                  "luafile" "luafile"
                                  "lvimgrep" "lv"
                                  "lvimgrepadd" "lvimgrepa"
                                  "lwindow" "lw"
                                  "make" "mak"
                                  "map"
                                  "mapc" "mapclear"
                                  "mark" "ma"
                                  "marks"
                                  "match" "mat"
                                  "me" "menu" "noreme"  "noremenu"
                                  "menutranslate" "menut"
                                  "mkexrc" "mk"
                                  "mksession" "mks"
                                  "mkspell" "mksp"
                                  "mkview" "mkvie"
                                  "mkvimrc" "mkv"
                                  "mode" "mod"
                                  "move" "m"
                                  "mzfile" "mzf"
                                  "mzscheme" "mz"
                                  "nbclose" "nbc"
                                  "nbkey" "nb"
                                  "nbstart" "nbs"
                                  "new"
                                  "next" "n"
                                  "nm" "nmap"
                                  "nmapc" "nmapclear"
                                  "nme" "nmenu" "nnoreme" "nnoremenu"
                                  "nn" "nnoremap"
                                  "no" "noremap" "nor"
                                  "nohlsearch" "noh"
                                  "nore" "norem"
                                  "number" "nu"
                                  "nun" "nunmap"
                                  "oldfiles" "ol"
                                  "om" "omap"
                                  "omapc" "omapclear"
                                  "ome" "omenu" "onoreme" "onoremenu"
                                  "only" "on"
                                  "ono" "onoremap"
                                  "open" "o"
                                  "options" "opt"
                                  "ou" "ounmap"
                                  "pclose" "pc"
                                  "pedit" "ped"
                                  "perl" "pe"
                                  "perldo" "perld"
                                  "pop" "po"
                                  "popu"
                                  "popup" "popu"
                                  "ppop" "pp"
                                  "preserve" "pre"
                                  "previous" "prev"
                                  "print" "p"
                                  "profdel" "profd"
                                  "profile" "prof"
                                  "promptfind" "promptf"
                                  "promptrepl" "promptr"
                                  "psearch" "ps"
                                  "ptNext" "ptN"
                                  "ptag" "pta"
                                  "ptfirst" "ptf"
                                  "ptjump" "ptj"
                                  "ptlast" "ptl"
                                  "ptnext" "ptn"
                                  "ptprevious" "ptp"
                                  "ptrewind" "ptr"
                                  "ptselect" "pts"
                                  "put" "pu"
                                  "pwd" "pw"
                                  "pyfile" "pyf"
                                  "python" "py"
                                  "qall" "qa"
                                  "quit" "q"
                                  "quitall" "quita"
                                  "read" "r"
                                  "recover" "rec"
                                  "redir" "redi"
                                  "redo" "red"
                                  "redraw" "redr"
                                  "redrawstatus" "redraws"
                                  "registers" "reg"
                                  "resize" "res"
                                  "retab" "ret"
                                  "return" "rn"
                                  "rewind" "rew"
                                  "right" "ri"
                                  "rightbelow" "rightb"
                                  "ruby" "rub"
                                  "rubydo" "rubyd"
                                  "rubyfile" "rubyf"
                                  "rundo"
                                  "runtime" "ru"
                                  "rviminfo" "sa"
                                  "sNext" "sN"
                                  "sall" "sal"
                                  "sandbox" "san"
                                  "sargument" "sa"
                                  "saveas" "sav"
                                  "sbNext" "sbN"
                                  "sball" "sba"
                                  "sbfirst" "sbf"
                                  "sblast" "sbl"
                                  "sbmodified" "sbm"
                                  "sbnext" "sbn"
                                  "sbprevious" "sbp"
                                  "sbrewind" "sbr"
                                  "sbuffer" "sb"
                                  "scriptencoding" "scripte"
                                  "scriptnames" "scrip"
                                  "set" "se"
                                  "setfiletype" "setf"
                                  "setglobal" "setg"
                                  "setlocal" "setl"
                                  "sfind" "sf"
                                  "sfirst" "sfir"
                                  "shell" "sh"
                                  "sign"
                                  "silent" "sil"
                                  "simalt" "sim"
                                  "slast" "sla"
                                  "sleep" "sl"
                                  "smagic" "sm"
                                  "smap" "smap"
                                  "smapc" "smapclear"
                                  "sme" "smenu" "snoreme" "snoremenu"
                                  "snext" "sn"
                                  "sniff" "sni"
                                  "snomagic" "sno"
                                  "snor" "snoremap"
                                  "sort" "sor"
                                  "source" "so"
                                  "spelldump" "spelld"
                                  "spellgood" "spe"
                                  "spellinfo" "spelli"
                                  "spellrepall" "spellr"
                                  "spellundo" "spellu"
                                  "spellwrong" "spellw"
                                  "split" "sp"
                                  "sprevious" "spr"
                                  "srewind" "sre"
                                  "stag" "sta"
                                  "startgreplace" "startg"
                                  "startinsert" "star"
                                  "startreplace" "startr"
                                  "stjump" "stj"
                                  "stop" "st"
                                  "stopinsert" "stopi"
                                  "stselect" "sts"
                                  "sunhide" "sun"
                                  "sunm" "sunmap"
                                  "suspend" "sus"
                                  "sview" "sv"
                                  "syncbind"
                                  "t"
                                  "tNext" "tN"
                                  "tab"
                                  "tabNext" "tabN"
                                  "tabclose" "tabc"
                                  "tabdo" "tabd"
                                  "tabedit" "tabe"
                                  "tabfind" "tabf"
                                  "tabfirst" "tabfir"
                                  "tablast" "tabl"
                                  "tabmove" "tabm"
                                  "tabnew"
                                  "tabnext" "tabn"
                                  "tabonly" "tabo"
                                  "tabprevious" "tabp"
                                  "tabrewind" "tabr"
                                  "tabs"
                                  "tag" "ta"
                                  "tags"
                                  "tcl" "tc"
                                  "tcldo" "tcld"
                                  "tclfile" "tclf"
                                  "tearoff" "te"
                                  "tfirst" "tf"
                                  "throw" "th"
                                  "tjump" "tj"
                                  "tlast" "tl"
                                  "tmenu" "tm"
                                  "tnext" "tn"
                                  "tnor" "tnoremap"
                                  "topleft" "to"
                                  "tprevious" "tp"
                                  "trewind" "tr"
                                  "try" "endt" "endtry"
                                  "tselect" "ts"
                                  "tunmenu" "tu"
                                  "unabbreviate" "una"
                                  "undo" "una"
                                  "undojoin" "undoj"
                                  "undolist" "undol"
                                  "unhide" "unh"
                                  "unlockvar" "unlo"
                                  "unm" "unmap"
                                  "unsilent" "uns"
                                  "update" "up"
                                  "verbose" "verb"
                                  "version" "ve"
                                  "vertical" "vert"
                                  "view" "vie"
                                  "vimgrep" "vim"
                                  "vimgrepadd" "vimgrepa"
                                  "visual" "vi"
                                  "vm" "vmap"
                                  "vmapc" "vmapclear"
                                  "vme" "vmenu" "vnoreme" "vnoremenu"
                                  "vn" "vnoremap"
                                  "vnew" "vne"
                                  "vsplit" "vs"
                                  "vu" "vunmap"
                                  "wNext" "wN"
                                  "wall" "wa"
                                  "while" "endwhile" "wh" "endw"
                                  "wincmd" "winc"
                                  "windo"
                                  "winpos" "winp"
                                  "winsize" "win"
                                  "wnext" "wn"
                                  "wprevious" "wp"
                                  "wq"
                                  "wqa" "wqall" "xa" "xall"
                                  "write" "w"
                                  "wsverb" "ws"
                                  "wundo"
                                  "wviminfo" "wv"
                                  "x" "xit"
                                  "xm" "xmap"
                                  "xmapc" "xmapclear"
                                  "xme" "xmenu" "xnoreme" "xnoremenu"
                                  "xn" "xnoremap"
                                  "xu" "xunmap"
                                  "yank" "y"
                                  ) 'words)
                    "\\([^_]\\|$\\)")
           2 '(face vimrc-command))

          ;; Built-in functions
          (,(concat "\\(^\\|[ \t]*\\)"
                    (regexp-opt '("writefile"
                                  "winwidth"
                                  "winsaveview"
                                  "winrestview"
                                  "winrestcmd"
                                  "winnr"
                                  "winline"
                                  "winheight"
                                  "wincol"
                                  "winbufnr"
                                  "visualmode"
                                  "virtcol"
                                  "values"
                                  "undotree"
                                  "undofile"
                                  "type"
                                  "trunc"
                                  "tr"
                                  "toupper"
                                  "tolower"
                                  "tempname"
                                  "tanh"
                                  "tan"
                                  "taglist"
                                  "tagfiles"
                                  "tabpagewinnr"
                                  "tabpagenr"
                                  "tabpagebuflist"
                                  "system"
                                  "synstack"
                                  "synIDtrans"
                                  "synIDattr"
                                  "synID"
                                  "synconcealed"
                                  "substitute"
                                  "submatch"
                                  "strwidth"
                                  "strtrans"
                                  "strridx"
                                  "strpart"
                                  "strlen"
                                  "string"
                                  "stridx"
                                  "strftime"
                                  "strdisplaywidth"
                                  "strchars"
                                  "str2nr"
                                  "str2float"
                                  "sqrt"
                                  "split"
                                  "spellsuggest"
                                  "spellbadword"
                                  "soundfold"
                                  "sort"
                                  "sinh"
                                  "sin"
                                  "simplify"
                                  "shellescape"
                                  "setwinvar"
                                  "settabwinvar"
                                  "settabvar"
                                  "setreg"
                                  "setqflist"
                                  "setpos"
                                  "setmatches"
                                  "setloclist"
                                  "setline"
                                  "setcmdpos"
                                  "setbufvar"
                                  "serverlist"
                                  "server2client"
                                  "searchpos"
                                  "searchpairpos"
                                  "searchpair"
                                  "searchdecl"
                                  "search"
                                  "round"
                                  "reverse"
                                  "resolve"
                                  "repeat"
                                  "rename"
                                  "remove"
                                  "remote_send"
                                  "remote_read"
                                  "remote_peek"
                                  "remote_foreground"
                                  "remote_expr"
                                  "reltimestr"
                                  "reltime"
                                  "readfile"
                                  "range"
                                  "pumvisible"
                                  "printf"
                                  "prevnonblank"
                                  "pow"
                                  "pathshorten"
                                  "nr2char"
                                  "nextnonblank"
                                  "mzeval"
                                  "mode"
                                  "mkdir"
                                  "min"
                                  "max"
                                  "matchstr"
                                  "matchlist"
                                  "matchend"
                                  "matchdelete"
                                  "matcharg"
                                  "matchadd"
                                  "match"
                                  "mapcheck"
                                  "maparg"
                                  "map"
                                  "log10"
                                  "log"
                                  "localtime"
                                  "lispindent"
                                  "line2byte"
                                  "line"
                                  "libcallnr"
                                  "libcall"
                                  "len"
                                  "keys"
                                  "join"
                                  "items"
                                  "islocked"
                                  "isdirectory"
                                  "insert"
                                  "inputsecret"
                                  "inputsave"
                                  "inputrestore"
                                  "inputlist"
                                  "inputdialog"
                                  "input"
                                  "index"
                                  "indent"
                                  "iconv"
                                  "hostname"
                                  "hlID"
                                  "hlexists"
                                  "histnr"
                                  "histget"
                                  "histdel"
                                  "histadd"
                                  "hasmapto"
                                  "haslocaldir"
                                  "has_key"
                                  "has"
                                  "globpath"
                                  "glob"
                                  "getwinvar"
                                  "getwinposy"
                                  "getwinposx"
                                  "gettabwinvar"
                                  "gettabvar"
                                  "getregtype"
                                  "getreg"
                                  "getqflist"
                                  "getpos"
                                  "getpid"
                                  "getmatches"
                                  "getloclist"
                                  "getline"
                                  "getftype"
                                  "getftime"
                                  "getfsize"
                                  "getfperm"
                                  "getfontname"
                                  "getcwd"
                                  "getcmdtype"
                                  "getcmdpos"
                                  "getcmdline"
                                  "getcharmod"
                                  "getchar"
                                  "getbufvar"
                                  "getbufline"
                                  "get"
                                  "garbagecollect"
                                  "function"
                                  "foreground"
                                  "foldtextresult"
                                  "foldtext"
                                  "foldlevel"
                                  "foldclosedend"
                                  "foldclosed"
                                  "fnamemodify"
                                  "fnameescape"
                                  "fmod"
                                  "floor"
                                  "float2nr"
                                  "findfile"
                                  "finddir"
                                  "filter"
                                  "filewritable"
                                  "filereadable"
                                  "feedkeys"
                                  "extend"
                                  "expr8"
                                  "expand"
                                  "exp"
                                  "exists"
                                  "eventhandler"
                                  "eval"
                                  "escape"
                                  "empty"
                                  "diff_hlID"
                                  "diff_filler"
                                  "did_filetype"
                                  "delete"
                                  "deepcopy"
                                  "cursor"
                                  "cscope_connection"
                                  "count"
                                  "cosh"
                                  "cos"
                                  "copy"
                                  "contained"
                                  "confirm"
                                  "complete_check"
                                  "complete_add"
                                  "complete"
                                  "col"
                                  "clearmatches"
                                  "cindent"
                                  "char2nr"
                                  "changenr"
                                  "ceil"
                                  "call"
                                  "byteidx"
                                  "byte2line"
                                  "bufwinnr"
                                  "bufnr"
                                  "bufname"
                                  "bufloaded"
                                  "buflisted"
                                  "bufexists"
                                  "browsedir"
                                  "browse"
                                  "atan2"
                                  "atan"
                                  "asin"
                                  "argv"
                                  "argidx"
                                  "argc"
                                  "append"
                                  "add"
                                  "acos"
                                  "abs"
                                  ) 'words)
                    "\\([ \t]*(\\)")
           2 '(face vimrc-function-builtin))

          ;; Numbers
          ("\\<0[xX][[:xdigit:]]+"
           (0 '(face vimrc-number)))
          ("#[[:xdigit:]]\\{6\\}"
           (0 '(face vimrc-number)))
          (,(concat
             "\\(\\<\\|-\\)[[:digit:]]+"
             "\\(\\.[[:digit:]]+\\([eE][+-]?[[:digit:]]+\\)?\\)?")
           0 '(face vimrc-number))

          ;;
          ;; Operators start:
          (,(concat "\\("
                    ;; word char
                    "\\(\\<isnot\\>\\)"
                    "\\|" "\\(\\<is\\>\\)"

                    "\\|" "\\(![=~]?[#?]?\\)"
                    "\\|" "\\(>[#\\\\?=]?[#?]?\\)"
                    "\\|" "\\(<[#\\\\?=]?[#?]?\\)"
                    "\\|" "\\(\\+=?\\)"
                    "\\|" "\\(-=?\\)"
                    "\\|" "\\(=[=~]?[#?]?\\)"
                    "\\|" "\\(||\\)"
                    "\\|" "\\(&&\\)"

                    "\\|" "\\(\\.\\)"
                    "\\)"
                    )
           1 font-lock-constant-face) ;; Operators end;
          )))
