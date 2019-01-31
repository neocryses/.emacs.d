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
  "Utility macro to create functions to deal with coding system problems"
  (let* ((read-coding (car coding))
         (write-coding (cdr coding))
         (read-name (symbol-name read-coding))
         (write-name (symbol-name write-coding))
         (suffix "-\\(mac\\|dos\\|unix\\)")
         ;; (bind-name
         ;;  (if (eql 1 (length (delete-dups (mapcar (lambda (name)
         ;;                                            (funcall #'replace-regexp-in-string
         ;;                                                     suffix "" name)) (list read-name write-name))))
         ;;           )
         ;;      read-name
         ;;    (concat read-name "-" write-name)))
         (bind-name
          (if (eql 1 (length (delete-dups
                              (list read-name write-name))))
              read-name
            (concat read-name "-" write-name)))
         (fun-name (intern (format "let-%s-for-rw" bind-name)))
         (doc (format "Advice function to let bind coding system to %s for ORIG-FUN."
                      coding)))
    `(progn (defun ,fun-name (orig-fun &rest args)
              ,doc
              (let ((coding-system-for-read ',read-coding)
                    (coding-system-for-write ',write-coding))
                ;; (message (concat "defun was successful!" ,(symbol-name orig-fun) ,(symbol-name write-coding)))
                (apply orig-fun args)))
            (advice-add ,fun :around #',fun-name))))
(let-coding-for-rw 'projectile-files-via-ext-command (utf-8-dos . utf-8-unix))
;; (replace-regexp-in-string "-\\(mac\\|dos\\|unix\\)" "" "utf-8-dos")

;; (setq test-var-var (nthcdr 0 '(utf-8-dos . utf-8-unix)))
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

    ;; TODO: Create an advice to make the following line obsolete
    ;; (add-to-list 'process-coding-system-alist '("[cC][mM][dD][pP][rR][oO][xX][yY]" . (utf-8-dos . utf-8-unix)))

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
;; (defadvice call-interactively (after show-last-command activate)
;;   "Shows the interactive command that was just run in the message area."
;;   (unless (eq major-mode 'minibuffer-inactive-mode)
;;     (message "Ran %s" (ad-get-arg 0))))

;;; UI settings

;; Whether frames should be resized implicitly.
(setq-default frame-inhibit-implied-resize t)

;;;; Font settings

;;;;; Version 5

(defvar default-font-family nil
  "Font to be used for English characters")

(defvar ja-default-font-family nil
  "Font to be used for Japanese characters")

(defvar default-font-size nil
  "Default font size")

(defvar ja-default-font-rescale nil
  "Rescaling value for Japanese characters")

(cond (*is-win*
       (setq default-font-family "Myrica M")
       (setq default-font-size 10)
       (setq ja-default-font-family default-font-family)
       (setq ja-default-font-rescale 1.0))
      (*is-mac*
       (progn
         (setq default-font-family "Operator Mono SSm")
         (setq default-font-size 11)
         (setq ja-default-font-family "Ricty Discord")
         (setq ja-default-font-rescale 1.3))))

(defun set-font (&optional frame)
  (when frame
    (select-frame frame))
  (when (display-graphic-p)
    (let* ((font-family default-font-family)
           (font-size default-font-size)
           (font-height (* font-size 10))
           (ja-font-family ja-default-font-family))
      (set-face-attribute 'default nil :family font-family :height font-height)
      (let ((name (frame-parameter nil 'font))
            (font-spec (font-spec :family font-family))
            (characters '((?\u00A0 . ?\u00FF) ; Latin-1
                          (?\u0100 . ?\u017F) ; Latin Extended-A
                          (?\u0180 . ?\u024F) ; Latin Extended-B
                          (?\u0250 . ?\u02AF) ; IPA Extensions
                          (?\u0370 . ?\u03FF) ; Greek and Coptic
                          ?\u0183)) ;Whitespace-mode ·
            (ja-font-spec (font-spec :family ja-font-family))
            (ja-characters '(katakana-jisx0201
                             cp932-2-byte
                             japanese-jisx0212
                             japanese-jisx0213-2
                             japanese-jisx0213.2004-1))) 
        (dolist (ja-character ja-characters)
          (set-fontset-font name ja-character ja-font-spec))
        (dolist (character characters)
          (set-fontset-font name character font-spec))
        (add-to-list 'face-font-rescale-alist (cons ja-font-family ja-default-font-rescale))))))
(add-hook 'after-init-hook #'set-font)
(add-hook 'after-make-frame-functions #'set-font)

;; (defun set-font (&optional frame)
;;   (when frame
;;     (select-frame frame))
;;   (when (display-graphic-p)
;;     (let* ((font-family "Operator Mono SSm")
;;            (font-size 11)
;;            (font-height (* font-size 10))
;;            (jp-font-family "Ricty Discord"))
;;       (set-face-attribute 'default nil :family font-family :height font-height)
;;       (let ((name (frame-parameter nil 'font))
;;             (font-spec (font-spec :family font-family))
;;             (characters '((?\u00A0 . ?\u00FF) ; Latin-1
;;                           (?\u0100 . ?\u017F) ; Latin Extended-A
;;                           (?\u0180 . ?\u024F) ; Latin Extended-B
;;                           (?\u0250 . ?\u02AF) ; IPA Extensions
;;                           (?\u0370 . ?\u03FF))) ; Greek and Coptic
;;             (jp-font-spec (font-spec :family jp-font-family))
;;             (jp-characters '(katakana-jisx0201
;;                              cp932-2-byte
;;                              japanese-jisx0212
;;                              japanese-jisx0213-2
;;                              japanese-jisx0213.2004-1))) 
;;         (dolist (character characters)
;;           (set-fontset-font name character font-spec))
;;         (dolist (jp-character jp-characters)
;;           (set-fontset-font name jp-character jp-font-spec))
;;         (add-to-list 'face-font-rescale-alist (cons jp-font-family 1.3))))))
;; (add-hook 'after-init-hook #'set-font)
;; (add-hook 'after-make-frame-functions #'set-font)

;;;;; Version 4

;; (when (eq system-type 'darwin)
;;   (let* ((fontset-name "macos")
;;          (size 11)
;;          (asciifont "Operator Mono SSm")
;;          (jpfont "Ricty Discord")
;;          (font (format "%s-%d:weight=normal:slant=normal" asciifont size))
;;          (fontspec (font-spec :family asciifont))
;;          (jp-fontspec (font-spec :family jpfont))
;;          (fsn (create-fontset-from-ascii-font font nil fontset-name)))
;;     (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
;;     (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
;;     (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec) ; 半角カナ
;;     (set-fontset-font fsn '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
;;     (set-fontset-font fsn '(#x0370 . #x03FF) fontspec)) ; ギリシャ文字

;;   (when (eq system-type 'darwin)
;;     (add-to-list 'default-frame-alist '(font . "fontset-macos")))

;;   (dolist (elt '(
;;                  ("Ricty Discord" . 1.3)
;;                  ))
;;     (add-to-list 'face-font-rescale-alist elt))

;;   (set-face-font 'default "fontset-macos")) 

;;;;; Version 3

;; (defvar ef/font "Operator Mono SSm-11"
;;   "Font to be used for English characters")

;; (defvar ef/font-ja "Ricty Discord"
;;   "Font to be used for Japanese characters")

;; (defun ef/set-font (&optional frame)
;;   (when frame
;;     (select-frame frame))
;;   (set-face-attribute 'default nil :font ef/font)
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font) charset
;;                       (font-spec :family ef/font-ja :size 14))))
;; (ef/set-font)
;; (add-hook 'after-make-frame-functions #'ef/set-font)

;;;;; Version 2

;; Windows specific setting - here only temporarily
;; (set-face-attribute 'default nil :font "Operator Mono SSm-10")
;; (set-frame-font "Operator Mono SSm-10" nil t)

;; 半角英字設定
;; (set-face-attribute 'default nil :family "Myrica M" :height 90)

;; 全角かな設定
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'japanese-jisx0208
;;                   (font-spec :family "Ricty Discord" :size 12))

;; 半角ｶﾅ設定
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'katakana-jisx0201
;;                   (font-spec :family "Ricty Discord" :size 12))

;;;;; Version 1

;; Set the font for macOS
;; (when (eq system-type 'darwin)
;;   (add-to-list 'default-frame-alist '(font . "Operator Mono SSm-10")))

;; Set the font for Windows
;; (when (eq system-type 'windows-nt)
;;   (set-face-attribute 'default nil :font "Myrica M-10")
;;   (set-frame-font "Myrica M-10" nil t)) 

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

;;; Packages

;;;; Evil

(use-package evil
  :ensure t
  :demand
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-auto-balance-windows nil)
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode 1)
  (add-hook 'c-mode-common-hook (lambda () (modify-syntax-entry ?_ "w")))

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

;; This package needs evil-want-keybinding set to nil on evil init
(use-package evil-collection
  :after evil
  :ensure t
  :demand
  :config
  (evil-collection-init '(simple calc calendar comint custom ediff occur xref simple term)))

(use-package evil-goggles
  :ensure t
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
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-surround
  :after evil
  :ensure t
  :demand t
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
  :ensure t)

(use-package evil-visualstar
  :ensure t
  :defer t
  :config
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
  :ensure t
  :commands (rg))

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
        shackle-default-size 8
        shackle-rules '(("*compilation*" :size 0.25 :select nil)
                        ("*info*" :size 0.5 :select t)
                        ("*Backtrace*" :size 20 :select nil)
                        ("*Warnings*"  :size 12 :select nil :autofit t)
                        ("*Messages*"  :size 12 :select nil)
                        ("*Help*" :same t :inhibit-window-quit t :size 20 :select t)
                        (apropos-mode :size 0.3)))
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
    (dired (file-name-directory buffer-file-name)))

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

(use-package display-line-numbers
  :ensure nil
  :unless (version< emacs-version "26.0")
  :hook ((prog-mode . display-line-numbers-mode)
         (org-mode . (lambda () (setq display-line-numbers 'visual)))
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
  (add-to-list 'company-backends '(company-capf company-dabbrev))

  :general
  (:keymaps '(company-active-map)
   [tab] 'company-complete-common-or-cycle
   [backtab] (lambda () (interactive)(company-complete-common-or-cycle -1))
   "C-n" 'company-select-next
   "C-p" 'company-select-previous))

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
    (setq helm-ag-base-command "rg --vimgrep --no-heading --encoding ms932")
    ;; (setq helm-ag-base-command "rg --no-heading --encoding sjis")
    ;; (setq helm-ag-base-command
    ;;       (concat helm-ag-base-command
    ;;               (cond ((eq 'utf-8-dos buffer-file-coding-system)
    ;;                      " --encoding utf-8")
    ;;                     ((eq 'utf-8-unix buffer-file-coding-system)
    ;;                      " --encoding utf-8")
    ;;                     ((eq 'cp932-dos buffer-file-coding-system)
    ;;                      " --encoding cp932")
    ;;                     ((eq 'cp932-unix buffer-file-coding-system)
    ;;                      " --encoding cp932")))
    ;;       ))
    ))

(use-package helm-descbinds
  :ensure t
  :defer .3
  :commands helm-descbinds)

(use-package helm-swoop
  :ensure t
  :defer .3
  :general
  (:states '(normal visual)
   :prefix global-leader
   "/" 'helm-swoop))

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
    (defun win-to-unix-path (path)
      "Convert windows style path to unix style path"
      (subst-char-in-string ?\\ ?/ path))

    (defun win-to-unix-path-list (orig-fun &rest args)
      "Convert list of windows style path to list of unix style path"
      (mapcar 'win-to-unix-path
              (apply orig-fun args)))

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
  (when (member (system-name) work-computer-list)
    (setq require-final-newline nil)))

(use-package simple
  :ensure nil
  :defer t
  :config
  (column-number-mode 1))

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

(use-package calc
  :ensure nil
  ;; :defer t
  :commands (calc)
  ;; :config
  ;; (require 'calc-ext)

  ;; :general
  ;; (:states '(normal visual)
  ;;  "-" 'dired-buffer-file-parent)
  
  ;; (:keymaps '(calc-mode-map)
  ;;  :states '(normal visual)
  ;;  "0" 'calcDigit-start
  ;;  "1" 'calcDigit-start
  ;;  "2" 'calcDigit-start
  ;;  "3" 'calcDigit-start
  ;;  "4" 'calcDigit-start
  ;;  "5" 'calcDigit-start
  ;;  "6" 'calcDigit-start
  ;;  "7" 'calcDigit-start
  ;;  "8" 'calcDigit-start
  ;;  "9" 'calcDigit-start

  ;;  (kbd "<tab>") 'calc-roll-down
  ;;  (kbd "S-<return>") 'calc-over
  ;;  (kbd "<return>") 'calc-enter
  ;;  (kbd "SPC") 'calc-enter

  ;;  (kbd "C-x C-t") 'calc-transpose-lines
  ;;  (kbd "C-M-d") 'calc-pop-above
  ;;  (kbd "C-M-i") 'calc-roll-up
  ;;  (kbd "M-RET") 'calc-last-args
  ;;  (kbd "C-M-w") 'kill-ring-save
  ;;  (kbd "M-%") 'calc-percent
  ;;  (kbd "M-k") 'calc-copy-as-kill
  ;;  (kbd "M-w") 'calc-copy-region-as-kill
  ;;  (kbd "M-DEL") 'calc-pop-above
  ;;  (kbd "M-m t") 'calc-total-algebraic-mode
  ;;  (kbd "<delete>") 'calc-pop
  ;;  (kbd "<mouse-2>") 'calc-yank
  ;;  "x" 'calc-pop ; was "C-d".  TODO: Conflicts with calc-execute-extended-command.
  ;;  "d" 'calc-kill                       ; was "C-k"
  ;;  "u" 'calc-undo                       ; was "U"
  ;;  "X" 'calc-call-last-kbd-macro        ; "@" is already used.
  ;;  "pp" 'calc-yank                      ; was "C-y"
  ;;  "pP" 'calc-copy-to-buffer            ; was "y"

  ;;  (kbd "C-p") 'calc-precision          ; was "p"

  ;;  "?" 'calc-help
  ;;  ;; "h" 'calc-help-prefix ; TODO: Rebind?
  ;;  "i" 'calc-info

  ;;  "\"" 'calc-auto-algebraic-entry
  ;;  "$" 'calc-auto-algebraic-entry       ; TODO: No need for this one?
  ;;  "'" 'calc-algebraic-entry

  ;;  "!" 'calc-factorial
  ;;  "#" 'calcDigit-start
  ;;  "%" 'calc-mod
  ;;  "&" 'calc-inv
  ;;  "(" 'calc-begin-complex
  ;;  ")" 'calc-end-complex
  ;;  "*" 'calc-times
  ;;  "+" 'calc-plus
  ;;  "," 'calc-comma
  ;;  "-" 'calc-minus
  ;;  "." 'calcDigit-start
  ;;  "/" 'calc-divide
  ;;  ":" 'calc-fdiv
  ;;  ";" 'calc-semi          ; TODO: Shall we really override `evil-ex'?
  ;;  "<" 'calc-scroll-left
  ;;  "=" 'calc-evaluate
  ;;  ">" 'calc-scroll-right
  ;;  "@" 'calcDigit-start
  ;;  "A" 'calc-abs
  ;;  "B" 'calc-log
  ;;  "C" 'calc-cos
  ;;  ;; "D" 'calc-redo             ; TODO: What's the purpose of this?  Bind to C-r?
  ;;  "E" 'calc-exp
  ;;  "F" 'calc-floor
  ;;  "G" 'calc-argument
  ;;  "H" 'calc-hyperbolic
  ;;  "I" 'calc-inverse
  ;;  "J" 'calc-conj
  ;;  "K" 'calc-keep-args
  ;;  "L" 'calc-ln
  ;;  "M" 'calc-more-recursion-depth
  ;;  "N" 'calc-eval-num
  ;;  "O" 'calc-option
  ;;  "P" 'calc-pi
  ;;  "Q" 'calc-sqrt
  ;;  "R" 'calc-round
  ;;  "S" 'calc-sin
  ;;  "T" 'calc-tan
  ;;  "[" 'calc-begin-vector
  ;;  "]" 'calc-end-vector
  ;;  "\\" 'calc-idiv
  ;;  "^" 'calc-power
  ;;  "_" 'calcDigit-start
  ;;  "`" 'calc-edit
  ;;  "e" 'calcDigit-start
  ;;  "n" 'calc-change-sign
  ;;  "o" 'calc-realign
  ;;  "w" 'calc-why
  ;;  "x" 'calc-execute-extended-command ; TODO: Conflicts with calc-pop.
  ;;  "|" 'calc-concat
  ;;  "{" 'calc-scroll-down                ; TODO: Not necessary?
  ;;  "}" 'calc-scroll-up                  ; TODO: Not necessary?
  ;;  "~" 'calc-num-prefix

  ;;  ;; "V" (lookup-key calc-mode-map (kbd "V"))
  ;;  ;; "Y" (lookup-key calc-mode-map (kbd "Y"))
  ;;  ;; "Z" (lookup-key calc-mode-map (kbd "Z"))
  ;;  ;; "a" (lookup-key calc-mode-map (kbd "a"))
  ;;  ;; "b" (lookup-key calc-mode-map (kbd "b"))
  ;;  ;; "c" (lookup-key calc-mode-map (kbd "c"))
  ;;  ;; "D" (lookup-key calc-mode-map (kbd "d"))
  ;;  ;; "f" (lookup-key calc-mode-map (kbd "f"))
  ;;  ;; "g" (lookup-key calc-mode-map (kbd "g"))
  ;;  ;; "zj" (lookup-key calc-mode-map (kbd "j"))
  ;;  ;; "zk" (lookup-key calc-mode-map (kbd "k"))
  ;;  ;; "zl" (lookup-key calc-mode-map (kbd "l"))
  ;;  ;; "m" (lookup-key calc-mode-map (kbd "m"))
  ;;  ;; "r" (lookup-key calc-mode-map (kbd "r"))
  ;;  ;; "s" (lookup-key calc-mode-map (kbd "s"))
  ;;  ;; "t" (lookup-key calc-mode-map (kbd "t"))
  ;;  ;; "U" (lookup-key calc-mode-map (kbd "u"))
  ;;  ;; "v" (lookup-key calc-mode-map (kbd "v"))
  ;;  ;; "zz" (lookup-key calc-mode-map (kbd "z"))

  ;;  ;; "V" (lookup-key calc-mode-map (kbd "V"))
  ;;  ;; "Y" (lookup-key calc-mode-map (kbd "Y"))
  ;;  ;; "Z" (lookup-key calc-mode-map (kbd "Z"))
  ;;  ;; "a" (lookup-key calc-mode-map (kbd "a"))
  ;;  ;; "b" (lookup-key calc-mode-map (kbd "b"))
  ;;  ;; "c" (lookup-key calc-mode-map (kbd "c"))
  ;;  ;; "D" (lookup-key calc-mode-map (kbd "d"))
  ;;  ;; "f" (lookup-key calc-mode-map (kbd "f"))
  ;;  ;; "g" (lookup-key calc-mode-map (kbd "g"))
  ;;  ;; "zj" (lookup-key calc-mode-map (kbd "j"))
  ;;  ;; "zk" (lookup-key calc-mode-map (kbd "k"))
  ;;  ;; "zl" (lookup-key calc-mode-map (kbd "l"))
  ;;  ;; "m" (lookup-key calc-mode-map (kbd "m"))
  ;;  ;; "r" (lookup-key calc-mode-map (kbd "r"))
  ;;  ;; "s" (lookup-key calc-mode-map (kbd "s"))
  ;;  ;; "t" (lookup-key calc-mode-map (kbd "t"))
  ;;  ;; "U" (lookup-key calc-mode-map (kbd "u"))
  ;;  ;; "v" (lookup-key calc-mode-map (kbd "v"))
  ;;  ;; "zz" (lookup-key calc-mode-map (kbd "z"))

  ;;  ;; quit
  ;;  ;; "ZQ" 'quit-window ; TODO: Rebind "Z"?
  ;;  ;; "ZZ" 'quit-window ; TODO: Rebind "Z"?
  ;;  "q" 'calc-quit)

  ;; (:keymaps '(calc-mode-map)
  ;;  :states '(visual)
  ;;  "d" 'calc-kill-region)
  )

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
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

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
  :hook (emacs-lisp-mode . lispy-mode)
  :config
  (lispy-set-key-theme '(paredit c-digits)))

(use-package lispyville
  :ensure t
  :delight lispyville-mode
  :hook (lispy-mode . lispyville-mode))

;;;; Org

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
        org-hide-leading-stars t)
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

;;;; SQL

(use-package sql-indent
  :disabled
  :ensure t
  :defer t)

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
