

# emacs.d


# Table of Contents

1.  [emacs.d](#org378550f)
    1.  [About](#org6017c95)
        1.  [Installation](#org0e3632e)
        2.  [Naming Conventions](#org2ab8dd1)
    2.  [Meta](#org5d744ea)
        1.  [Variables](#org03ca492)
    3.  [Package Management](#org4be2788)
        1.  [Setup package sources](#org88cdcc2)
        2.  [use-package](#org3ecbad0)
        3.  [straight.el](#org86214cd)
    4.  [Better Defaults](#orgf862a7f)
    5.  [UI](#org58a0eb9)
        1.  [Fonts](#org80b32a3)
        2.  [Frame](#org0f38925)
    6.  [Editing](#orgcf12774)
        1.  [Evil mode](#org36746fd)
        2.  [Brackets](#org658179b)
        3.  [line highlights and pulse](#org3ace2cd)
        4.  [Movement](#org02cdefd)
        5.  [Completions](#org41c87a9)
    7.  [Languages](#org7bc7d26)
        1.  [Company](#org8ed37a6)
        2.  [flycheck](#orga28dc1e)
        3.  [LSP](#org7808f0a)
        4.  [Web](#org4a6affc)
        5.  [JS/TS](#orgbccaa02)
        6.  [React](#org21c47a3)
        7.  [Vue.js](#orgb6a3b14)
        8.  [Go](#orgd46f770)
        9.  [Java](#org26c329a)
        10. [Clojure](#org2ec450f)
        11. [Scala](#orgfdc2175)
        12. [Zig](#orgbe63034)
    8.  [Other](#org607c917)
        1.  [Org](#org9d4e875)
        2.  [Zen](#org5634030)


<a id="org6017c95"></a>

## About


<a id="org0e3632e"></a>

### Installation

1.  Emacs & Dependencies

    On Mac OS, first install required dependencies.
    
        brew install ripgrep coreutils fd
        # Installs clang
        xcode-select --install
    
    Install [railwaycat's emacs](https://github.com/railwaycat/homebrew-emacsmacport)
    
        brew tap railwaycat/emacsmacport
        brew install emacs-mac --with-modules --with-emacs-big-sur-icon
        ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications/Emacs.app


<a id="org2ab8dd1"></a>

### Naming Conventions

Loosely based on bbatsov's [The Emacs Lisp Style Guide](https://github.com/bbatsov/emacs-lisp-style-guide) and doom emacs's [Naming conventions](https://github.com/hlissner/doom-emacs/blob/5b3f52f5fb98cc3af653b043d809254cebe04e6a/docs/contributing.org#naming-conventions)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Type</th>
<th scope="col" class="org-left">Convention</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Regular Public fn</td>
<td class="org-left">ab-{name}</td>
</tr>


<tr>
<td class="org-left">Interactive fn</td>
<td class="org-left">ab/{name}</td>
</tr>


<tr>
<td class="org-left">Internal fn</td>
<td class="org-left">ab&#x2013;{name}</td>
</tr>


<tr>
<td class="org-left">hooks</td>
<td class="org-left">ab-[-]{name}-h</td>
</tr>


<tr>
<td class="org-left">predicates</td>
<td class="org-left">ab-[-]{name}-p</td>
</tr>


<tr>
<td class="org-left">advice</td>
<td class="org-left">ab-[-]{name}-a</td>
</tr>
</tbody>
</table>


<a id="org5d744ea"></a>

## Meta

Enable lexical scoping

    ;;; -*- lexical-binding: t -*-


<a id="org03ca492"></a>

### Variables

User details

    (setq user-full-name "Abhilash Meesala"
          user-mail-address "mail@abhilashm.me")

Predicates to determine running context.

    (defvar is-mac (eq system-type 'darwin)
      "Is this environment a mac?")
    
    (defvar is-gui (display-graphic-p)
      "Is emacs running in gui mode?")
    
    (defvar is-term (not is-gui)
      "Is emacs running in a terminal?")


<a id="org4be2788"></a>

## Package Management


<a id="org88cdcc2"></a>

### Setup package sources

      (require 'package)
    
    (let ((versioned-package-dir
           (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
    			 user-emacs-directory)))
      (setq package-user-dir versioned-package-dir))
    
      ;; gnupg does not honor the `package-user-dir` 
      (when (boundp 'package-gnupghome-dir)
        (setq package-gnupghome-dir
    	  (expand-file-name "gnupg" package-user-dir)))
    
      (add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
    
      (unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
        (setq package-enable-at-startup nil)          ; To prevent initializing twice
        (package-initialize))


<a id="org3ecbad0"></a>

### use-package

    ;; Setup `use-package'
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    
    ;; Should set before loading `use-package'
    (eval-and-compile
      (setq use-package-always-ensure t)
      (setq use-package-expand-minimally t)
      (setq use-package-enable-imenu-support t))
    
    (require 'use-package)
    
    ;; Required by `use-package'
    (use-package diminish)
    (use-package bind-key)
    
    ;; Update GPG keyring for GNU ELPA
    (use-package gnu-elpa-keyring-update)


<a id="org86214cd"></a>

### TODO straight.el


<a id="orgf862a7f"></a>

## Better Defaults

UTF-8 everywhere

    ;; utf-8 everywhere
    (when (fboundp 'set-charset-priority)
      (set-charset-priority 'unicode))
    (prefer-coding-system 'utf-8)
    (setq locale-coding-system 'utf-8)
    (set-language-environment 'utf-8)
    (set-default-coding-systems 'utf-8)

Reduce noise on start up

    (setq inhibit-startup-message t
          inhibit-startup-echo-area-message user-login-name
          inhibit-default-init t
          initial-major-mode 'fundamental-mode
          initial-scratch-message nil)

When we visit a previously visited file, place point at last location.

    (setq save-place-file (expand-file-name ".places" user-emacs-directory))
    (setq save-place-forget-unreadable-files nil)
    (save-place-mode 1)

    ;; Don't write lock-files, I'm the only one here
    (setq create-lockfiles nil)
    
    ;; y/n instead of yes/no 
    (defalias 'yes-or-no-p 'y-or-n-p)
    
    ;; always highlight current line
    (global-hl-line-mode t)

Move custom preferences to another file.

    (setq custom-file (expand-file-name "custom.el" user-emacs-directory))


<a id="org58a0eb9"></a>

## UI


<a id="org80b32a3"></a>

### Fonts

    (set-frame-font "Fantasque Sans Mono-17" nil t)
    (setq-default line-spacing 0.24)


<a id="org0f38925"></a>

### Frame

Display file paths in frame titles

    (setq frame-title-format
           '((:eval (if (buffer-file-name)
    		    (abbreviate-file-name (buffer-file-name))
    		  "%b")))
           icon-title-format frame-title-format)

    ;; Make titlebar on Mac look better 
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . light))
    ;; beautiful fonts on Mac
    (setq mac-allow-anti-aliasing t)
    (setq ns-use-thin-smoothing t)
    
    ;; no need for bidirectional rendering
    (setq-default bidi-display-reordering 'left-to-right
    	      bidi-paragraph-direction 'left-to-right)


<a id="orgcf12774"></a>

## Editing


<a id="org36746fd"></a>

### Evil mode

    (use-package evil
      :init
      (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
      (setq evil-want-keybinding nil)
      :config
      (evil-mode 1))
    
    (use-package evil-collection
      :after evil
      :config
      (evil-collection-init))

    

    (setq mac-option-modifier 'meta
          mac-command-modifier 'super)
    (global-set-key [(super a)] #'mark-whole-buffer)
    (global-set-key [(super v)] #'yank)
    (global-set-key [(super c)] #'kill-ring-save)
    (global-set-key [(super s)] #'save-buffer)
    (global-set-key [(super l)] #'goto-line)
    (global-set-key [(super w)] #'delete-frame)
    (global-set-key [(super z)] #'undo)

    ;; Open init.el always
    ;; FIXME: Change this once the config is in a decent shape
    (setq initial-buffer-choice
          "~/.emacs.d/config.org")


<a id="org658179b"></a>

### TODO Brackets


<a id="org3ace2cd"></a>

### TODO line highlights and pulse


<a id="org02cdefd"></a>

### Movement

Setup Avy

> avy is a GNU Emacs package for jumping to visible text using a char-based decision tree. See also ace-jump-mode and vim-easymotion - avy uses the same idea.

    (use-package avy
      :custom
      (avy-timeout-seconds 0.2))


<a id="org41c87a9"></a>

### Completions

Install a generic completion framework like Ivy (,and Counsel and Swiper).

    (use-package ivy
      :config
      (ivy-mode 1))
    (use-package swiper)
    (use-package counsel
      :config
      (counsel-mode 1))


<a id="org7bc7d26"></a>

## Languages


<a id="org8ed37a6"></a>

### TODO Company


<a id="orga28dc1e"></a>

### TODO flycheck


<a id="org7808f0a"></a>

### TODO LSP


<a id="org4a6affc"></a>

### TODO Web


<a id="orgbccaa02"></a>

### TODO JS/TS


<a id="org21c47a3"></a>

### TODO React


<a id="orgb6a3b14"></a>

### TODO Vue.js


<a id="orgd46f770"></a>

### TODO Go


<a id="org26c329a"></a>

### TODO Java


<a id="org2ec450f"></a>

### TODO Clojure


<a id="orgfdc2175"></a>

### TODO Scala


<a id="orgbe63034"></a>

### TODO Zig


<a id="org607c917"></a>

## Other


<a id="org9d4e875"></a>

### TODO Org


<a id="org5634030"></a>

### TODO Zen

