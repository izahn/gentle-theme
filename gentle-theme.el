;;; gentle-theme.el --- A gentle, quiet, and reserved high-contrast color theme 

;; Copyright (C) 2018 Ista Zahn

;; Author: Ista Zahn

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A gentle and simple color theme. Nothing fancy, just tones things down
;; so font-lock doesn't distract you.

;; Inspired by:
;; URL: http://github.com/maio/eink-emacs

;;; Code:


(deftheme gentle
  "Subtle, mostly monochrome, and always gentle.")

(setq markdown-header-scaling t)

(let ((darker "black")
      (dark "gray10")
      (darkish "gray20")
      (grayish "gray40")
      (lightish "gray90")
      (light "gray98")
      (lighter "white")
      (blueish "#000053")
      (greenish "#00320a")
      (purpleish "#320046"))

  (custom-theme-set-faces
   'gentle
   `(default ((t (:background ,light :foreground ,darker))))
   `(cursor ((t (:background "black" :foreground "black" :weight bold))))
   `(region ((t (:background ,lightish :distant-foreground ,lighter))))
   `(shadow ((t (:foreground ,grayish :weight bold))))
   `(fringe ((t (:foreground ,grayish :background ,lightish))))
   `(header-line ((t :background ,grayish :foreground ,lighter)))
   `(linum ((t (:inherit shadow :background ,lightish))))
   `(mode-line ((t (:foreground ,lightish :background ,darkish
                         :box (:line-width -1 :color ,darkish)))))
   `(mode-line-inactive ((t (:inherit mode-line :background ,lightish :foreground ,grayish :weight light))))
   `(isearch ((t (:foreground "white" :background ,grayish :weight bold))))
   `(lazy-highlight ((t (:foreground "white" :background ,grayish) :weight normal)))
   `(message-separator ((t (:foreground ,greenish))))
   `(message-header-name ((t (:foreground ,purpleish))))
   `(diredfl-compressed-file-name ((t (:foreground ,darkish))))
   `(diredfl-compressed-file-suffix ((t (:foreground ,dark))))
   `(diredfl-date-time ((t (:foreground ,darkish))))
   `(diredfl-deletion ((t (:inherit warning))))
   `(diredfl-dir-heading ((t (:background ,light :foreground ,darker))))
   `(diredfl-dir-name ((t (:background ,light :foreground ,purpleish :weight bold))))
   `(diredfl-symlink ((t (:background ,light :foreground ,greenish))))
   `(diredfl-exec-priv ((t (:background ,lighter :foreground ,grayish))))
   `(diredfl-executable-tag ((t (:foreground ,darker))))
   `(diredfl-file-name ((t (:foreground ,dark :weight bold))))
   `(diredfl-file-suffix ((t (:foreground ,dark))))
   `(diredfl-flag-mark ((t (:background ,lighter :foreground ,darkish))))
   `(diredfl-link-priv ((t (:foreground ,darkish))))
   `(diredfl-no-priv ((t (:background ,light :foreground ,grayish))))
   `(diredfl-number ((t (:foreground ,darkish))))
   `(diredfl-other-priv ((t (:background ,light :foreground ,dark))))
   `(diredfl-rare-priv ((t (:background ,light :foreground ,darkish))))
   `(diredfl-read-priv ((t (:background ,light :foreground ,grayish))))
   `(diredfl-write-priv ((t (:background ,light :foreground ,grayish))))
   `(diredfl-ignored-file-name ((t (:background ,light :foreground ,grayish))))
   `(diredfl-dir-priv ((t (:background ,light :foreground ,grayish))))
   `(font-lock-type-face ((t (:foreground "gray10"))))
   `(font-lock-keyword-face ((t (:foreground ,blueish :background nil :weight normal))))
   `(font-lock-constant-face ((t (:background nil :foreground ,darkish))))
   `(trailing-whitespace ((t (:background "gray"))))
   `(font-lock-comment-face ((t (:foreground ,grayish :weight extra-bold :background nil))))
   `(font-lock-doc-face ((t (:foreground ,darkish :background nil :weight extra-bold))))
   `(font-lock-string-face ((t (:slant italic :background nil :foreground ,dark))))
   `(font-lock-comment-delimiter-face ((t (:background nil))))
   `(font-lock-type-face ((t (:background nil))))
   `(font-lock-builtin-face ((t (:foreground ,purpleish :background nil))))
   `(font-lock-variable-name-face ((t (:background nil :foreground ,darker))))
   `(font-lock-function-name-face ((t (:foreground ,greenish :background nil))))
   `(font-lock-warning-face ((t (:inherit warning :background nil))))
   `(term ((t (:background ,light :foreground ,darker :distant-foreground ,lightish))))
   `(term-color-white ((t (:foreground "gray20" :background "gray99" :weight bold))))
   `(term-color-black ((t (:foreground ,darker :background ,light))))
   `(term-default-fg-color ((t (:inherit term-color-white))))
   `(term-default-bg-color ((t (:inherit term-color-black))))
   `(company-tooltip ((t (:background ,lightish :foreground ,darker))))
   `(company-scrollbar-fg ((t (:background ,darker))))
   `(company-scrollbar-bg ((t (:background ,grayish))))
   `(company-tooltip-selection ((t ( :foreground ,lightish :background ,grayish :weight bold))))
   `(company-echo-common ((t ( :foreground ,darkish :background ,lightish))))
   `(company-tooltip-common ((t ( :foreground ,darker :background nil :weight bold))))
   `(ivy-current-match ((t ( :foreground ,lightish :background ,grayish :weight bold))))
   `(ivy-minibuffer-match-face-1 ((t ( :foreground ,darkish :background ,lightish))))
   `(ivy-minibuffer-match-face-2 ((t ( :foreground ,darkish :background ,lightish :weight bold))))
   `(ivy-minibuffer-match-face-3 ((t ( :foreground ,darkish :background ,lightish))))
   `(ivy-minibuffer-match-face-4 ((t ( :foreground ,dark :background ,light :weight bold))))
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-italic-face ((t (:foreground ,dark :slant italic))))
   `(highlight ((t (:background ,lightish))))
   `(show-paren-match ((t (:background ,grayish :foreground ,darker :weight bold))))
   `(outline-1 ((t (:foreground ,darker :weight bold ))))
   `(outline-2 ((t (:foreground ,dark :weight bold ))))
   `(outline-3 ((t (:foreground ,darkish :weight bold))))
   `(outline-4 ((t (:foreground ,darker :weight normal))))
   `(font-latex-sectioning-5-face ((t (:inherit outline-1))))
   `(font-latex-italic-face ((t (:foreground "black" :slant italic))))
   `(font-latex-string-face ((t (:inherit font-lock-string-face))))
   `(font-latex-sedate-face ((t (:inherit font-lock-keyword-face :weight medium))))
   `(org-block-begin-line ((t (:foreground "gray80"))))
   `(org-block-end-line ((t (:foreground "gray80"))))
   `(markdown-header-face ((t (:inherit outline-1))))
   `(mu4e-header-highlight-face ((t (:inherit highlight))))
   `(mu4e-region-code ((t (:background ,grayish :foreground ,lightish))))
   `(mu4e-header-title-face ((t (:inherit nil :foreground ,dark :background ,light :weight bold))))))

(provide-theme 'gentle)
