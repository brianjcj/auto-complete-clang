;;; auto-complete-clang.el --- Auto Completion source for clang for GNU Emacs

;; Copyright (C) 2010  Brian Jiang

;; Author: Brian Jiang <brianjcj@gmail.com>
;; Keywords: completion, convenience
;; Version: 0.1b

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
;;
;; Auto Completion source for clang. Most of codes are taken from
;; company-clang.el and modified and enhanced for Auto Completion.

;;; Code:


(provide 'auto-complete-clang)
(require 'auto-complete)


(defcustom ac-clang-executable
  (executable-find "clang")
  "*Location of clang executable"
  :group 'auto-complete
  :type 'file)

(defcustom ac-clang-auto-save t
  "*Determines whether to save the buffer when retrieving completions.
clang can only complete correctly when the buffer has been saved."
  :group 'auto-complete
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))


;;; Extra compilation flags to pass to clang.
(defcustom ac-clang-flags nil
  "Extra flags to pass to the Clang executable.
This variable will typically contain include paths, e.g., ( \"-I~/MyProject\", \"-I.\" )."
    :group 'auto-complete
  :type '(repeat (string :tag "Argument" "")))

;;; The prefix header to use with Clang code completion. 
(defvar ac-clang-prefix-header nil)

;;; Set the Clang prefix header
(defun ac-clang-set-prefix-header (ph)
  (interactive
   (let ((def (car (directory-files "." t "\\([^.]h\\|[^h]\\).pch\\'" t))))
     (list
      (read-file-name (concat "Clang prefix header(current: " ac-clang-prefix-header ") : ")
                      (when def (file-name-directory def))
                      def nil (when def (file-name-nondirectory def))))))
  (cond ((string-match "^[\s\t]*$" ph)
         (setq ac-clang-prefix-header nil))
        (t
         (setq ac-clang-prefix-header ph))))

(defconst ac-clang-completion-pattern
  "^COMPLETION: \\(%s[^\s\n:]*\\)\\(?: : \\)*\\(.*$\\)")

(defconst ac-clang-error-buffer-name "*clang error*")

(defun ac-clang-parse-output (prefix)
  (goto-char (point-min))
  (let ((pattern (format ac-clang-completion-pattern
                         (regexp-quote prefix)))
        lines match detailed_info
        (prev-match ""))
    (while (re-search-forward pattern nil t)
      (setq match (match-string-no-properties 1))
      (unless (string= "Pattern" match)
        (setq detailed_info (match-string-no-properties 2))
      
        (if (string= match prev-match)
            (progn
              (when detailed_info
                (setq match (propertize match
                                        'ac-clang-help
                                        (concat
                                         (get-text-property 0 'ac-clang-help (car lines))
                                         "\n"
                                         detailed_info)))
                (setf (car lines) match)
                ))
          (setq prev-match match)
          (when detailed_info
            (setq match (propertize match 'ac-clang-help detailed_info)))
          (push match lines))))
    
    ;; (goto-char (point-min))
    ;; (while (re-search-forward "^OVERLOAD: " nil t)
    ;;   (message (buffer-substring-no-properties (line-beginning-position)
    ;;                                            (line-end-position))))
    
    ;; (unless (cdr lines)
    ;;   (when (car lines)
    ;;     ;; (message (ac-clang-clean-document (get-text-property 0 'ac-clang-help (car lines))))
    ;;     (with-output-to-temp-buffer
    ;;       "*clang*"
    ;;       (princ (ac-clang-clean-document (get-text-property 0 'ac-clang-help (car lines)))))
    ;;     (fit-window-to-buffer (get-buffer-window "*clang*") (/ (frame-height) 2) 8)))
    
    lines))


(defun ac-clang-handle-error (res args)
  (goto-char (point-min))
  (let* ((buf (get-buffer-create ac-clang-error-buffer-name))
         (cmd (concat ac-clang-executable " " (mapconcat 'identity args " ")))
         (pattern (format ac-clang-completion-pattern ""))
         (err (if (re-search-forward pattern nil t)
                  (buffer-substring-no-properties (point-min)
                                                  (1- (match-beginning 0)))
                ;; Warn the user more agressively if no match was found.
                (message "clang failed with error %d:\n%s" res cmd)
                (buffer-string))))

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (current-time-string)
                (format "\nclang failed with error %d:\n" res)
                cmd "\n\n")
        (insert err)
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun ac-clang-call-process (prefix &rest args)
  (with-temp-buffer
    (let ((res (apply 'call-process ac-clang-executable nil t nil args)))
      (unless (eq 0 res)
        (ac-clang-handle-error res args))
      ;; Still try to get any useful input.
      (ac-clang-parse-output prefix))))


(defsubst ac-clang-build-location (pos)
  (save-excursion
    (goto-char pos)
    (format "%s:%d:%d" buffer-file-name (line-number-at-pos)
            (1+ (current-column)))))

(defsubst ac-clang-build-complete-args (pos)
  (append '("-cc1" "-fsyntax-only")
          ac-clang-flags
          (when (stringp ac-clang-prefix-header)
            (list "-include-pch" (expand-file-name ac-clang-prefix-header)))
          '("-code-completion-at")
          (list (ac-clang-build-location pos))
          (list buffer-file-name)))


(defsubst ac-clang-clean-document (s)
  (when s
    (setq s (replace-regexp-in-string "<#\\|#>\\|\\[#" "" s))
    (setq s (replace-regexp-in-string "#\\]" " " s)))
  s)

(defun ac-clang-document (item)
  (if (stringp item)
      (let (s)
        (setq s (get-text-property 0 'ac-clang-help item))
        (ac-clang-clean-document s)))
  ;; (popup-item-property item 'ac-clang-help)
  )


(defface ac-clang-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for clang candidate"
  :group 'auto-complete)

(defface ac-clang-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the clang selected candidate."
  :group 'auto-complete)

(defun ac-clang-candidate ()
  (and ac-clang-auto-save
       (buffer-modified-p)
       (basic-save-buffer))
  (apply 'ac-clang-call-process
         ac-prefix
         (ac-clang-build-complete-args (- (point) (length ac-prefix)))))


(defvar ac-template-start-point nil)
(defvar ac-template-candidates (list "ok" "no" "yes:)"))

(defun ac-clang-action ()
  (interactive)
  ;; (ac-last-quick-help)
  (let ((help (ac-clang-clean-document (get-text-property 0 'ac-clang-help (cdr ac-last-completion))))
        (candidates (list)) ss ret-fn args idx)
    (setq ss (split-string help "\n"))
    (dolist (s ss)
      (when (string-match "^\\(.*\\)\\((.*)\\).*$" s)
        (setq ret-fn (match-string 1 s)
              args (match-string 2 s))
        (push (propertize args 'ac-clang-help ret-fn) candidates)
        (when (setq idx (string-match "\{#" args))
          (push (propertize (concat (substring args 0 idx) ")") 'ac-clang-help ret-fn) candidates))
        (when (setq idx (string-match ", \\.\\.\\." args))
          (push (propertize (concat (substring args 0 idx) ")") 'ac-clang-help ret-fn) candidates))))
    (when candidates
      (setq candidates (delete-dups candidates))
      (setq candidates (nreverse candidates))
      (setq ac-template-candidates candidates)
      (setq ac-template-start-point (point))
      (ac-complete-template)

      (unless (cdr candidates) ;; unless length > 1
        (message (replace-regexp-in-string "\n" "   ;    " help))))))

(defun ac-clang-prefix ()
  (or (ac-prefix-symbol)
      (let ((c (char-before)))
        (when (or (eq ?\. c)
                  ;; ->
                  (and (eq ?> c)
                       (eq ?- (char-before (1- (point)))))
                  ;; ::
                  (and (eq ?: c)
                       (eq ?: (char-before (1- (point))))))
          (point)))))

(ac-define-source clang
  '((candidates . ac-clang-candidate)
    (candidate-face . ac-clang-candidate-face)
    (selection-face . ac-clang-selection-face)
    (prefix . ac-clang-prefix)
    (requires . 0)
    (document . ac-clang-document)
    (action . ac-clang-action)
    (cache)
    (symbol . "c")))

(defun ac-clang-same-count-in-string (c1 c2 s)
  (let ((count 0) (cur 0) (end (length s)) c)
    (while (< cur end)
      (setq c (aref s cur))
      (cond ((eq c1 c)
             (setq count (1+ count)))
            ((eq c2 c)
             (setq count (1- count))))
      (setq cur (1+ cur)))
    (= count 0)))

(defun ac-clang-split-args (s)
  (let ((sl (split-string s ", *")))
    (cond ((string-match "<" s)
           (let ((res (list)) (pre "") subs)
             (while sl
               (setq subs (pop sl))
               (unless (string= pre "")
                 (setq subs (concat pre ", " subs))
                 (setq pre ""))
               (cond ((ac-clang-same-count-in-string ?\< ?\> subs)
                      (push subs res))
                     (t
                      (setq pre subs))))
             (nreverse res)))
          (t
           sl))))


(defun ac-template-candidate ()
  ac-template-candidates)

(defun ac-template-action ()
  (interactive)
  (unless (null ac-template-start-point)
    (let ((pos (point)) s sl (snp ""))
      (setq s (buffer-substring-no-properties ac-template-start-point pos))
      (setq s (replace-regexp-in-string "[()]" "" s))
      (unless (string= s "")
        (setq s (replace-regexp-in-string "{#" "\\\\{" s))
        (setq s (replace-regexp-in-string "#}" "" s))
        (setq sl (ac-clang-split-args s))
        ;; todo: take care undo-list
        (cond ((featurep 'yasnippet)
               (dolist (arg sl)
                 (setq snp (concat snp ", ${" arg "}")))
               (condition-case nil
                   (yas/expand-snippet (concat "("  (substring snp 2) ")") ac-template-start-point pos) ;; 0.6.1c
                 (error
                  ;; try this one:
                  (ignore-errors (yas/expand-snippet ac-template-start-point pos (concat "("  (substring snp 2) ")"))) ;; work in 0.5.7
                  )))
              ((featurep 'snippet)
               (delete-region ac-template-start-point pos)
               (dolist (arg sl)
                 (setq snp (concat snp ", $${" arg "}")))
               (snippet-insert (concat "("  (substring snp 2) ")")))
              (t
               (message "Dude! You are too out! Please install a yasnippet or a snippet script:)")))))))


(defun ac-template-prefix ()
  ac-template-start-point)


;; this source shall only be used internally.
(ac-define-source template
  '((candidates . ac-template-candidate)
    (prefix . ac-template-prefix)
    (requires . 0)
    (action . ac-template-action)
    (document . ac-clang-document)
    (cache)
    (symbol . "t")))

