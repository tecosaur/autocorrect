;;; autocorrect.el --- Spellchecker agnostic autocorrection -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 TEC
;;
;; Author: TEC <contact@tecosaur.net>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: March 27, 2024
;; Modified: March 27, 2024
;; Version: 0.1.0
;; Keywords: convenience, text
;; Homepage: https://code.tecosaur.net/tec/autocorrect.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Spellchecker agnostic autocorrect in Emacs
;;
;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'abbrev)

(defgroup autocorrect nil
  "Automatically fix typos and frequent spelling mistakes."
  :group 'text
  :prefix "autocorrect-")

;;;; Customisation variables:

;; To record corrections made, we can just use a simple text file.
;;
;; For simplicity of operation, I think we can just append each correction the
;; file as "misspelled corrected" lines. This has a number of advantages, such
;; as avoiding recalculations while typing, avoiding race conditions with
;; multiple Emacs sessions, and making merging data on different machines
;; trivial.
;;
;; In the Emacs session though, I think we'll want to have a hash table of the
;; counts of each correction. We can have the misspelled words as the keys, and
;; then have each value be an alist of ~(correction . count)~ pairs. This table
;; can be lazily built and processed after startup.

(defcustom autocorrect-file
  (file-name-concat (or (getenv "XDG_STATE_HOME") "~/.local/state")
                    "emacs" "autocorrections.txt")
  "File where a record of spell checks and autocorrections is saved.
Each line of this file is of the form:

    MISSPELLING [MANUAL-CORRECTION-COUNT AUTOCORRECTION-COUNT] CORRECTED

Where MANUAL-CORRECTION-COUNT and AUTOCORRECTION-COUNT are
optional (indicated by the brackets)."
  :type 'file)

;; We probably want to also specify a threshold number of misspellings that
;; trigger entry to the abbrev table, both on load and when made during the
;; current Emacs session. For now, I'll try a value of three for on-load and two
;; for misspellings made in the current Emacs session. I think I want to avoid a
;; value of one since that makes it easy for a misspelling with multiple valid
;; corrections to become associated with a single correction too soon. This is a
;; rare concern, but it would be annoying enough to run into that I think it's
;; worth requiring a second misspelling.

(defcustom autocorrect-count-threshold-alltime 3
  "The number of recorded identical misspellings to create an abbrev.
This applies to misspellings read from `autocorrect-file'."
  :type 'natnum)

(defcustom autocorrect-count-threshold-session 2
  "The number of identical misspellings to create an abbrev.
This applies to misspellings made in the current Emacs session."
  :type 'natnum)

;; It's probably not sensible to do autocorrection anywhere,
;; and predicate functions are a flexible way of allowing this to be specified.
(defcustom autocorrect-predicates nil
  "Predicate functions called at point with argument START.
These functions should return t if autocorrection is valid at START."
  :type '(repeat function))

(defcustom autocorrect-post-correct-hook nil
  "Hook run after an autocorrection has occurred.
Each function is called with two arguments, the original misspelling and the
correction inserted.

In some cases (when the correction consists of multiple words), it is not
possible to determine this information and this hook will not be run.
This limitation stems from the how abbrev implements post-insertion hooks."
  :type 'hook)

;; When we handle just-performed spelling corrections, if the word is capitalised
;; it could either be because:
;; - It is appearing at the start of the sentence
;; - It is a proper noun, and should always be capitalised
;;
;; We want to differentiate these two cases, which we can do by converting the
;; corrected word to lowercase and testing whether that form is spellchecked as
;; correct.
(defcustom autocorrect-check-spelling-function nil
  "Predicate function that indicates whether a word is correctly spelled.
This is used to check whether a correction can be safely lowercased."
  :type '(choice function (const nil)))

(defcustom autocorrect-child-abbrev-tables '(text-mode-abbrev-table prog-mode-abbrev-table)
  "List of abbrev tables that autocorrect should insert itself as a parent of.
Must be set before `autocorrect-setup' is first called to take effect."
  :type '(repeat variable))

;;;; Internal variables:

(defvar autocorrect--table (make-hash-table :test #'equal)
  "A record of all corrections made, generated from `autocorrect-file'.
Misspelled words are the keys, and a alist of corrections and their counts are
the values.")

(defvar autocorrect--abbrev-table nil
  "The spelling abbrev table.")

(defvar autocorrect--abbrev-table-saved-version 0
  "The version of `autocorrect--abbrev-table' saved to disk.")

(defvar autocorrect--file-mtime (seconds-to-time 0)
  "The mtime of `autocorrect-file' last time it was looked at.")

(defvar autocorrect--should-update-savefile nil
  "Indicator for whether there are any changes that should be written on save.")

;;;; Minor mode:

;;;###autoload
(define-minor-mode autocorrect-mode
  "Automatically correct misspellings with abbrev."
  :init-value t)

;;;###autoload
(define-globalized-minor-mode global-autocorrect-mode
  autocorrect-mode autocorrect--enable)

(defun autocorrect--enable ()
  "Turn on `autocorrect-mode' in the current buffer."
  (autocorrect-mode 1))

;;;; Utility functions:

(defun autocorrect--appropriate-p ()
  "Return non-nil it is currently appropriate to make an autocorrection.
See `autocorrect-predicates'."
  (and autocorrect-mode
       (run-hook-with-args-until-failure 'autocorrect-predicates (point))))

(defun autocorrect--run-post-correct-hook ()
  "Determine the correction, and run `autocorrect-post-correct-hook'.
It is expected that this is run as a post-abbrev hook within `abbrev-insert'.
This assumption allows us to determine the newly inserted correction, so long as
it is a single word, and then perform a reverse lookup of `autocorrect--table'
to find the original misspelling."
  (when autocorrect-post-correct-hook
    (let ((correction (substring-no-properties (thing-at-point 'word)))
          misspelling)
      (maphash
       (lambda (misp corrections)
         (unless misspelling
           (when (equal (caar corrections) correction)
             (setq misspelling misp))))
       autocorrect--table)
      (when misspelling
        (run-hook-with-args 'autocorrect-post-correct-hook
                            misspelling correction)))))

(defun autocorrect--capitalised-or-uppercase-p (word)
  "Return t if WORD is of form \"Capitalised\" or \"UPPERCASE\"."
  (and (not (string-empty-p word))
       (char-uppercase-p (aref word 0))
       ;; To check whether a function is indeed lowercase we'll try using
       ;; ~char-uppercase-p~ instead of Regexp for speed (I think but haven't
       ;; tested that this will be faster).
       (let ((letter-cases (mapcar #'char-uppercase-p word)))
         (or (not (memq t (cdr letter-cases)))
             (not (memq nil (cdr letter-cases)))))))

(defun autocorrect--should-downcase-p (misspelling corrected)
  "Check whether it is a good idea to `downcase' MISSPELLING and CORRECTED.
This is conditional on all of the following being true:
- MISSPELLING satisfies `autocorrect--capitalised-or-uppercase-p'
- CORRECTED satisfies `autocorrect--capitalised-or-uppercase-p'
- The lowercase form of CORRECTED satisfies
  `autocorrect-check-spelling-function'"
  (and autocorrect-check-spelling-function
       (autocorrect--capitalised-or-uppercase-p misspelling)
       (autocorrect--capitalised-or-uppercase-p corrected)
       (funcall autocorrect-check-spelling-function
                (downcase corrected))))

(defun autocorrect--update-table (misspelling corrected &optional manual-count auto-count)
  "Update the MISSPELLING to CORRECTED entry in the table.

Unless specified, it is assumed that MANUAL-COUNT is 1 and
AUTO-COUNT is 0, referring to the number of times MISSPELLING has
been manually and automatically corrected to CORRECTED
respectively."
  (let* ((correction-counts
          (gethash misspelling autocorrect--table))
         (record-cons
          (assoc corrected correction-counts))
         (manual-count (or manual-count 1))
         (auto-count (or auto-count 0)))
    (if record-cons
        (setcdr record-cons
                (cons (+ (cadr record-cons) manual-count)
                      (+ (cddr record-cons) auto-count)))
      (puthash misspelling
               (let ((cinfo (cons corrected (cons manual-count auto-count))))
                 (if correction-counts
                     (push cinfo correction-counts)
                   (list cinfo)))
               autocorrect--table))))

(defun autocorrect--write-to-file (&optional content append)
  "Write the string CONTENT or the current buffer to `autocorrect-file'.
APPEND is passed through to `write-region'."
  (let ((write-region-inhibit-fsync t) ; Quicker writes, not needed
        (coding-system-for-write 'utf-8)
        (inhibit-message t))
    (write-region content nil autocorrect-file append)))

;;;; Abbrev management:

(defun autocorrect--set-abbrev (misspelling corrected)
  "Create an abbrev from MISSPELLING to CORRECTED in `autocorrect--abbrev-table'."
  (let ((sym (obarray-put autocorrect--abbrev-table misspelling)))
    (unless (and (boundp sym) (equal (symbol-value sym) corrected))
      (set sym corrected)
      (fset sym #'autocorrect--run-post-correct-hook)
      (setplist sym (list :count 0 :system t))
      (abbrev-table-put
       autocorrect--abbrev-table :abbrev-table-modiff
       (1+ (abbrev-table-get autocorrect--abbrev-table :abbrev-table-modiff))))))

(defun autocorrect--unset-abbrev (misspelling)
  "Remove any abbrevs associated with MISSPELLING in `autocorrect--abbrev-table'."
  (let ((sym (obarray-get autocorrect--abbrev-table misspelling)))
    (when sym (obarray-remove autocorrect--abbrev-table sym))))

(defun autocorrect--setup-abbrevs ()
  "Setup `autocorrect--abbrev-table'.
Also set it as a parent of `global-abbrev-table'."
  (unless autocorrect--abbrev-table
    (setq autocorrect--abbrev-table
          (make-abbrev-table (list :enable-function #'autocorrect--appropriate-p)))
    (dolist (child-abbrev-table (mapcar #'symbol-value autocorrect-child-abbrev-tables))
      (abbrev-table-put
       child-abbrev-table :parents
       (cons autocorrect--abbrev-table
             (abbrev-table-get child-abbrev-table :parents))))))

(defun autocorrect--create-abbrevs ()
  "Apply the history threshold to the current correction table."
  (maphash
   (lambda (misspelling corrections)
     (when (and (= (length corrections) 1)
                (>= (cadar corrections)
                    autocorrect-count-threshold-alltime))
       (autocorrect--set-abbrev misspelling (caar corrections))))
   autocorrect--table))

(defun autocorrect--transfer-abbrev-counts-to-table ()
  "Transfer autocorrect counts from the abbrev table to the record table.
More specifically, this finds all entries of `autocorrect--abbrev-table'
with a non-zero :count field, adds that value to the autocorrect count via
`autocorrect--update-table', and zeros the :count field."
  (let (symbols)
    (mapatoms
     (lambda (sym)
       (push sym symbols))
     autocorrect--abbrev-table)
    (dolist (sym symbols)
      (when (and (gethash (symbol-name sym) autocorrect--table)
                 (> (get sym :count) 0))
        (autocorrect--update-table
         (symbol-name sym) (symbol-value sym)
         0 (get sym :count))
        (put sym :count 0)))))

(defun autocorrect--remove-invalid-abbrevs ()
  "Remove entries of `autocorrect--abbrev-table' not in `autocorrect--table'."
  (mapatoms
   (lambda (symb)
     (when (symbol-value symb)
       (let ((misspelling (symbol-name symb)))
         (let ((corrections (gethash misspelling autocorrect--table)))
           (unless (and (= (length corrections) 1)
                        (>= (cadar corrections)
                            autocorrect-count-threshold-alltime))
             (obarray-remove autocorrect--abbrev-table symb))))))
   autocorrect--abbrev-table))

(defun autocorrect--sync-abbrevs ()
  "Synchronise `autocorrect--abbrev-table' with `autocorrect--table'."
  (autocorrect--remove-invalid-abbrevs)
  (autocorrect--create-abbrevs))

;;;; History management:

(defun autocorrect--read ()
  "Read `autocorrect-file' into the correction table."
  (unless (hash-table-empty-p autocorrect--table)
    (setq autocorrect--table (make-hash-table :test #'equal)))
  (if (file-exists-p autocorrect-file)
      (with-temp-buffer
        (insert-file-contents autocorrect-file)
        (setq autocorrect--file-mtime
              (file-attribute-modification-time (file-attributes autocorrect-file)))
        (goto-char (point-min))
        (let ((pt (point))
              misspelling next-word
              manual-count auto-count
              corrected)
          (while (< (point) (point-max))
            (setq misspelling
                  (and (forward-word)
                       (buffer-substring pt (point)))
                  pt (1+ (point)))
            (setq next-word
                  (or (and (forward-word)
                           (buffer-substring pt (point)))
                      ""))
            (setq manual-count (string-to-number next-word))
            (if (and (= manual-count 0) (not (string= next-word "0")))
                (setq corrected
                      (if (eolp) next-word (buffer-substring pt (pos-eol)))
                      manual-count 1
                      auto-count 0)
              (setq pt (1+ (point))
                    next-word
                    (or (and (forward-word)
                             (buffer-substring pt (point)))
                        ""))
              (setq auto-count (string-to-number next-word)
                    corrected
                    (if (and (= auto-count 0) (not (string= next-word "0")))
                        (if (eolp) next-word (buffer-substring pt (pos-eol)))
                      (buffer-substring (min (1+ (point)) (pos-eol))
                                        (pos-eol)))))
            (forward-line 1)
            (setq pt (point))
            (when (and misspelling corrected)
              (autocorrect--update-table
               misspelling corrected manual-count auto-count)))))
    (make-directory (file-name-directory autocorrect-file))
    (write-region "" nil autocorrect-file)))

(defun autocorrect--write ()
  "Write the current `autocorrect--table' to `autocorrect-file'."
  (with-temp-buffer
    (maphash
     (lambda (misspelling corrections)
       (dolist (correction corrections)
         (insert misspelling
                 " " (number-to-string (cadr correction))
                 " " (number-to-string (cddr correction))
                 " " (car correction)
                 "\n")))
     autocorrect--table)
    (autocorrect--write-to-file)
    (setq autocorrect--file-mtime
          (file-attribute-modification-time (file-attributes autocorrect-file)))))

;;;; List UI

(defvar autocorrect--list-format
  `[("Misspelling" 16
     ,(lambda (a b)
        (string< (aref (cadr a) 0) (aref (cadr b) 0))))
    (" → " 4
     ,(lambda (a b)
        (let ((a-face (car (alist-get 'face (list (text-properties-at 1 (aref (cadr a) 1))))))
              (b-face (car (alist-get 'face (list (text-properties-at 1 (aref (cadr b) 1))))))
              (face-priorities '(error default shadow)))
          (< (length (memq a-face face-priorities))
             (length (memq b-face face-priorities))))))
    ("Correction" 16
     ,(lambda (a b)
        (string< (aref (cadr a) 2) (aref (cadr b) 2))))
    ("#Manual" 8
     ,(lambda (a b)
        (< (string-to-number (aref (cadr a) 3))
           (string-to-number (aref (cadr b) 3))))
     :right-align t)
    ("#Auto" 6
     ,(lambda (a b)
        (< (string-to-number (aref (cadr a) 4))
           (string-to-number (aref (cadr b) 4))))
     :right-align t)]
  "Table format for the error list.")

(defun autocorrect--list-entries ()
  "Generated a list of current autocorrections for `autocorrect-list-mode'."
  (let ((misspelled-col-width 12)
        (corrected-col-width 12)
        entries)
    (maphash
     (lambda (misspelling corrections)
       (setq misspelled-col-width (max misspelled-col-width (length misspelling)))
       (let ((active-abbrev (obarray-get autocorrect--abbrev-table misspelling))
             (ignored-p (member "" (mapcar #'car corrections)))
             (ambiguous-p (> (length corrections) 1))
             (session-only-p
              (and (= 1 (length corrections))
                   (> autocorrect-count-threshold-alltime (cadar corrections))
                   (>= (cadar corrections) autocorrect-count-threshold-session))))
         (when (and active-abbrev (not (symbol-value active-abbrev)))
           (setq active-abbrev nil))
         (dolist (correction corrections)
           (setq corrected-col-width (max corrected-col-width (length (car correction))))
           (push (list (cons misspelling correction)
                       (vector
                        (if (and ambiguous-p (not ignored-p))
                            (progn
                              (setq misspelled-col-width (max misspelled-col-width (+ (length misspelling) 4)))
                              (concat misspelling " "
                                      (propertize "(" 'face 'shadow)
                                      (propertize
                                       (number-to-string
                                        (- (length corrections)
                                           (length (memq correction corrections))
                                           -1))
                                       'face '(italic shadow))
                                      (propertize ")" 'face 'shadow)))
                          (propertize misspelling
                                      'face (if ignored-p 'shadow 'default)))
                        (concat
                         " "
                         (cond
                          (active-abbrev "→")
                          (ignored-p (propertize "→" 'face 'error))
                          (t (propertize "→" 'face 'shadow))))
                        (if (string-empty-p (car correction))
                            (propertize "ignore flag" 'face '(shadow italic))
                          (propertize (car correction) 'face 'success))
                        (if (string-empty-p (car correction))
                            (propertize "-" 'face 'shadow)
                          (propertize
                           (number-to-string (cadr correction))
                           'face
                           (cond
                            (session-only-p
                             'warning)
                            (active-abbrev
                             'font-lock-number-face)
                            (t 'shadow))))
                        (let ((auto-count
                               (+ (cddr correction)
                                  (if active-abbrev
                                      (get active-abbrev :count)
                                    0))))
                          (if (and (= auto-count 0)
                                   (not active-abbrev))
                              (propertize "-" 'face 'shadow)
                            (propertize
                             (number-to-string auto-count)
                             'face (if (= auto-count 0)
                                       'shadow
                                     'font-lock-number-face))))))
                 entries))))
     autocorrect--table)
    (setf (cadr (aref autocorrect--list-format 0)) misspelled-col-width)
    (setf (cadr (aref autocorrect--list-format 2)) corrected-col-width)
    entries))

(defvar-keymap autocorrect-list-mode-map
  :doc "Keymap for `autocorrect-list-mode'."
  "i" #'autocorrect-ignore-word
  "x" #'autocorrect-remove-correction
  "a" #'autocorrect-create-correction)

(define-derived-mode autocorrect-list-mode tabulated-list-mode
  "Autocorrections"
  "Major mode for listing autocorrections.

\\{autocorrect-list-mode-map}"
  (setq tabulated-list-format autocorrect--list-format
        tabulated-list-sort-key (cons "Misspelling" nil)
        tabulated-list-padding 1
        tabulated-list-entries #'autocorrect--list-entries)
  (setq-local truncate-string-ellipsis "…")
  (tabulated-list-init-header))

;;;; User-facing functions:

(defun autocorrect-list ()
  "List loaded autocorrections."
  (interactive)
  (let ((buffer (get-buffer-create "*Autocorrections*")))
    (with-current-buffer buffer
      (autocorrect-list-mode)
      (tabulated-list-print))
    (switch-to-buffer buffer)))

(defun autocorrect-ignore-word (word)
  "Prevent WORD from being autocorrected."
  (interactive
   (list (if (eq major-mode 'autocorrect-list-mode)
             (car (tabulated-list-get-id (point)))
           (completing-read
            "Word: " (sort (hash-table-keys autocorrect--table) #'string<)))))
  (autocorrect--update-table word "" 0 0)
  (autocorrect--unset-abbrev word)
  (autocorrect--write-to-file (concat word " 0 0\n") t)
  (when (eq major-mode 'autocorrect-list-mode)
    (tabulated-list-print t nil)))

(defun autocorrect-remove-correction (misspelling corrected)
  "Remove the record for MISSPELLING to CORRECTED from the autocorrect table."
  (interactive
   (if (eq major-mode 'autocorrect-list-mode)
       (let ((info (tabulated-list-get-id (point))))
         (if (yes-or-no-p
              (format
               (if (string-empty-p (cadr info))
                   "Remove ignore flag from %s?"
                 "Remove %s ⟶ %s?")
               (propertize (car info) 'face 'warning)
               (propertize (cadr info) 'face 'success)))
             (list (car info) (cadr info))
           (user-error "")))
     (let* ((misp-words (hash-table-keys autocorrect--table))
            (misp (completing-read "Misspelling: " (sort misp-words #'string<) nil t))
            (corrections (gethash misp autocorrect--table))
            (correction-words
             (mapcar
              (lambda (c) (if (string-empty-p (car c))
                         (concat (propertize "⚑" 'face 'error) " "
                                 (propertize "ignore flag" 'face 'italic))
                       (car c)))
              corrections))
            (crtn (completing-read "Correction: " (sort correction-words #'string<) nil t)))
       (when (equal (text-properties-at 0 crtn) '(face error))
         (setq crtn ""))
       (list misp crtn))))
  (let ((entry (gethash misspelling autocorrect--table)))
    (when entry
      (autocorrect-reload)
      (if (= 1 (length entry))
          (progn
            (remhash misspelling autocorrect--table)
            (autocorrect--unset-abbrev misspelling))
        (setq entry (delq (assoc corrected entry) entry))
        (puthash misspelling entry autocorrect--table)
        (when (= 1 (length entry))
          (autocorrect--maybe-create-abbrev misspelling (caar entry))))
      (autocorrect-save)))
  (message "Removed %s ⟶ %s"
           (propertize misspelling 'face 'warning)
           (propertize corrected 'face 'success))
  (when (eq major-mode 'autocorrect-list-mode)
    (tabulated-list-print t nil)))

(defun autocorrect-create-correction (misspelling corrected)
  "Create an autocorrection from MISSPELLING to CORRECTED.
To instantly become active, we pretend that this correction has
already been manually made as many times as needed according to
`autocorrect-count-threshold-alltime'."
  (interactive
   (list (read-string "Misspelling: ")
         (read-string "Corrected: ")))
  (cond
   ((string-empty-p misspelling)
    (user-error "Misspelling must be non-empty"))
   ((string-empty-p corrected)
    (user-error "Correction must be non-empty, did you want `autocorrect-ignore-word'?")))
  (autocorrect--write-to-file
   (concat misspelling " " corrected " "
           (number-to-string autocorrect-count-threshold-alltime)
           "\n")
   t)
  (autocorrect--update-table
   misspelling corrected autocorrect-count-threshold-alltime)
  (autocorrect--maybe-create-abbrev misspelling corrected)
  (when (eq major-mode 'autocorrect-list-mode)
    (tabulated-list-print t nil)))

(defun autocorrect-reload ()
  "Reload `autocorrect-file' if it has changed since it was last read."
  (interactive)
  (when (time-less-p
         autocorrect--file-mtime
         (file-attribute-modification-time (file-attributes autocorrect-file)))
    (autocorrect--read)))

(defun autocorrect-save ()
  "Save the current autocorrect information to `autocorrect-file'."
  (interactive)
  (autocorrect-reload)
  (autocorrect--transfer-abbrev-counts-to-table)
  (autocorrect--write))

;;;; Spellchecker interface:

(defun autocorrect--maybe-create-abbrev (misspelling corrected)
  "Update the autocorrect table and possibly create an abbrev.
The correction count for MISSPELLING to CORRECTED in the table is incremented by
one, and should the number of corrections exceed
`autocorrect-count-threshold-session' and there be no other recorded
corrections, and abbrev will be created."
  (when (and (>= (or (cadr (assoc corrected (gethash misspelling autocorrect--table))) 0)
                 autocorrect-count-threshold-session)
             (= 1 (length (gethash misspelling autocorrect--table))))
    (autocorrect--set-abbrev misspelling corrected)
    (message "Created new autocorrection: %s ⟶ %s"
             (propertize misspelling 'face 'warning)
             (propertize corrected 'face 'success))))

(defun autocorrect-record-correction (misspelling corrected)
  "Record the correction of MISSPELLING to CORRECTED."
  (when (autocorrect--should-downcase-p misspelling corrected)
    (setq misspelling (downcase misspelling)
          corrected (downcase corrected)))
  (autocorrect--write-to-file
   (concat misspelling " " corrected "\n") t)
  (autocorrect--update-table misspelling corrected)
  (autocorrect--maybe-create-abbrev misspelling corrected))

;;;; Setup:

;;;###autoload
(defun autocorrect-setup ()
  "Read and process the history file into abbrevs."
  (autocorrect--setup-abbrevs)
  (autocorrect--read)
  (autocorrect--sync-abbrevs)
  (add-hook 'kill-emacs-hook #'autocorrect-save))

(provide 'autocorrect)
;;; autocorrect.el ends here
