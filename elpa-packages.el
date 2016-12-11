;;; elpa-packages.el --- Tools for collect information on ELPA Packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; Keywords: tools
;; Version: 0
;; Package-Requires: ((cl-lib "0.5"))

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

;;; Code:

(require 'cl-lib)
(require 'json)

(defun package-build--sym-to-keyword (s)
  "Return a version of symbol S as a :keyword."
  (intern (concat ":" (symbol-name s))))

(defun package-build--pkg-info-for-json (info)
  "Convert INFO into a data structure which will serialize to JSON in the desired shape."
  (let* ((ver (elt info 0))
         (deps (elt info 1))
         (desc (elt info 2))
         (type (elt info 3))
         (props (when (> (length info) 4) (elt info 4))))
    (list :ver ver
          :deps (cl-mapcan (lambda (dep)
                             (list (package-build--sym-to-keyword (car dep))
                                   (cadr dep)))
                           deps)
          :desc desc
          :type type
          :props props)))

(defun packages--archive-alist (archive-file)
  "Return the archive alist in ARCHIVE-FILE."
  (with-temp-buffer
    (insert-file-contents archive-file)
    (cdr (read (current-buffer)))))

(defun packages--archive-alist-for-json (alist)
  "Return the archive alist in a form suitable for JSON encoding."
  (cl-mapcan (lambda (entry)
               (list (package-build--sym-to-keyword (car entry))
                     (package-build--pkg-info-for-json (cdr entry))))
             alist))

(defun packages-archive-as-json (archive-file json-file)
  "Dump the ARCHIVE-FILE to JSON-FILE as json."
  (cl-assert (file-readable-p archive-file) t)
  (with-temp-file json-file
    (insert (json-encode (packages--archive-alist-for-json (packages--archive-alist archive-file))))))

(defun packages-archives-as-json (archive-files json-file)
  (let (al)
    (dolist (f archive-files)
      (setq al (append (packages--archive-alist f) al)))
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file json-file
        (insert (json-encode
                 (packages--archive-alist-for-json
                  (sort (cl-remove-duplicates al :test (lambda (p1 p2) (eq (car p1) (car p2))))
                        (lambda (p1 p2)
                          (string< (symbol-name (car p1))
                                   (symbol-name (car p2))))))))))))

(defun packages--archive-url (elpa)
  (format "http://elpa.emacs-china.org/%s/archive-contents" elpa))

(let (archive-files)
  (dolist (elpa (nreverse               ; Prefer GNU ELPA (`cl-remove-duplicates' keeps the last one)
                 '(gnu
                   melpa
                   melpa-stable
                   org
                   marmalade
                   sunrise-commander
                   user42)))
    (let ((url (packages--archive-url elpa))
          (file (format "%s-archive-contents" elpa)))
      (url-copy-file url file t)
      (push file archive-files)))
  (packages-archives-as-json archive-files "all.json"))

(provide 'elpa-packages)
;;; elpa-packages.el ends here
