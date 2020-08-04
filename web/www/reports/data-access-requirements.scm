;;; Copyright © 2020  Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (www reports data-access-requirements)
  #:use-module (www db data-access-requirements)

  #:export (title project report-overview))

(define title   "Data access requirements")
(define project "d0df862e036f1e12b8613cca05100ef01454351864e600267348f840ca088434")

(define (report-overview)
  `(table (@ (class "item-table"))
          (tr (th "Dataset ID")
              (th "Title")
              (th "Usage license")
              (th "Submittter"))
          ,(map (lambda (entry)
                 `(tr (td ,(assoc-ref entry "id"))
                      (td ,(assoc-ref entry "title"))
                      (td ,(assoc-ref entry "primaryDuo"))
                      (td ,(assoc-ref entry "email"))))
                (form-submissions))))
