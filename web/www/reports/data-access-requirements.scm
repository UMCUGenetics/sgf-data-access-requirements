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
  #:use-module (www config)
  #:use-module (www db data-access-requirements)
  #:use-module (www reports)
  #:use-module (www util)
  #:use-module (logger)

  #:export (title project report-overview report-pdf))

(define title   "Data access requirements")
(define project "d0df862e036f1e12b8613cca05100ef01454351864e600267348f840ca088434")

(define (report-pdf port submission-id)
  (let* ((pdf  (pdf-report))
         (data (car (form-submission-by-id submission-id)))
         (get  (lambda (field)
                 (let ((s (assoc-ref data field)))
                   (format #f "~a" s)))))
    (cond
     [(not pdf)
      (log-error "report-pdf" "The PDF is not.")
      #f]
     [(not data)
      (log-error "report-pdf" "The data is not there.")
      #f]
     [else
      (pdf-report-set-logo!          pdf (resolved-static-file-path "static/images/umcu-logo.png") 'center)
      (pdf-report-set-title!         pdf "Data Access Requirements")
      (pdf-report-set-subtitle!      pdf "DAC DBG UMC Utrecht")
      (pdf-report-render-text-field! pdf "Submission date" (get "submissionDate") 1)
      (pdf-report-render-section!    pdf "Submission details")
      (pdf-report-render-subsection! pdf "Dataset reference")
      (pdf-report-render-text-field! pdf "EGA Dataset ID"  (get "datasetId") 1)
      (pdf-report-render-text-field! pdf "Dataset title"   (get "datasetTitle") 1)
      (pdf-report-render-text-field! pdf "Project website" (get "projectWebsite") 1)
      (pdf-report-render-spacer!     pdf 12)
      (pdf-report-render-subsection! pdf "Producers and collaborators")
      (pdf-report-render-text-field! pdf "P.I." (get "principalInvestigator") 1)
      (pdf-report-render-text-field! pdf "Data producer" (get "dataProducer") 1)
      (pdf-report-render-text-field! pdf "Collaborators" (get "collaborators") 1)
      (pdf-report-render-spacer!     pdf 12)
      (pdf-report-render-section!    pdf "Dataset protection details")
      (pdf-report-render-subsection! pdf "Specific limitations on area of research")

      ;; DUO
      (let* ((primary (basename (get "primaryDuo")))
             (primary-name (assoc-ref %primary-license-terms primary)))
        (pdf-report-render-text-field! pdf "Data usage license"
                                       (cond
                                        [primary-name primary-name]
                                        [primary primary]
                                        [else "Unknown"]) 1))

      (let ((additional-duo-terms (form-additional-duo-terms-by-submission submission-id)))
        (unless (null? additional-duo-terms)
          (let* ((duo-term (car additional-duo-terms))
                 (term (basename duo-term))
                 (term-name (assoc-ref %additional-limitation-terms term)))
            (pdf-report-render-text-field! pdf "Additional limitations"
                                           (cond
                                            [term-name term-name]
                                            [term term]
                                            [else "Unknown"]) 1))
          (for-each (lambda (duo-term)
                      (let* ((term (basename duo-term))
                             (term-name (assoc-ref %additional-limitation-terms term)))
                        (pdf-report-render-text-field! pdf " "
                                                       (cond
                                                        [term-name term-name]
                                                        [term term]
                                                        [else "Unknown"]) 1)))
                    (cdr additional-duo-terms))))

      (pdf-report-new-page! pdf)
      (pdf-report-render-section!    pdf "Dataset Adminstration Details")
      (pdf-report-render-text-field! pdf "Data owner"
                                     (get "dataAdministrativeOwners") 5)
      (pdf-report-render-text-field! pdf "Minimum protection measures required"
                                     (get "minimumProtectionMeasures") 5)
      (pdf-report-new-page! pdf)
      (let ((access-type (get "accessType")))
        (pdf-report-render-text-field! pdf "Access type"
                                       (if (string-prefix? "sgf://0.0.1/access-type-" access-type)
                                           (string-capitalize (substring access-type 24))
                                           access-type) 1))
      (pdf-report-write-to-port! port pdf #t)
      (pdf-report-close pdf)
      #t])))

(define (report-overview)
  `(table (@ (class "item-table"))
          (tr (th "Dataset ID")
              (th "Title")
              (th "Usage license")
              (th "Submittter")
              (th (@ (style "white-space: nowrap; text-align: right;")) "View as"))
          ,(map (lambda (entry)
                  `(tr (td (a (@ (href ,(string-append
                                         "/form/data-access-requirements?submission-id="
                                         (assoc-ref entry "submissionId"))))
                              ,(assoc-ref entry "datasetId")))
                       (td ,(assoc-ref entry "datasetTitle"))
                       (td ,(assoc-ref entry "primaryDuo"))
                       (td ,(assoc-ref entry "emailAddress"))
                       (td (@ (class "button-column right-button-column"))
                           (a (@ (href ,(string-append
                                         "/report/" project
                                         "/data-access-requirements/"
                                         (assoc-ref entry "submissionId") ".pdf")))
                              ,(icon 'pdf)))))
                (form-submissions))))
