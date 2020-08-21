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

(define-module (www forms data-access-requirements)
  #:use-module (logger)
  #:use-module (ice-9 receive)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (www config)
  #:use-module (www db api)
  #:use-module (www db data-access-requirements)
  #:use-module (www forms)
  #:use-module (www pages)
  #:use-module (www util)

  #:export (page submit api))

(define (accordion-button prefix-text level)
  `(div (@ (class "large-action-btn")
           (onclick ,(js "proceed(" level ")")))
        (a (@ (href "#")
              (onclick ,(js "proceed(" level ")")))
           ,(format #f "~a to step ~a" prefix-text (1+ level)))))

(define (proceed-button level) (accordion-button "Proceed" level))
(define (back-button level)    (accordion-button "Back" level))

;; ----------------------------------------------------------------------------
;; CUSTOM LOOK
;; ----------------------------------------------------------------------------

(define* (page-template title request-path content-tree #:key (dependencies '()))
  `((html (@ (lang "en"))
     (head
      (title ,(string-append (www-name) " | " title))
      (meta (@ (http-equiv "Content-Type") (content "text/html; charset=utf-8")))
      (link (@ (rel "icon")
               (type "image/x-icon")
               (href "/static/images/favicon.ico")))
      ,(dependent-scripts-and-styles (reverse dependencies))
      (link (@ (rel "stylesheet")
               (href "/static/css/udac.css")
               (type "text/css"))))
     (body
      (div (@ (id "wrapper"))
           (div (@ (role "banner")
                   (id "header"))
                (div (@ (class "title")
                        (style "text-align: center"))
                     (img (@ (src "/static/images/ubec-logo.png")
                             (alt ,(www-name))))))
           (div (@ (role "main")
                   (id "content")) ,content-tree)
           (div (@ (role "contentinfo")
                   (id "footer"))
                (p "Powered by "
                   (a (@ (href "https://www.sparqling-genomics.org")
                         (target "_blank"))
                      "SPARQLing-genomics") ".")))))))

;; ----------------------------------------------------------------------------
;; PAGE
;; ----------------------------------------------------------------------------

(define* (page request-path arguments #:key (error-message #f) (data '()))

  (when (and arguments (assoc-ref arguments 'submission-id))
    (set! data (form-submission-by-id (assoc-ref arguments 'submission-id)))
    (set! error-message #t))

  (let ((show-missing? (not (null? data))))
    (page-template "Data access requirements" request-path
     `((h2 "Data access requirements")

       ,(if error-message
            `(div (@ (class "form-error-message"))
                  ,(if (string? error-message)
                       `(p ,error-message)
                       `(p "Please fill in the fields marked in red.")))
            '())

       (form (@ (id "dar-form")
                (method "POST")
                (action ,request-path))

             (div (@ (id "accordion"))

                  ;; SUBMISSION DETAILS
                  ;; ------------------------------------------------------------
                  (h3 "Submission details")
                  (div
                   (h4 "Submission date")
                   ,(input-with-value "submission-date" "date"
                     (assoc-ref data 'submission-date)
                     #:required? #t
                     #:show-missing? show-missing?)

                   (h4 "Dataset reference")
                   (table (tr (td (@ (style "width: 150pt"))
                                  "EGA Dataset ID")
                              (td ,(input-with-value "dataset-id" "text"
                                    (assoc-ref data 'dataset-id)
                                    #:required? #t
                                    #:show-missing? show-missing?
                                    #:placeholder "Example: EGAD00001000001")))
                          (tr (td "Dataset title")
                              (td ,(input-with-value "dataset-title" "text"
                                    (assoc-ref data 'dataset-title)
                                    #:required? #t
                                    #:show-missing? show-missing?)))
                          (tr (td "Project website")
                              (td ,(input-with-value "project-website" "text"
                                    (assoc-ref data 'project-website)
                                    #:required? #t
                                    #:show-missing? show-missing?
                                    #:placeholder "e.g. ZonMw project page."))))

                   (h4 "Producers and collaborators")
                   (table
                     (tr (td (@ (style "width: 150pt"))
                             "Principal investigator")
                         (td ,(input-with-value "principal-investigator" "text"
                               (assoc-ref data 'principal-investigator)
                               #:required? #t
                               #:show-missing? show-missing?)))
                     (tr (td "Data producer")
                         (td ,(input-with-value "data-producer" "text"
                               (assoc-ref data 'data-producer)
                               #:required? #t
                               #:show-missing? show-missing?)))
                     (tr (td "Collaborators")
                         (td ,(input-with-value "collaborators" "text"
                               (assoc-ref data 'collaborators)))))

                   (h4 "Proceed")
                   ,(proceed-button 1))

                  ;; DATASET PROTECTION DETAILS
                  ;; ------------------------------------------------------------
                  (h3 "Dataset protection details")
                  (div
                   (h4 "Data controller")
                   ,(ul-without-bullets (and (not (string? (assoc-ref data 'data-controller-agreed)))
                                             show-missing?)
                     `(li ,(checkbox "data-controller-agreed"
                            (string-append
                             "I agree that the UBEC DAC of the UMC Utrecht "
                             "will become the primary data controller, and "
                             "that approved data reuse applicants will sign "
                             "the Data Transfer Agreement (DTA) of the UMC "
                             "Utrecht.")
                            #:required? #t
                            #:checked? (assoc-ref data 'data-controller-agreed))))

                   (h4 "Specific limitations on area of research")
                   ,(let ((is-checked?
                           (lambda (item)
                             (and (string? (assoc-ref data 'dul-radio))
                                  (string= (assoc-ref data 'dul-radio) item)))))
                      (ul-without-bullets (and (not (string? (assoc-ref data 'dul-radio)))
                                               show-missing?)
                       `(li (strong "No restrictions"))
                       `(li ,(radio-button "DUO_0000004" "dul-radio"
                                           "This dataset has no sharing restrictions"
                                           #:checked? (is-checked? "DUO_0000004")
                                           #:onchange (js "hide_additional_duo()")))
                       `(li (strong "General research use"))
                       `(li ,(radio-button "DUO_0000005"  "dul-radio"
                                           "General research use and clinical care"
                                           #:checked? (is-checked? "DUO_0000005")
                                           #:onchange (js "show_additional_duo()")))
                       `(li ,(radio-button "DUO_0000006"  "dul-radio"
                                           "Health or medical or biomedical research"
                                           #:checked? (is-checked? "DUO_0000006")
                                           #:onchange (js "show_additional_duo()")))
                       `(li ,(radio-button "DUO_0000007" "dul-radio"
                                           "Disease-specific research"
                                           #:checked? (is-checked? "DUO_0000007")
                                           #:onchange (js "show_additional_duo()")))
                       `(li ,(radio-button "DUO_0000011"  "dul-radio"
                                           "Population origins or ancestry research"
                                           #:checked? (is-checked? "DUO_0000011")
                                           #:onchange (js "show_additional_duo()")))))

                   (div (@ (id "additional-limitations"))
                     (h4 "Additional limitations on re-use of data")

                     ,(ul-without-bullets #f
                       (map (lambda (item)
                             `(li ,(checkbox (car item) (cdr item)
                                    #:checked? (assoc-ref data (string->symbol
                                                                (car item))))))
                            %additional-limitation-terms)))

                   (h4 "Consent")
                   ,(ul-without-bullets #f
                     `(li ,(checkbox "consent-box"
                                     "Consent is available for the individuals participated in   this project."
                                     #:class "form-checkbox"
                                     #:checked? (not (not (assoc-ref data 'consent-box)))
                                     #:onchange (js "toggle_consent_box()"))))

                   (div (@ (id "consent-form-location-id"))
                        (p "The information and consent forms are located at:")
                        ,(textarea "consent-form-location"
                          #:value (assoc-ref data 'consent-form-location)
                          #:required? (not (not (assoc-ref data 'consent-box)))
                          #:show-missing? show-missing?))

                   (h4 "Proceed")
                   ,(proceed-button 2)
                   ,(back-button 0))

                  ;; DATASET PROTECTION DETAILS
                  ;; ------------------------------------------------------------
                  (h3 "Dataset administration details")
                  (div
                   (h4 "Data owner")
                   ,(textarea "data-administrative-owners"
                     #:placeholder
                     (string-append
                      "Please note other data ownership related "
                      "information. Please define who should be "
                      "contacted in case of further questions.")
                     #:required? #t
                     #:show-missing? show-missing?
                     #:value (assoc-ref data 'data-administrative-owners))

                   (h4 "Minimum protection measures required")
                   ,(textarea "minimum-protection-measures"
                     #:placeholder
                     (string-append
                      "Please note other protection measures that apply "
                      "to each data-set. That can also include technical "
                      "conditions the data receiving institutions has to "
                      "comply to.")
                     #:value (assoc-ref data 'minimum-protection-measures))

                   (h4 "Other limitations for use")
                   (p (@ (style "font-weight: bold;")) "Access type: ")
                   (p "Please note that sensitive and human related data "
                      "cannot be shared openly.")
                   (span
                    ,@(if (and show-missing?
                               (not (assoc-ref data 'access-type)))
                          `((@ ,required-marking))
                          '())
                    ,(map (lambda (item)
                            (let ((id (string-append "access-type-"
                                                     (string-downcase item)))
                                  (current (assoc-ref data 'access-type)))
                              (radio-button id "access-type" item
                               #:checked? (and current (string= current id)))))
                          '("Open" "Restricted" "Embargoed" "Closed")))
                   (p (@ (style "font-weight: bold; margin-bottom: 0pt;"))
                      "Explanation of limitations:")
                   ,(textarea "other-use-limitations"
                     #:value (assoc-ref data 'other-use-limitations))

                   (h4 "Citing and crediting")
                   ,(textarea "citing"
                     #:placeholder
                     (string-append
                      "Please explain how you would your data and "
                      "related publication to be cited and "
                      "credited.")
                     #:value (assoc-ref data 'citing))

                   (h4 "File access")
                   (p "Receiver of the Data guarantees the below restrictions on file access:")
                   (ul
                    (li "Data can be held in unencrypted files on an institutional"
                        " compute system, with Unix user group read/write access for"
                        " one of more appropriate groups but not Unix world"
                        " read/write access behind a secure firewall. Laptops"
                        " holding these data should have password protected logins"
                        " and screenlocks (set to lock after 5 minutes of"
                        " inactivity). If held on USB keys or other portable hard"
                        " drives, the data must be encrypted.")
                    (li "Data is not transferrable to third parties. If a third"
                        " party should be granted access to the Data, the third"
                        " party should apply through the formal DAC procedure (see"
                        " below: ‘Process of acquiring access to dataset’).")
                    (li "If Receiver of the Data is leaving the institute where"
                        " he/she acquired the Data, Data should be removed from"
                        " all systems."))

                   (h4 "Additional measures")
                   ,(textarea "additional-measures"
                     #:placeholder
                     (string-append
                      "Please define other technical restrictions or "
                      "reporting measures you see fit.")
                     #:value (assoc-ref data 'additional-measures))

                   (h4 "Process of acquiring access to dataset")
                   (p "The access to this dataset can be acquired by contacting the"
                      " Data Access Committee of DBG via "
                      (a (@ (href "mailto:dacdbg@umcutrecht.nl"))
                         "dacdbg@umcutrecht.nl") ".")

                   ,(textarea "additional-communication"
                     #:placeholder
                     (string-append
                      "Please add specific communication wishes in the "
                      "case of a data access request of the data-set(s) "
                      "mentioned above.")
                     #:value (assoc-ref data 'additional-communication)
                     #:required? #t
                     #:show-missing? show-missing?)

                   (h4 "Contact information")
                   (p "At what e-mail address can we reach you to follow-up "
                      "on this request?")
                   ,(input-with-value "email-address" "text"
                                    (assoc-ref data 'email-address)
                                    #:required? #t
                                    #:show-missing? show-missing?)

                   (div (@ (class "large-submit-btn")
                           (onclick ,(js "submit_form()")))
                        (a (@ (href "#")
                              (onclick ,(js "submit_form()")))
                           "Submit form"))

                   (div (@ (class "large-action-btn")
                           (onclick ,(js "proceed(1)")))
                        (a (@ (href "#")
                              (onclick ,(js "proceed(1)")))
                           "Back to step 2")))))
       (script (@ (src "/static/js/data-access-requirements.js")) ""))
     #:dependencies '(jquery jquery-ui))))

;; ----------------------------------------------------------------------------
;; SUBMIT
;; ----------------------------------------------------------------------------

(define (submit request-path post-data)
  (log-debug "data-access-requirements submit" "Received:~%~s" post-data)

  (receive (state message)
    (add-submission post-data)
    (if state
        (page-template "Thank you!" request-path
         `((h2 "Thank you!")
           (p "Your submission for " ,(assoc-ref post-data 'dataset-id)
              " has been received and we will follow-up to "
              ,(assoc-ref post-data 'email-address) ".")))
        (page request-path #f
              #:error-message (if (list? message)
                                  (list-ref message 0)
                                  message)
              #:data post-data))))

;; ----------------------------------------------------------------------------
;; API-HANDLER
;; ----------------------------------------------------------------------------

(define (api request-path arguments input-port output-port
             accept-type content-type content-length)
  "Implements the form's internal API handler to provide auto-completions."

  (cond
   ;; ALL-STUDIES
   ;; -------------------------------------------------------------------------
   [(string-suffix? "/api/all-studies" request-path)
    (let ((studies (all-study-ids)))
      (respond-200 output-port accept-type `((studies . ,studies))))]

   ;; DATASET AUTOCOMPLETE
   ;; -------------------------------------------------------------------------
   [(string-suffix? "/api/dataset-ac" request-path)
    (let ((data (dataset-ac)))
      (respond-200 output-port accept-type data))]

   ;; DATASETS-BY-STUDY-ID
   ;; -------------------------------------------------------------------------
   [(string-suffix? "/api/datasets-by-study-id" request-path)
    (let* ((raw-data (utf8->string (get-bytevector-n input-port content-length)))
           (data     (api-request-data->alist content-type raw-data))
           (study-id (assoc-ref data 'study-id))
           (datasets (datasets-by-study-id study-id)))
      (respond-200 output-port accept-type `((datasets . ,datasets))))]

   ;; OTHER
   ;; -------------------------------------------------------------------------
   [else
    (respond-500 output-port accept-type "Not OK")]))
