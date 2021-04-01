;; -*- lexical-binding: t -*-
;; (setf lexical-binding t)

(use-package request)
(use-package url)

(setq api-key "<API-KEY>")
(setq accounts '(("No accounts available" 0)))
(setq active-account-id 0)

(defun select-account (account-name)
  (interactive (list (completing-read "Select account: " (seq-map 'car accounts) nil t)))
  (setq active-account-id (cadr (assoc account-name accounts)))
  (message "Account %s with id %d" account-name active-account-id))

(defun graphql (endpoint payload success-handler &optional headers)
  (request
    endpoint
    :type "POST"
    :headers (append '(("Content-Type" . "application/json")) headers)
    :data (json-encode payload)
    :parser 'json-read
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall success-handler data)))
    :error (cl-function (lambda (&key error-thrown data &allow-other-keys) (message (format "%s %s" error-thrown data))))))

(defun on-success-handler (data)
  (let* ((accounts-data (let-alist data .data.actor.accounts))
         (account-items (seq-map (lambda (a) (list (cdadr a) (cdar a))) accounts-data)))
    (setq accounts account-items)
    (call-interactively 'select-account)))

(defun insert-image-from-url (url)
  (let ((image-path url)
        (image nil))
    (with-current-buffer
        (url-retrieve-synchronously image-path)
      (setq image (buffer-substring (1+ url-http-end-of-headers) (point-max))))
    (save-excursion
      (end-of-line)
      (insert "\n")
      (insert-image (create-image image nil t :height 300)))))

(defun get-chart-link (nrql)
  (graphql
   "https://api.newrelic.com/graphql"
   `(
     :query "
       query GetChartLink($accountId: Int!, $nrql: Nrql!) {
         actor {
           account(id: $accountId) {
             nrql(query: $nrql) {
               staticChartUrl
             }
           }
         }
       }
     "
     :variables ,`(:accountId ,active-account-id :nrql ,nrql))
   (lambda (data) (let ((chartLink (let-alist data .data.actor.account.nrql.staticChartUrl))) (insert-image-from-url (kill-new chartLink))))
   (list `("Api-Key" . ,api-key))))

(defun open-select-account ()
  (graphql
   "https://api.newrelic.com/graphql"
   '(("query" . "{ actor { accounts { id name } } }"))
   'on-success-handler
   (list `("Api-Key" . ,api-key))))

;; (get-chart)
;; (open-select-account)
(get-chart-link "SELECT * FROM SyntheticCheck")
(get-chart-link "SELECT count(*) FROM SyntheticCheck TIMESERIES")
