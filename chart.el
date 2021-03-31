;; -*- lexical-binding: t -*-
;; (setf lexical-binding t)

(setq api-key "<API-KEY>")

(defun select-account (account-id)
  "Prints the given ACCOUNT-ID in the current buffer."
  (interactive "nAccount ID: ")
  (let ((account-id-as-string (number-to-string account-id)))
    (message account-id-as-string)
    (save-excursion
      (insert account-id-as-string))))

(setq accounts '(("First" 1) ("Second" 2)))

(defun select-account-2 (account-name)
  (interactive (list (completing-read "Select account: " accounts nil t)))
  (message "Account %s with id %d" account-name (cadr (assoc account-name accounts))))

(use-package request)
(use-package url)

(defun on-success (data)
  (message "I received: %S" data))

(defun graphql (endpoint query variables success-handler &optional headers)
  (request
    endpoint
    :type "POST"
    :headers (append '(("Content-Type" . "application/json")) headers)
    :data (json-encode query)
    :parser 'json-read
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall success-handler data)))
    :error (cl-function (lambda (&key error-thrown data &allow-other-keys) (message (format "%s %s" error-thrown data))))))

(graphql
 "https://stormy.maciejsmolinski.com/graphql"
 '(("query" . "{ sessions { id slug } }"))
 nil
 'on-success)

(defun on-success-handler (data)
  (let* ((accounts-data (let-alist data .data.actor.accounts))
         (account-items (seq-map (lambda (a) (list (cdadr a) (cdar a))) accounts-data)))
    (setq accounts account-items)
    (call-interactively 'select-account-2)))

(graphql
 "https://api.newrelic.com/graphql"
 '(("query" . "{ actor { accounts { id name } } }"))
 nil
 'on-success-handler
 (list `("Api-Key" . ,api-key)))

(let ((image-path "https://gorgon.nr-assets.net/image/02e53da9-e9d8-4f3b-aa7a-30438111d898?type=line")
      (image nil))
  (with-current-buffer
      (url-retrieve-synchronously image-path)
    (setq image (buffer-substring (1+ url-http-end-of-headers) (point-max))))
  (insert-image (create-image image nil t :height 300)))
