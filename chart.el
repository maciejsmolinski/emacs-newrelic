;; -*- lexical-binding: t -*-
;; (setf lexical-binding t)

(defun select-account (account-id)
  "Prints the given ACCOUNT-ID in the current buffer."
  (interactive "nAccount ID: ")
  (let ((account-id-as-string (number-to-string account-id)))
    (message account-id-as-string)
    (save-excursion
      (insert account-id-as-string))))

;; (call-interactively 'select-account)

(use-package request)
(use-package url)

(defun on-success (data)
  (message "I received: %S" data))

(defun graphql (endpoint query variables success-handler)
  (request
    endpoint
    :type "POST"
    :headers '(("Content-Type" . "application/json"))
    :data (json-encode query)
    :parser 'json-read
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall success-handler data)))))

(let ((image-path "https://gorgon.nr-assets.net/image/02e53da9-e9d8-4f3b-aa7a-30438111d898?type=line")
      (image nil))
  (with-current-buffer
      (url-retrieve-synchronously image-path)
    (setq image (buffer-substring (1+ url-http-end-of-headers) (point-max))))
  (insert-image (create-image image nil t :height 300)))

(graphql
 "https://stormy.maciejsmolinski.com/graphql"
 '(("query" . "{ sessions { id } }"))
 nil
 'on-success)
