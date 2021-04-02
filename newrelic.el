;;; newrelic.el --- An unoffical New Relic package -*- lexical-binding: t -*-

;;; Copyright (C) 2021 Maciej Smolinski

;; Author: Maciej Smolinski <contact AT maciejsmolinski.com>
;; URL: https://github.com/maciejsmolinski/emacs-newrelic
;; Version: 0.0.1-pre
;; Package-Requires: ((emacs "27.1"))
;; Keywords: newrelic monitoring observability

;; This file is not part of GNU Emacs.

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

;;; Provides a set of commands for interacting with New Relic
;;; through the official GraphQL API (api.newrelic.com/graphiql)

;;; Code

;;;; Requirements

(require 'request)
(require 'url)

;;;; Customization

(defgroup newrelic nil
  "New Relic customization group"
  :prefix "newrelic-"
  :group 'newrelic)

;;;; Constants

(defconst newrelic-gql-get-chart-link "
query GetChartLink($accountId: Int!, $nrql: Nrql!) {
  actor {
    account(id: $accountId) {
      nrql(query: $nrql) {
        staticChartUrl
      }
    }
  }
}
")

;;;; Variables

(setq api-key "<API-KEY>")
(setq accounts '(("No accounts available" 0)))
(setq active-account-id 0)

(defun graphql (endpoint payload success-handler &optional headers)
  (request
    endpoint
    :type "POST"
    :headers (append '(("Content-Type" . "application/json")) headers)
    :data (json-encode payload)
    :parser 'json-read
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall success-handler data)))
    :error (cl-function (lambda (&key error-thrown data &allow-other-keys) (message (format "%s %s" error-thrown data))))))

(defun select-account-prompt (account-name)
  (interactive (list (completing-read "Select account: " (seq-map 'car accounts) nil t)))
  (setq active-account-id (cadr (assoc account-name accounts)))
  (message "Account %s with id %d" account-name active-account-id))

(defun on-success-handler (data)
  (let* ((accounts-data (let-alist data .data.actor.accounts))
         (account-items (seq-map (lambda (a) (list (cdadr a) (cdar a))) accounts-data)))
    (setq accounts account-items)
    (call-interactively 'select-account-prompt)))

(defun select-account ()
  (graphql
   "https://api.newrelic.com/graphql"
   '(("query" . "{ actor { accounts { id name } } }"))
   'on-success-handler
   (list `("Api-Key" . ,api-key))))

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

(defun nrql-chart (nrql)
  (interactive "MNRQL: ")
  (graphql
   "https://api.newrelic.com/graphql"
   `(
     :query ,newrelic-gql-get-chart-link
     :variables ,`(:accountId ,active-account-id :nrql ,nrql))
   (lambda (data)
     (let ((chartLink (let-alist data .data.actor.account.nrql.staticChartUrl)))
       (insert-image-from-url chartLink)))
   (list `("Api-Key" . ,api-key))))

;; (open-select-account)
;; (nrql-chart "SELECT * FROM SyntheticCheck")
;; (nrql-chart "SELECT count(*) FROM SyntheticCheck TIMESERIES")
