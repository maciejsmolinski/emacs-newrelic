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

;;; Code:

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
        nrql
        staticChartUrl
      }
    }
  }
}
")

(defconst newrelic-gql-get-accounts "
query GetAccounts {
  actor {
    accounts {
      id
      name
    }
  }
}
")

;;;; Variables

(setq newrelic-api-key nil)
(setq newrelic-accounts-list nil)
(setq newrelic-active-account-id nil)

;;;; Commands

;;;###autoload
(defun newrelic-nrql-chart (nrql)
  (interactive "MNRQL: ")
  (when (newrelic--ensure-accounts-loaded)
    (call-interactively 'newrelic-set-active-account)
    (funcall 'newrelic-get-chart-link nrql)))

;;;###autoload
(defun newrelic-set-active-account (account-name)
  (interactive (list (completing-read "Select account: " (seq-map 'car newrelic-accounts-list) nil t)))
  (when (newrelic--ensure-accounts-loaded)
    (setq newrelic-active-account-id (cadr (assoc account-name newrelic-accounts-list)))
    (message "Selected account %s with id %d" account-name newrelic-active-account-id)))

;;;###autoload
(defun newrelic-refresh-accounts-list ()
    (interactive)
  (when (newrelic--ensure-accounts-loaded)
    (message "Accounts list refreshed")))

;;;; Functions

;;;;; Public

;;;;;; API Calls

(defun newrelic-get-chart-link (nrql)
  (newrelic-query
   `(:query ,newrelic-gql-get-chart-link
     :variables ,`(:accountId ,newrelic-active-account-id :nrql ,nrql))
   'newrelic--get-chart-link-callback))

(defun newrelic--get-chart-link-callback (data)
  (let* ((chartLink (let-alist data .data.actor.account.nrql.staticChartUrl))
         (nrql (let-alist data .data.actor.account.nrql.nrql))
         (buffer (get-buffer-create (concat "* NRQL @ " (number-to-string newrelic-active-account-id) " *"))))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (save-excursion
        (insert nrql "\n")
        (insert-image-from-url chartLink)))))

(defun newrelic-load-accounts ()
  (newrelic-query
   `(:query ,newrelic-gql-get-accounts)
   'newrelic--get-accounts-callback))

(defun newrelic--get-accounts-callback (data)
  (let* ((accounts-data (let-alist data .data.actor.accounts))
         (account-items (seq-map (lambda (a) (list (cdadr a) (cdar a))) accounts-data)))
    (setq newrelic-accounts-list account-items)))

;;;;;; Utils

(defun newrelic-query (payload on-success)
  (newrelic-graphql
   "https://api.newrelic.com/graphql"
   payload
   on-success
   `(:headers (("Api-Key" . ,newrelic-api-key)))))

(defun newrelic-graphql (endpoint payload success-handler &optional options)
  (request
    endpoint
    :type "POST"
    :headers (append '(("Content-Type" . "application/json")) (plist-get options :headers))
    :data (json-encode payload)
    :parser 'json-read
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall success-handler data)))
    :error (cl-function (lambda (&key error-thrown data &allow-other-keys) (message (format "%s %s" error-thrown data))))))

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

;;;;; Private

(defun newrelic--ensure-accounts-loaded ()
  (cond
   ((not newrelic-api-key) (progn (message "Error. `newrelic-api-key` is missing. Set one with (setq newrelic-api-key \"<your-api-key>\") to fix it.") nil))
   ((not newrelic-accounts-list) (progn (message "Fetching accounts list") (newrelic-load-accounts)))
   (newrelic-accounts-list t)))

;;;; Footer

(provide 'newrelic)

;;; newrelic.el ends here
