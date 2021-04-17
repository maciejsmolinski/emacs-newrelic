;;; newrelic-api.el --- Utils for interacting with New Relic APIs -*- lexical-binding: t -*-

;;; Copyright (C) 2021 Maciej Smolinski

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

;; A set of functions responsible for interacting with New Relic
;; through the official GraphQL API (api.newrelic.com/graphiql)

;;; Code:

;;;; Requirements

(require 'request)

;;;; Variables

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
" "A GraphQL Query used by the client to get a link to the chart image for a given query.")

(defconst newrelic-gql-get-accounts "
query GetAccounts {
  actor {
    accounts {
      id
      name
    }
  }
}
" "A GraphQL Query used by the client to get a list of account name and id pairs.")

(defconst newrelic-gql-get-dashboards "
query GetDashboards {
  actor {
    entitySearch(queryBuilder: {type: DASHBOARD}) {
      results {
        entities {
          ... on DashboardEntityOutline {
            name
            accountId
            guid
            dashboardParentGuid
          }
        }
      }
    }
  }
}
" "A GraphQL Query used by the client to get a list of dashboards including their name, guid and other details.")

;;;; Functions

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
      (goto-char (point-min))
      (save-excursion
        (insert "\n" nrql "\n\n")
        (insert-image-from-url chartLink)
        (insert "\n")))))

(defun newrelic-load-accounts ()
  (newrelic-query
   `(:query ,newrelic-gql-get-accounts)
   'newrelic--get-accounts-callback))

(defun newrelic--get-accounts-callback (data)
  (let* ((accounts-data (let-alist data .data.actor.accounts))
         (account-items (seq-map (lambda (a) (list (cdadr a) (cdar a))) accounts-data)))
    (setq newrelic-accounts-list account-items)))

(defun newrelic-load-dashboards ()
  (newrelic-query
   `(:query ,newrelic-gql-get-dashboards)
   'newrelic--load-dashboards-callback))

(defun newrelic--load-dashboards-callback (data)
  (let ((dashboards-data (let-alist data .data.actor.entitySearch.results.entities)))
    (setq newrelic-dashboards-list dashboards-data)))


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
    :error (cl-function (lambda (&key error-thrown data &allow-other-keys) (message (format "%s %s" error-thrown data))))
    :sync t))


(provide 'newrelic-api)

;;; newrelic-api.el ends here
