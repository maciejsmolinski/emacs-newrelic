;;; newrelic.el --- An unofficial New Relic client package -*- lexical-binding: t -*-

;;; Copyright (C) 2021 Maciej Smolinski

;; Author: Maciej Smolinski <contact AT maciejsmolinski.com>
;; URL: https://github.com/maciejsmolinski/emacs-newrelic
;; Version: 0.0.1-pre
;; Package-Requires: ((emacs "24.4") (request "0.3.3"))
;; Keywords: tools newrelic monitoring observability

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

;; An unofficial New Relic client package.
;; Provides a set of commands for interacting with New Relic
;; through the official GraphQL API (api.newrelic.com/graphiql)

;;; Code:

;;;; Requirements

(require 'url)
(require 'browse-url)
(require 'newrelic-api)

;;;; Customization

(defgroup newrelic nil
  "New Relic customization group"
  :prefix "newrelic-"
  :group 'newrelic)

(defcustom newrelic-api-key nil
  "New Relic Query API Key."
  :type 'string
  :group 'newrelic)

(defvar newrelic-accounts-list nil
  "A list of New Relic account name and account id pairs.")

(defvar newrelic-active-account-id nil
  "An account being currently operated on.")

(defvar newrelic-dashboards-list nil
  "A list of New Relic dashboards.")

;;;; Commands

;;;###autoload
(defun newrelic-set-active-account ()
  (interactive)
  (when (newrelic--ensure-accounts-loaded)
    (let ((account-name (completing-read "Select account: " (seq-map 'car newrelic-accounts-list) nil t)))
      (setq newrelic-active-account-id (cadr (assoc account-name newrelic-accounts-list)))
      (message "Selected account %s with id %d" account-name newrelic-active-account-id))))

;;;###autoload
(defun newrelic-nrql-eval (nrql)
  (interactive "MNRQL: ")
  (when (newrelic--ensure-accounts-loaded)
    (call-interactively 'newrelic-set-active-account)
    (funcall 'newrelic-get-chart-link nrql)))

;;;###autoload
(defun newrelic-nrql-eval-line (pos)
  (interactive "d")
  (when (newrelic--ensure-accounts-loaded)
    (let ((nrql nil))
      (save-excursion
        (goto-char pos)
        (setq nrql (buffer-substring (line-beginning-position) (line-end-position))))
      (call-interactively 'newrelic-set-active-account)
      (funcall 'newrelic-get-chart-link nrql))))

;;;###autoload
(defun newrelic-nrql-eval-region (pos-min pos-max)
  (interactive "r")
  (when (newrelic--ensure-accounts-loaded)
    (let ((nrql (buffer-substring pos-min pos-max)))
      (call-interactively 'newrelic-set-active-account)
      (funcall 'newrelic-get-chart-link nrql))))

(defun newrelic-dashboards ()
  (interactive)
  (when (newrelic--ensure-dashboards-loaded)
    (let* ((dashboards (mapcar (lambda (item) (concat (let-alist item .name) " @@ " (let-alist item .guid))) newrelic-dashboards-list))
           (dashboard (completing-read "Select dashboard: " dashboards nil t))
           (selected-dashboard-name (car (split-string dashboard " @@ ")))
           (selected-dashboard-id (cadr (split-string dashboard " @@ "))))
      (message "Opening dashboard %s with id %s" selected-dashboard-name selected-dashboard-id)
      (browse-url (concat "https://one.newrelic.com/redirect/entity/" selected-dashboard-id)))))

;;;; Functions

;;;;;; Utils

(defun insert-image-from-url (url)
  (let ((image-path url)
        (image nil))
    (with-current-buffer (url-retrieve-synchronously image-path)
      (setq image (buffer-substring (1+ url-http-end-of-headers) (point-max))))
    (insert-image (create-image image nil t :height 300))))

;;;;; Private

(defun newrelic--ensure-accounts-loaded ()
  (cond
   ((not newrelic-api-key) (error "Error. `newrelic-api-key` is missing. Set one with (setq newrelic-api-key \"<your-api-key>\") to fix it"))
   ((not newrelic-accounts-list) (progn (message "Fetching accounts list") (newrelic-load-accounts)))
   (newrelic-accounts-list t)))

(defun newrelic--ensure-dashboards-loaded ()
  (cond
   ((not newrelic-api-key) (error "Error. `newrelic-api-key` is missing. Set one with (setq newrelic-api-key \"<your-api-key>\") to fix it"))
   ((not newrelic-dashboards-list) (progn (message "Fetching dashboards list") (newrelic-load-dashboards)))
   (newrelic-dashboards-list t)))

;;;; Footer

(provide 'newrelic)

;;; newrelic.el ends here
