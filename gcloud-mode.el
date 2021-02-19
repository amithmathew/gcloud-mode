;;; gcloud-mode.el --- A minor mode to modify gcloud configuration in emacs. -*- coding: utf-8-unix

;; Author: Amith Mathew <me@amithm.ca>
;; URL: https://github.com/amithmathew/gcloud-mode
;; Version: 0.1
;; Keywords: gcp, gcloud, google

;;; Commentary:

;;; INTRODUCTION
;;
;; Using tramp to edit files on Cloud-Shell or on GCE instances, requires that
;; the right gcloud configuration is selected. This includes making sure that
;; the right authorization, project and region are selected.
;; This minor mode provides three things to help with this.
;;   1. A modeline entry showing currently active configuration including
;;        the active project, account/auth and configuration.
;;   2. Keybinding and function to allow selecting another configuration or
;;        project from available configurations/projects, including support
;;        for auto-complete.
;;   3. Tramp methods to make connecting to cloud-shell and GCE instances
;;        through `gcloud compute ssh` easier.

;;; USAGE
;;
;; After installation, this mode binds C-c C-g to the function gcloud-activate
;; Upon invocation, this provides a list of accessible configurations and
;; projects to select from.
;;  - Configurations take the form `config:<config_name>`.
;;  - Projects take the form `project:<project_name>|<project_id>`. This is
;;      required because project-names, while human-friendly, are not unique.
;;      and you may want to look at the project-id to disambiguate.
;;
;; If you ARE using gcloud configurations to manage multiple configurations,
;; be aware that setting a project value will update the currently active
;; configuration. This is how the gcloud command works, and is expected
;; behaviour.



;;; Code:


(defgroup gcloud-mode nil
  "Setup functionality for the gcloud minor mode."
  :group 'external
  :tag "GCloud Mode")

(defcustom gcloud-enable-all-the-icons '("[ ]" "[x]")
  "Enable all-the-icons support. Use only if all-the-icons is installed."
  :group 'gcloud-mode
  :type '(repeat string))

(defvar gcloud-active-config "None")
(defvar gcloud-active-projectid "None")
(defvar gcloud-active-account "None")
(defvar gcloud-active-region "None")
(defvar gcloud-all-the-icons nil)
(defvar gcloud-mode-active nil)
(defvar gcloud-modeline-format " [GCP:%s(%s,%s)]")
(defvar gcloud-modeline-match-regex "\s\\[.*:.*\\(.*,.*\\)\\]")
(defvar gcloud-modeline-string)

(defun gcloud-add-tramp-methods ()
  "Add gcloud tramp methods if they don't already exist."
  (if (not (member "cloudshell" (mapcar 'car tramp-methods)))
    (push
     (cons
      "cloudshell"
      '((tramp-login-program "gcloud")
        (tramp-login-args (("alpha" "cloud-shell" "ssh")))
        (tramp-remote-shell "/bin/sh")
        (tramp-remote-shell-args ("-i") ("-c"))))
     tramp-methods))
  (if (not (member "gssh" (mapcar 'car tramp-methods)))
    (push
     (cons
      "gssh"
      '((tramp-login-program "gcloud")
        (tramp-login-args (("compute" "ssh" "%h")))
        (tramp-remote-shell "/bin/sh")
        (tramp-remote-shell-args ("-i") ("-c"))))
     tramp-methods)))

(defun gcloud-run-command (&rest args)
  "Run arbitrary gcloud commands."
  (with-temp-buffer
    (apply 'call-process "gcloud" nil t nil args)
    (buffer-string)))

(defun gcloud-get-active-configuration ()
    "Get current gcloud configuration and returns a hash table with values."
    (let* ((active-configuration (gcloud-run-command "config"
                                                     "configurations"
                                                     "list"
                                                     "--filter=is_active=True" "--format=json(name,properties.core.account,properties.core.project,properties.compute.region)"))
           (result (aref (json-parse-string active-configuration :object-type 'alist :null-object 'None) 0)))
      ;;(message "%s" active-configuration)
      (setq gcloud-active-config (cdr (assoc 'name result)))
      (setq gcloud-active-projectid (cdr (assoc 'project (assoc 'core (assoc 'properties result)))))
      (setq gcloud-active-region (cdr (assoc 'region (assoc 'compute (assoc 'properties result)))))
      (setq gcloud-active-account (cdr (assoc 'account (assoc 'core (assoc 'properties result)))))))

(defun gcloud-get-configurations ()
  "Gets list of existing configurations."
  (split-string (gcloud-run-command "config" "configurations" "list" "--format=value(name)") "\n" t))


(defun gcloud-get-projects-and-projectids (&optional limit)
  "Returns a simple list of alists with project name and project id."
  (if limit
      (append (json-parse-string (gcloud-run-command "projects" "list" "--format=json(name,projectId)" "--limit" (number-to-string limit)) :object-type 'alist)) nil)
  (append (json-parse-string (gcloud-run-command "projects" "list" "--format=json(name,projectId)") :object-type 'alist) nil))

(defun gcloud-project-config-completion-list ()
  "Builds a list of configurations and projects for selection"
  (let* ((configlist (mapcar (lambda (x)
                              (concat "config:" x))
                            (gcloud-get-configurations)))
        (projects (gcloud-get-projects-and-projectids))
        (projectlist (mapcar (lambda(x)
                               (concat "project:"
                                       (cdr (assoc 'name x))
                                       "|"
                                       (cdr (assoc 'projectId x))))
                             projects)))
    (append configlist projectlist)))

(defun gcloud-set-project (projectid)
  (interactive "sProject: ")
  (message (gcloud-run-command "config" "set" "core/project" projectid)))

(defun gcloud-activate-configuration (config)
  (interactive "sConfiguration: ")
  (message (gcloud-run-command "config" "configurations" "activate" config)))

(defun gcloud-activate (userselection)
  (interactive
   (list (completing-read "Project or configuration: "
                          (gcloud-project-config-completion-list)
                          )))
  (let* ((name (nth 1 (split-string userselection ":")))
         (type (car (split-string userselection ":")))
         (projectid (nth 1 (split-string name "|"))))
    (cond
     ((equal type "config") (gcloud-activate-configuration name))
     ((equal type "project") (gcloud-set-project projectid))
     (message type))
    (gcloud-get-active-configuration)
    (gcloud-modeline-update)))

;;;; Modeline
(defun gcloud-modeline-update ()
  "Update modeline with GCP information"
  (setq gcloud-modeline-string
        (format gcloud-modeline-format
                gcloud-active-projectid
                gcloud-active-account
                gcloud-active-config))
  (concat mode-line-misc-info gcloud-modeline-string))

(defun gcloud-modeline-delete()
  "Remove modeline information when mode is deactivated"
  (setq mode-line-misc-info (replace-regexp-in-string gcloud-modeline-match-regex "" mode-line-misc-info)))


(defun gcloud-modeline-all-the-icons ()
  "Uses all-the-icons if enabled."
  (if gcloud-all-the-icons
      (setq gcloud-modeline-format (replace-regexp-in-string "\s\\[GCP:"
                                                             (format " [%s:"
                                                                     (all-the-icons-faicon "cloud" :height 0.75 :v-adjust 0))
                                                             gcloud-modeline-format))))

;;;; Keybindings
(defvar gcloud-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-g") 'gcloud-activate)
    map))


;;;###autoload
(define-minor-mode gcloud-mode
  "Minor mode to help with gcloud defaults like current project and active configuration."
  :global t
  :lighter " gcp"
  :keymap gcloud-mode-map
  (if (not gcloud-mode-active)
      ;; gcloud-mode isn't enabled, enable it.
      (progn
        (message "Enabling gcloud mode...")
        (setq gcloud-mode-active t)
        (gcloud-get-active-configuration)
        (gcloud-add-tramp-methods)
        (gcloud-modeline-all-the-icons)
        (gcloud-modeline-update)
        (message "Enabled gcloud mode."))
    ;; gcloud-mode is enabled, disable it.
    (progn
      (message "Disabling gcloud mode...")
      (gcloud-modeline-delete)
      (setq gcloud-mode-active nil)
      (message "Disabled gcloud mode."))))

(provide 'gcloud-mode)
;;; gcloud-mode.el ends here.
