;;; workon.el --- work with conda environments       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  keewis

;; Author: Keewis <keewis@posteo.de>
;; Keywords: tools, convenience, languages

;;; Commentary:

;;

;;; Code:

(require 'conda)
(require 'f)
(require 's)

(defvar workon-project-path-name ".workon_path"
  "The filename used to save the project path.")

(defun workon-cd--project-path ()
  "Construct the path of the current environment's project path file."
  (if conda-env-current-path
      (f-join conda-env-current-path workon-project-path-name)))

(defun workon-cd--project ()
  "Read the project path of the current environment."
  (let ((project-path (workon-cd--project-path)))
    (if (f-file? project-path)
        (f-full (s-trim-right (f-read-text project-path))))))

(defun workon-set-project ()
  "Set the current directory as the current environment's project path."
  (interactive)
  (let ((cwd default-directory)
        (project-path (workon-cd--project-path)))
    (f-write-text (f-full cwd) coding-category-utf-8 project-path)))

(defun workon-cd (type)
  "Switch to the directory indicated by TYPE.

Can be either 'env' (switch to the environment directory) or
'project' (switch to the project directory)."
  (interactive (list (completing-read "Switch to: " (list "env" "project"))))
  (if (not conda-env-current-name)
      (error "workon-cd: no active conda environment"))
  (let ((path (cond ((s-equals? type "env")
                     conda-env-current-path)
                    ((s-equals? type "project")
                     (workon-cd--project)))))
    (message "workon-cd: switching to %s" path)
    (if path (setq default-directory path))))


(defun workon (environment)
  "Activate ENVIRONMENT and change to the project path."
  (interactive (list (completing-read "Activate environment: "
                                      (conda-env-candidates))))
  (conda-env-activate environment)
  (workon-cd "project"))


(provide 'workon)
;;; workon.el ends here
