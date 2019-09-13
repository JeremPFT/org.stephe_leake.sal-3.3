;; Project definitions

(require 'ada-project)
(require 'project-menu)
(require 'xref-ada)

(add-to-list 'project-find-functions 'project-menu-prj)

(let* ((prj-file (expand-file-name "sal.prj"))
       (prj-name "sal")
       (prj (make-ada-project
	     :env-vars nil
	     :ada-prj-file prj-file)))

  (project-menu-add-project prj prj-name default-directory)

  (project-menu-select-by-name prj-name)
  )
;; end of file
