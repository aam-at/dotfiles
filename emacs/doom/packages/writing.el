;;; writing.el --- Writing and prose dependencies -*- no-byte-compile: t; -*-

(package! flycheck-vale)
(package! writeroom-mode)
(package! jinx)
(package! llama)
(package! powerthesaurus)
(package! le-thesaurus)
(package! mw-thesaurus
	  :recipe (:host github :repo "agzam/mw-thesaurus.el"))
(package! synosaurus)
(package! academic-phrases)
(package! write-or-die
	  :recipe (list :local-repo
			(expand-file-name "../local/write-or-die"
			                  (file-truename doom-user-dir))))
(package! words
	  :recipe (list :local-repo
			(expand-file-name "../local/words"
			                  (file-truename doom-user-dir))))
