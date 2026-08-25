;;; writing.el --- Dependencies from the writing layer -*- no-byte-compile: t; -*-

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
	  :recipe (:local-repo "../../spacemacs/writing/local/write-or-die"))
(package! words
	  :recipe (:local-repo "../../spacemacs/writing/local/words"))
