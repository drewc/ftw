(asdf:defsystem :ftw-persistence
  :license 
  "Copyright (c) 2009 Drew Crampsie ALL RIGHTS RESERVED"
  :components ((:module :persistence
		:components (		
			    (:file "packages")
			    (:file "rucksack-hacks")
			    (:file "utils")
			    (:file "rucksack")
			    (:file "id-sequence")
			    (:file "user")
			    (:file "node-class")
			    (:file "node")
			    (:file "proxy-node")
			    (:file "modification-context")
			   ; (:file "versioned-node")
			   ; (:file "standard-nodes")
			   ; (:file "parent-node")
			   ; (:file "root-node")
			   ; (:file "document-node")
			    )
		:serial t))
  :depends-on (:closer-mop :rucksack :alexandria))




