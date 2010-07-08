(defpackage :ftw-persistence
  (:use :cl :rucksack :alexandria)
  (:export 
   #:add-class-to-rucksack
   ;;; Users
   #:create-user
   #:find-user
   #:user-name
   #:user-password
   #:user-email
   #:merge-user-rucksack-pathname
   #:merge-rucksack-pathname

   ;;; User Rucksacks
   #:funcall-with-user-rucksack
   #:with-user-rucksack
   
   ;;; Nodes
   #:node
   #:node-class
   #:node-id
   #:node-with-unique-id
   #:node-with-creation-time
   #:find-node
   #:find-node-in-rucksack

   ;;; Versioned Nodes
   #:create-versioned-node
   #:versioned-node-current-version
   #:funcall-with-node-version
   #:with-node-version

   ;;; Modification Context
   #:with-modification-context
   #:funcall-with-modification-context

   ;;; Storage Node types
   #:storage-node
   #:node-payload
   #:node-properties

   ;;; Parent Nodes
   #:create-parent-node
   #:node-children
   #:find-child-node
   #:insert-child-nodes
   
   #:storage-node-class
   #:versioned-node
   #:versioned-node-class
   #:make-versioned-node
   #:initialize-rucksack
   #:node-creation-time))