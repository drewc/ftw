(in-package :ftw-persistence)

(defclass storage-node (node-with-unique-id
			node-with-creation-time)
  ((payload :accessor node-payload :initarg :payload)
   (properties :accessor node-properties :initarg :properties))
  (:metaclass proxied-node-class)
  (:index t))

(defclass storage-node-class (proxied-node-class) ())