(ns uk.axvr.void
  (:import (uk.axvr.void_ V)))

(defonce ^:const VOID V/VOID)

(defrecord Tmp [])
(instance? clojure.lang.IEditableCollection (->Tmp))
