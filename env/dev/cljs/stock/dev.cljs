(ns ^:figwheel-no-load stock.dev
  (:require
    [stock.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
