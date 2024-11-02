(ns build
  (:require
   [clojure.tools.build.api :as b]))

(def lib 'io.github.johngorski/quilsteroids)
(def version (format "1.0.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))

;; delay to defer side effects (artifact downloads)
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (let [target "target"]
    (println "Deleting" target)
    (b/delete {:path target}))
  (println "SUCCESS: clean"))

(defn uber [_]
  (clean nil)
  (let [src-dirs ["src" "resources"]]
    (println (format "Copying %s to %s." src-dirs class-dir))
    (b/copy-dir {:src-dirs src-dirs
                 :target-dir class-dir}))
  (let [target-ns '[quilsteroids.game]]
    (println "Compiling" target-ns "to" class-dir)
    (b/compile-clj {:basis @basis
                    :ns-compile target-ns
                    :class-dir class-dir}))
  (println (format "Assembling classes in %s to uber-jar %s." class-dir uber-file))
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis @basis
           :main 'quilsteroids.game})
  (println "SUCCESS: uber"))
