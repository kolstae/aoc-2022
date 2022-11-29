(ns user)

(try ((requiring-resolve 'pjstadig.humane-test-output/activate!))
     (catch Exception _
       (println "Could not activate humane-test-output :(")))

(comment

 ((requiring-resolve 'flow-storm.api/local-connect) {:theme :dark})

 )
