(hush +)
(load "library.shen")

(define module-load.features-available?
  [] _ -> true
  [Feature | Features] Available
    -> (and (element? Feature Available)
            (module-load.features-available? Features Available)))

(define module-load.report
  Format Value
    -> (do (hush -)
           (output Format Value)
           (hush +)))

(define module-load.main
  [_ Name]
    -> (let Module (intern Name)
            Declaration (library.read-module Module)
            Required (library.module-required-features Declaration)
            Available (library.current-features)
         (if (module-load.features-available? Required Available)
             (do (library.use [Module])
                 (module-load.report "[OK] independently loaded ~A~%" Module))
             (module-load.report "[SKIP] unavailable features for ~A~%" Module)))
  Args -> (do (hush -)
              (error "expected one module name, got: ~R~%" Args)))

(module-load.main (value *argv*))
