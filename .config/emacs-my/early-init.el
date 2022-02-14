(setq package-enable-at-startup nil)
(when (fboundp 'native-compile-async)
    (setq comp-deferred-compilation t
          comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))
