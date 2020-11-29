```
(newgl:display (make-instance 'newgl.fftanim:spirograph :mp3-file "/home/jeremiah/music/Stilz/Judicator/Stilz - Judicator - 03 Electric Nights.mp3" :steps 511))
(newgl:display (make-instance 'newgl.fftanim:spirograph
                                       :steps 511
                                       :a-var (anim-utils:make-animated-var :val 13.0 :buckets '(6 7 8 9 10))
                                       :b-var (anim-utils:make-animated-var :val 5.0 :buckets '(7 9 11 13))
                                       :h-var  (anim-utils:make-animated-var :val 8.0 :buckets '(1 2 3 6 7))
                                       :stype :epitrochoid
                                       :mp3-file "/home/jeremiah/music/Automat/Automat/01 - Automat (Remastered).mp3"
                                       ))
```
