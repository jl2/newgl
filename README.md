# newgl
### _Jeremiah LaRocco <jeremiah_larocco@fastmail.com>_

New OpenGL wrapper library.


## Examples
```
(ql:quickload :newgl)
(newgl:viewer :objects
              (newgl:make-mandelbrot :window (make-instance 'newgl:complex-window
                                                            :imag-min -0.4284997f0
                                                            :imag-max -0.42617327f0
                                                            :real-min -1.2854176f0
                                                            :real-max -1.2827007f0)))
(newgl:viewer :objects (newgl:make-mandelbrot :window #(-1.0f0 1.0f0 0.0f0 -0.5365814f0 -0.66618943f0 -1.0f0 -1.0f0 0.0f0
-0.5365814f0 -0.6661973f0 1.0f0 1.0f0 0.0f0 -0.5365724f0 -0.66618943f0 1.0f0
  -1.0f0 0.0f0 -0.5365724f0 -0.6661973f0)))

(newgl:viewer :objects (newgl:make-mandelbrot :window #(-1.0f0 1.0f0 0.0f0 -1.1581888f0 -0.30270645f0 -1.0f0 -1.0f0 0.0f0
  -1.1581888f0 -0.3111232f0 1.0f0 1.0f0 0.0f0 -1.1485512f0 -0.30270645f0 1.0f0
  -1.0f0 0.0f0 -1.1485512f0 -0.3111232f0)))

(newgl:viewer :objects (newgl:make-mandelbrot :window #(-1.0f0 1.0f0 0.0f0 -1.1742291f0 -0.23033905f0 -1.0f0 -1.0f0 0.0f0
  -1.1742291f0 -0.2305259f0 1.0f0 1.0f0 0.0f0 -1.1740147f0 -0.23033905f0 1.0f0
  -1.0f0 0.0f0 -1.1740147f0 -0.2305259f0)))

(newgl:viewer :objects (newgl:make-mandelbrot :window #(-1.0f0 1.0f0 0.0f0 -0.5717396f0 0.56185037f0 -1.0f0 -1.0f0 0.0f0 -0.5717396f0
  0.5618009f0 1.0f0 1.0f0 0.0f0 -0.5716815f0 0.56185037f0 1.0f0 -1.0f0 0.0f0
  -0.5716815f0 0.5618009f0)))

(newgl:viewer :objects (newgl:make-mandelbrot :window #(-1.0f0 1.0f0 0.0f0 -0.57171535f0 0.5618296f0 -1.0f0 -1.0f0 0.0f0
  -0.57171535f0 0.56182176f0 1.0f0 1.0f0 0.0f0 -0.57170606f0 0.5618296f0 1.0f0
  -1.0f0 0.0f0 -0.57170606f0 0.56182176f0)))

(newgl:viewer :objects (newgl:make-mandelbrot :window #(-1.0f0 1.0f0 0.0f0
                                                        -0.019941114f0
                                                        -0.80132824f0 -1.0f0
                                                        -1.0f0 0.0f0
                                                        -0.019941114f0
                                                        -0.8014834f0 1.0f0
                                                        1.0f0 0.0f0
                                                        -0.019766055f0
                                                        -0.80132824f0 1.0f0
                                                        -1.0f0 0.0f0
                                                        -0.019766055f0
                                                        -0.8014834f0)))
```

## License

ISC


Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


