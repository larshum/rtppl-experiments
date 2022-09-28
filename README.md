# RTPPL

To run the example program, you need to install Miking using the `rtppl` branch
of my fork ([here](https://github.com/larshum/miking/tree/rtppl)). Then just run
`make`.

The recorder program will store the time-stamped values in a buffer and output
them to files when the producer stops running. After running, the files
`trace-fs1.txt` and `trace-fs2.txt` are created, containing one time-stamped
value per line.
