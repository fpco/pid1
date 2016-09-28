## pid1

Do signal handling and orphan reaping for Unix PID1 init processes.

This provides a Haskell library, and an executable based on that library, for
initializing signal handlers, spawning and child process, and reaping orphan
processes. These are the responsibilities that must be fulfilled by the initial
process in a Unix system, and in particular comes up when running Docker
containers.

This library/executable will automatically detect if it is run as some process
besides PID1 and, if so, use a straightforward `exec` system call instead.

__NOTE__ This package is decidedly _not_ portable, and will not work on
Windows. If you have a use case where you think it makes sense to run on
Windows, I'd be interested in hearing about it.

Blog post on some of the finer points here expected in the future.

### Usage

The recommended use case for this executable is to embed it in a Docker image.
Assuming you've placed it at `/sbin/pid1`, the two commonly recommended usages
are:

1. Override the entrypoint, either via `ENTRYPOINT` in your Dockerfile or
   `--entrypoint` on the command line.

   ```
   docker run --rm --entrypoint /sbin/pid1 fpco/pid1 ps
   ```

2. Add `/sbin/pid1` to the beginning of your command.

   ```
   docker run --rm --entrypoint /usr/bin/env fpco/pid1 /sbin/pid1 ps
   ```
