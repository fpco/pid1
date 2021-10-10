## pid1

[![Build Status](https://travis-ci.org/fpco/pid1.svg?branch=master)](https://travis-ci.org/fpco/pid1)

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

For a discussion on why this is useful, see [this
repo](https://github.com/snoyberg/docker-testing#readme).

### Usage

> pid1 [-e|--env ENV] [-u|--user USER] [-g|--group GROUP] [-w|--workdir DIR] [-t|--timeout TIMEOUT] COMMAND [ARG1 ARG2 ... ARGN]

Where:
* `-e`, `--env` `ENV` - Override environment variable from given name=value
  pair. Can be specified multiple times to set multiple environment variables.
* `-u`, `--user` `USER` - The username the process will setuid before executing
  COMMAND
* `-g`, `--group` `GROUP` - The group name the process will setgid before
  executing COMMAND
* `-w`, `--workdir` `DIR` - chdir to `DIR` before executing COMMAND
* `-t`, `--timeout` `TIMEOUT` - timeout (in seconds) to wait for all child processes to exit

`WARNING`: by default pid1 will first send the TERM signal to it's "immediate child" process.
In most scenarios that will be the only process running but in some cases that will be the
"main" process that could have spawned it's own children. In this scenario it's prudent to shutdown
the "main" process first, since usually it has mechanisms in place to shut down it's children. If
we were to shutdown a child process before "main" was shutdown it might try to restart it.
This is why, if the "main" process doesn't exit within `timeout` we will proceed to send the TERM
signal to all processes and wait **again** for `timeout` until we finally send the KILL signal to all
processes. This is a **breaking change since 0.1.3.0**.

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
