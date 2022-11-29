## 0.1.3.1

- Update to accomodate changes from latest process library. (`OpenExtHandle`).
- Improve documentation related to timeout.

## 0.1.3.0

- By default pid1 will first send the TERM signal to it's "immediate
child" process.  In most scenarios that will be the only process
running but in some cases that will be the "main" process that could
have spawned it's own children. In this scenario it's prudent to
shutdown the "main" process first, since usually it has mechanisms in
place to shut down it's children. If we were to shutdown a child
process before "main" was shutdown it might try to restart it.  This
is why, if the "main" process doesn't exit within `timeout` we will
proceed to send the TERM signal to all processes and wait **again**
for `timeout` until we finally send the KILL signal to all
processes. This is a **breaking change in 0.1.3.0**.

## 0.1.2.0

* Removes support for ',' separated list of environment variables
  for `-e` command line option
* Adds support for setting child processes wait timeout on SIGTERM or SIGINT

## 0.1.1.0

* Adds support for setuid and setguid when running command
* Adds support for setting current directory when running command

## 0.1.0.1

* Turn off all RTS options

## 0.1.0.0

* Initial release
