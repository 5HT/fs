FS Listener
===========

Backends
--------

* Mac [fsevent](https://github.com/thibaudgg/rb-fsevent)
* Linux [fanotify](https://launchpad.net/fatrace) and 
        [inotify](https://github.com/rvoicilas/inotify-tools/wiki)
* Windows [changenotifications]

The application relies on the appropriate binaries to be in your `$PATH`.
The monitoring path is read from the application configuration (variable `path`).
Once the `FS` app is started you may use the simple API:

```erlang
> fs:subscribe(). % the pid will receive events as messages
> flush(). 
Shell got {<0.47.0>,
           {fs,file_event},
           {"/Users/5HT/synrc/fs/src/README.md",[closed,modified]}}

> fs:known_events(). % returns events known by your current backend
[mustscansubdirs,userdropped,kerneldropped,eventidswrapped,
 historydone,rootchanged,mount,unmount,created,removed,
 inodemetamod,renamed,modified,finderinfomod,changeowner,
 xattrmod,isfile,isdir,issymlink,ownevent]

> fs:start_logger(). % starts a sample process that logs events with error_logger
=INFO REPORT==== 28-Aug-2013::19:36:26 ===
file_event: "/tank/proger/erlfsmon/src/4913" [closed,modified]
```

Credits
-------

* Vladimir Kirillov
* Maxim Sokhatsky

OM A HUM
