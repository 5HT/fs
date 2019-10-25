FS: Native Listener
===================
[![Build Status](https://travis-ci.org/synrc/fs.svg?branch=master)](https://travis-ci.org/synrc/fs)
[![Hex pm](http://img.shields.io/hexpm/v/fs.svg?style=flat)](https://hex.pm/packages/fs)

Backends
--------

* Mac [fsevent](https://github.com/thibaudgg/rb-fsevent)
* Linux [inotify](https://github.com/rvoicilas/inotify-tools/wiki)
* Windows [inotify-win](https://github.com/thekid/inotify-win)

NOTE: On Linux you need to install inotify-tools.

### Subscribe to Notifications

```erlang
> fs:start_link(fs_watcher, "/Users/5HT/synrc/fs"). % need to start the fs watcher
> fs:subscribe(fs_watcher). % the pid will receive events as messages
> flush().
Shell got {<0.47.0>,
           {fs,file_event},
           {"/Users/5HT/synrc/fs/src/README.md",[closed,modified]}}
```

### List Events from Backend

```erlang
> fs:known_events(fs_watcher). % returns events known by your backend
[mustscansubdirs,userdropped,kerneldropped,eventidswrapped,
 historydone,rootchanged,mount,unmount,created,removed,
 inodemetamod,renamed,modified,finderinfomod,changeowner,
 xattrmod,isfile,isdir,issymlink,ownevent]
```

### Sample Subscriber

```erlang
> fs:start_looper(). % starts a sample process that logs events
=INFO REPORT==== 28-Aug-2013::19:36:26 ===
file_event: "/tank/proger/erlfsmon/src/4913" [closed,modified]
```

### API compatibility

API is per default compatible to version before 1.10.

By application start, `fs` will start fs watcher on specified per enviroment `path` or
if enviroment is unsetted, than in `CWD`.

That means you can still use it, like:

```erlang
fs:subscribe()
```

If you do not want to use backwards_compatible mode, disable it by setting `fs` enviroment:

```
{backwards_compatible, false}
```

This option will lead, that default fs watcher willn't be started.

Credits
-------

* Vladimir Kirillov
* Maxim Sokhatsky

OM A HUM
