Shen Batteries
==============

Collection of libraries for Shen.

Loading libraries
-----------------

Load the module loader, point it at this repository, and use one or more
libraries:

```shen
(load "library.shen")
(library.set-home "/path/to/shen-batteries")
(library.use [box lazy])
```

`library.use` loads dependencies and sources once. Loading is permanent for the
current Shen image; there is no unload operation. Set the module home before
the first call to `library.use`; it cannot change after modules are loaded.

Modules use the portable `shen.module` version 1 format. Descriptors are found
at `<module-root>/<module-name>.shenmod`, and their source paths are relative to
the descriptor. Port-specific settings belong in namespaced `extension` fields
and are ignored by the portable source loader. `with-exit` and `iter` require
the `shen/scheme` feature.

Testing
-------

Provide the Shen executable explicitly:

```sh
make test SHEN=/absolute/path/to/shen
```

License
-------

Copyright © 2019 Bruno Deferrari under [BSD 3-Clause License](http://opensource.org/licenses/BSD-3-Clause).
