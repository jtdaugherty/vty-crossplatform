0.5.0.0
=======

* Addressed a security issue that would have allowed a malicious third
  party to upload a 'unknown-vty-build-platform' package to Hackage to
  introduce malicious code on platforms where 'vty-crossplatform' isn't
  supported. Now, instead of failing to build against a fake package for
  build error readability purposes, 'buildable' is set to 'False' to
  fail the build more directly.
* Updated to work with `vty-windows` 0.2.0.1.

0.4.0.0
=======

* Updated to work with `vty-windows` 0.2.0.0.

0.3.0.0
=======

Package changes:
* Updated `vty` lower bound to 6.1. Updated testing code to build with
  6.1.

0.2.0.0
=======

Package changes:
* Added build-time selection of `vty-unix` on FreeBSD, OpenBSD, NetBSD,
  Solaris, AIX, HPUX, IRIX, Hurd, and DragonflyBSD.

0.1.0.0
=======

* First version.
