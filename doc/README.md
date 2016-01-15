- [LPSG](#sec-1)
  - [Introduction](#sec-1-1)
  - [Example](#sec-1-2)
  - [OpenGL](#sec-1-3)
  - [Common Lisp dependencies](#sec-1-4)
  - [License](#sec-1-5)

# LPSG<a id="orgheadline6"></a>

LPSG is a library for writing high-performance graphical applications
in Common Lisp, using OpenGL. 

## Introduction<a id="orgheadline1"></a>

LPSG accepts geometric data and descriptions of the OpenGL state used
to render them. It processes the data in order to minimize traffic between the
CPU and GPU (Graphics Processing Unit). It can also reorder the associated
graphic commands, thereby reducing the number of function calls into
the OpenGL driver and the number of state changes in the graphics
hardware.

LPSG is still in a very early stage of development, but it will
include support for common features in 3D graphical applications,
such as simple text, rendering to a texture, rendering high-quality
transparency, GPU-based selection, and more.

## Example<a id="orgheadline2"></a>

A commented example is found in [cube.lisp](examples/cube.lisp).

## OpenGL<a id="orgheadline3"></a>

LPSG nominally requires OpenGL 3.3, but might be coaxed into running
with earlier versions of OpenGL and (yet-to-be-identified)
extensions. LPSG uses only core features of OpenGL. There is interest
in eventually running with OpenGL ES 3.0.

Some future features of LPSG, as well as user code, will require newer
versions of OpenGL than 3.3. There will be runtime support for determining
what features are supported on a platform.

## Common Lisp dependencies<a id="orgheadline4"></a>

LPSG depends directly on the following Common Lisp libraries,
available through QuickLisp:

-   [cl-opengl](<https://common-lisp.net/project/cl-opengl/>) OpenGL interface
-   [sb-cga](<http://nikodemus.github.io/sb-cga/>) matrix library
-   [mathkit](<https://github.com/lispgames/mathkit>) graphics math library
-   [alexandria](<https://common-lisp.net/project/alexandria/>) utility library

## License<a id="orgheadline5"></a>

LPSG is released under the Revised BSD license.
