<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. LPSG version 0.0.1</a>
<ul>
<li><a href="#sec-1-1">1.1. Introduction</a></li>
<li><a href="#sec-1-2">1.2. Documentation</a></li>
<li><a href="#sec-1-3">1.3. Example</a></li>
<li><a href="#sec-1-4">1.4. OpenGL</a></li>
<li><a href="#sec-1-5">1.5. Common Lisp dependencies</a></li>
<li><a href="#sec-1-6">1.6. License</a></li>
</ul>
</li>
</ul>
</div>
</div>

# LPSG version 0.0.1<a id="sec-1" name="sec-1"></a>

LPSG is a library for writing high-performance graphical applications
in Common Lisp, using OpenGL. 

## Introduction<a id="sec-1-1" name="sec-1-1"></a>

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

## Documentation<a id="sec-1-2" name="sec-1-2"></a>

Documentation for the LPSG API can be found
[here](<http://timoore.github.io/lpsg/lpsg.html>). The sources for the
documentation are in the [docs](docs) directory.

## Example<a id="sec-1-3" name="sec-1-3"></a>

A commented example is found in [cube.lisp](examples/cube.lisp).

## OpenGL<a id="sec-1-4" name="sec-1-4"></a>

LPSG nominally requires OpenGL 3.3, but might be coaxed into running
with earlier versions of OpenGL and (yet-to-be-identified)
extensions. LPSG uses only core features of OpenGL. There is interest
in eventually running with OpenGL ES 3.0.

Some future features of LPSG, as well as user code, will require newer
versions of OpenGL than 3.3. There will be runtime support for determining
what features are supported on a platform.

## Common Lisp dependencies<a id="sec-1-5" name="sec-1-5"></a>

LPSG depends directly on the following Common Lisp libraries, among
others. More will certainly be added in the future, so it is simplest
to use Quicklisp to load the LPSG project and its dependencies in one go.

-   [cl-opengl](<https://common-lisp.net/project/cl-opengl/>) OpenGL interface
-   [sb-cga](<http://nikodemus.github.io/sb-cga/>) matrix library
-   [mathkit](<https://github.com/lispgames/mathkit>) graphics math library
-   [alexandria](<https://common-lisp.net/project/alexandria/>) utility library

## License<a id="sec-1-6" name="sec-1-6"></a>

LPSG is released under the Revised BSD license.