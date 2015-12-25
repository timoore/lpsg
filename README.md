# LPSG

LPSG is a library for writing high-performance graphical applications
in Common Lisp, using OpenGL. 

## Introduction

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


## License

LPSG is released under the BSD license.
