# grisp_emulation

This is a hardware emulation layer for the
[GRiSP runtime](https://github.com/grisp/grisp). With this, you can replace the
GRiSP specific low-level drivers with a native Erlang emulation. This allows you
to run the GRiSP runtime on a normal development computer instead of on the
board.

The emulation layer comes with a few different emulated Pmod drivers:

* PmodACL2
* PmodGYRO
* PmodNAV
* MaxSonar

The emulated drivers are in various states of development, ranging from just
barely starting to a semi-full emulated state of hardware components.
Contributions adding new drivers are welcome!

It also comes with an emulation structure for changing the low-level drivers:

* SPI
* GPIO
* I2C

This is what the emulated drivers above hook into to fake actual hardware.
