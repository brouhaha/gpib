IEEE-488 (GP-IB, HP-IB) synthesizable core in VHDL
README.txt
Copyright 2016 Eric Smith <spacewar@gmail.com>

The IEEE-488 synthesizable core and related testbenches are Free
Software licensed under the terms of the Free Software Foundation
General Public License version 3. Please read the included files
LICENSE.txt and gpl3.txt for details.

The core consists of the VHDL package gpib_defs, a top-level
entity gpib, an entity for each interface function, named gpib_FN
(where FN is sh, ah, t, l, etc.), and an entity synchronizer, all
in files in the rtl subdirectory, with files named base on the package
nd entity names with a ".vhdl" suffix.

VHDL Testbenches for simulation, and stimulus files, are provided in
the testbench subdirectory.

The core does not provide a device interface compatible with any
existing IEEE-488 interface chip, such as the TI TMS9914A, NEC
uPD7210, Intel 8291, or Motorola 68488. If desired, it should be
possible to write a wrapper around the gpib entity to provide such an
interface, but I presently have no plans to do that.

As of 2016-01-14, the SH and AH interface functions have been tested
somewhat in simulation using the provided testbenches and stimulus
files, but the stimulus files do not have 100% coverage.  The
remaining interface functions, except SHE and AHE, have been coded but
have not been tested at all.  Over time it is expected that more
testing will be done.

I have no intent to implement SHE and AHE, which would provide the
faster, non-interlocked handshake mode of the 2003 edition of the
standard, as I do not have, nor am I even aware of the existence of,
any equipment that actually supports that capability.
