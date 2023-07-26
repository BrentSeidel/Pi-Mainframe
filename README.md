# Pi-Mainframe
Simulated mainframe computer based on a Raspberry Pi

When I was a young lad, computers had complicated panels filled with
switches and lights.  Sadly, these disappeared as I got older.

This project is an attempt to recreate the feel of old mainframe computers
using a Raspberry Pi 3 and some other assorted parts that I had laying
around.  The actual boxes and panels are 3D printed.

## Note
This is still very much a work in progress and things will change.

## IO Board PCB
I am currently in the process of creating a schematic and laying out a PCB
for the I/O boards.  The current state is in the repository, but it isn't
finished yet.  Once I am satisfied and have PCBs made and tested, I will
update the documentation.  This should simplify assembly.

The first version of the PCB had an error in it.  It could be fixed by
cutting traces and soldering in jumper wires.  Testing is not complete,
but it seems to work.  The schematic has been updated and a new board
is being layed out.

This board also seems like it wold be useful for other projects, so some
effort is being put into making it more generic.  Once I am happy with it,
it will move into a repository for schematics and boards (yet to be
created).

## Dependencies
This depends on the following repositories:
* https://github.com/BrentSeidel/BBS-Ada.git
* https://github.com/BrentSeidel/BBS-BBB-Ada.git
* https://github.com/BrentSeidel/Ada-Web-Server.git
* https://github.com/BrentSeidel/Things.git
* https://github.com/BrentSeidel/Sim-CPU.git

If you are actually interested in simulating old computers, you can
download and build simh from github at https://github.com/simh/simh.git
I was able to get the PDP-11 sim to build (I haven't tried others) and
run on a Raspberry Pi.  As far as I know, it has no support for blinking
lights.

You could, of course run simh in parallel with this project giving a
CPU simulation as well as lights and switches.  Unfortunately, they would
be completely independant.  Someday, perhaps this will change.

## Assembly
This project will require quite a bit of 3D printing and soldering.  The
included documentation includes a parts list.  Read the documentation
first, then download the dependencies repositories.  Finally decided
how big of a system you want.
1. Slice the 3D models and start them printing.  This will take a while,
   so start this first and then work on other things while they are printing.
2. Install Raspberian on the Raspberry Pi and install the following packages:
   * avahi - This allows discovery on your local network
   * gnat - The GNU Ada distribution
   * gprbuild - Used for building Ada software
3. Build the software.  If there are errors about missing "obj" directories,
   create the directories.
4. Start soldering the various boards and cables.
5. As panels come off the printer, install LEDs and switches as appropriate
6. As the trays come off the printer, install the boards and connectors.
7. Once everything is printed, put all the pieces together.
