# Pi-Mainframe
Simulated mainframe computer based on a Raspberry Pi

When I was a young lad, computers had complicated panels filled with
switches and lights.  Sadly, these disappeared as I got older.

This project is an attempt to recreate the feel of old mainframe computers
using a Raspberry Pi 3 and some other assorted parts that I had laying
around.  The actual boxes and panels are 3D printed.

Here is an example system.  Note that it is fairly modular and the
modules can be attached in different ways.
![System Front](./Pict/System-Front.JPG)

Back of system
![System Back](./Pict/System-Rear.JPG)

## Note
This is still very much a work in progress and things will change.

This project requires 3D printing, making PCBs, soldering, and building
software from source.  It might be a good project for an undergraduate
engineering program.

## Power
I originally used a random micro USB power suppoly for the Pi 3 that is
running the simulation.  It would complain about low voltage and
occasionally reboot.  I just replaced it with the official Raspberry Pi
power supply and it is not complaining about low voltage.  Time will
tell if it is more stable.

Ultimately, there will be limits to how much power you can get out of
the Raspberry Pi.  There are ways to reduce the power needed.  The first
thing that I would try would be to increase the value of the resistors
for the LEDs.  This would also reduce the brightness so you may wish to
do this as well, if the LEDs are too bright.  A little experimentation
will be needed to determine the proper value.  From a BOM viewpoint,
using the same resistors for both the switches and LEDs would be a win.

## PCBs
Three printed circuit boards have been designed to eliminate some of the
tedious soldering.  Using the PCBs make assembly much easier and the
resulting project much less of a mess of wires.

### IO Board PCB
The I/O board PCB has been created and tested.  The schematics and PCB
layout have been moved to the Circuits reporitory.  Documentation and
assembly instructions for it have been created there.

### Breakout PCB
A breakout PCB has been developed for breaking out the 10 conductor
ribbon cables that are being used for the I2C bus, and other uses.  It
is also in the Circuits repository.

### LED PCB
A PCB has been developed for holding the LEDs.  This required some redesign
to the panels.  However it eliminates the soldering of wires directly to
the LED leads.  The LED PCB can also be fairly easily added to and
removed from panels should you wish to, for example, use LEDs of different
colors.

## Dependencies
This depends on the following repositories:
* https://github.com/BrentSeidel/BBS-Ada.git
* https://github.com/BrentSeidel/BBS-BBB-Ada.git
* https://github.com/BrentSeidel/Ada-Web-Server.git
* https://github.com/BrentSeidel/Things.git
* https://github.com/BrentSeidel/Sim-CPU.git
* https://github.com/BrentSeidel/Circuits.git

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
