# Pi-Mainframe
Simulated mainframe computer based on a Raspberry Pi

When I was a young lad, computers had complicated panels filled with
switches and lights.  Sadly, these disappeared as I got older.

This project is an attempt to recreate the feel of old mainframe computers
using a Raspberry Pi 3 and some other assorted parts that I had laying
around.  The actual boxes and panels are 3D printed.

This is still very much a work in progress and things will change.

## Dependencies
This depends on the following repositories:
* https://github.com/BrentSeidel/BBS-Ada.git
* https://github.com/BrentSeidel/BBS-BBB-Ada.git
* https://github.com/BrentSeidel/Ada-Web-Server.git
* https://github.com/BrentSeidel/Things.git

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
