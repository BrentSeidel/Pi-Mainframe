with BBS.embed;
use type BBS.embed.uint16;
use type BBS.embed.uint32;
with BBS.embed.i2c;
with i2c;
--
--  This package contains information and code for the simulator.  Right now,
--  it just blinks lights in interesting patterns.
--
package Sim is
   --
   --  Public data for the simulation
   --
   --  Is selection automatic (True) or manual (False)
   --
   auto_man : Boolean := False;
   --
   --  Which pattern to select:
   --    0 - Copy switches
   --    1 - count
   --    2 - bounce
   --    3 - scan
   --    4 - fibbonacci
   --    others - Copy switches
   --
   pattern : Natural := 0;
   --
   --  Run the LED patterns.  This is a task so that it can run in parallel with
   --  the web server.
   --
   task run is
      entry Start;
   end run;
private
   --
   --  Data for the various patterns.
   --
   counter : BBS.embed.uint32 := 0;

   type bounce_type is (left, right);
   bouncer : BBS.embed.uint16 := 0;
   bounce_dir : bounce_type := left;

   scanner : BBS.embed.uint16 := 0;

   fib_1 : BBS.embed.uint16 := 1;
   fib_2 : BBS.embed.uint16 := 1;

   err  : BBS.embed.i2c.err_code;
   res  : i2c.result;
   --
   --  Code for the various patterns.
   --
   procedure count(d : Duration);
   procedure bounce(d : Duration);
   procedure scan(d : Duration);
   procedure fibonacci(d : Duration);
end;
