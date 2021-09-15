with BBS.embed;
use type BBS.embed.uint16;
with BBS.embed.i2c;
package patterns is
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
   counter : BBS.embed.uint16 := 0;

   type bounce_type is (left, right);
   bouncer : BBS.embed.uint16 := 0;
   bounce_dir : bounce_type := left;

   scanner : BBS.embed.uint16 := 0;

   fib_1 : BBS.embed.uint16 := 1;
   fib_2 : BBS.embed.uint16 := 1;

   err  : BBS.embed.i2c.err_code;
   --
   --  Code for the various patterns.
   --
   procedure count(d : Duration);
   procedure bounce(d : Duration);
   procedure scan(d : Duration);
   procedure fibonacci(d : Duration);
end;
