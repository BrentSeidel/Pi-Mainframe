with BBS.embed;
--use type BBS.embed.uint16;
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
   --  Switch settings (switch registers)
   --
   sr_ad  : BBS.embed.uint32 := 0;  -- Address/Data switch register
   sr_ctl : BBS.embed.uint32 := 0;  -- Control switch register
   --
   --  LED settings (LED registers)
   --
   lr_ad  : BBS.embed.uint32 := 0;  -- Address/Data LED register
   lr_ctl : BBS.embed.uint32 := 0;  -- Control/Mode LED register
   --
   --  Processor modes
   --
   type proc_mode is (PROC_KERN, PROC_EXEC, PROC_SUP, PROC_USER);
   for proc_mode use (PROC_KERN => 16#10#,
                      PROC_EXEC => 16#20#,
                      PROC_SUP  => 16#40#,
                      PROC_USER => 16#80#);
   for proc_mode'Size use 8;
   --
   --  Address types
   --
   type addr_type is (ADDR_INTR, ADDR_DATA, ADDR_INST);
   for addr_type use (ADDR_INTR => 16#01#,
                      ADDR_DATA => 16#02#,
                      ADDR_INST => 16#04#);
   for addr_type'Size use 8;
   --
   --  Control switches/LEDs
   --
   type controls is (CTRL_EXAM, CRTL_DEP, CTRL_ADDR, CTRL_AUTO, CTRL_START, CTRL_RUN);
   for controls use (CTRL_RUN   => 16#80#,
                     CTRL_START => 16#40#,
                     CTRL_AUTO  => 16#20#,
                     CTRL_ADDR  => 16#10#,
                     CRTL_DEP   => 16#08#,
                     CTRL_EXAM  => 16#04#);
   for controls'Size use 8;
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
   bouncer : BBS.embed.uint32 := 0;
   bounce_dir : bounce_type := left;

   scanner : BBS.embed.uint32 := 0;

   fib_1 : BBS.embed.uint32 := 1;
   fib_2 : BBS.embed.uint32 := 1;

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
