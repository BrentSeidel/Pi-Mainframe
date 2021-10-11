with BBS.embed;
use type BBS.embed.uint8;
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
   --  Is selection automatic (True) or manual (False).  This is set by the web
   --  interface and can be changed when ctl_auto is True.
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
   sr_ctl : BBS.embed.uint16 := 0;  -- Control switch register
   --
   --  LED settings (LED registers)
   --
   lr_ad  : BBS.embed.uint32 := 0;  -- Address/Data LED register
   lr_ctl : BBS.embed.uint16 := 0;  -- Control/Mode LED register
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
   --  Constants for LEDs
   --
   LED_MODE_USER  : constant BBS.embed.uint16 := 16#8000#;
   LED_MODE_SUP   : constant BBS.embed.uint16 := 16#4000#;
   LED_MODE_EXEC  : constant BBS.embed.uint16 := 16#2000#;
   LED_MODE_KERN  : constant BBS.embed.uint16 := 16#1000#;
   LED_MODE_BLNK  : constant BBS.embed.uint16 := 16#0800#;
   LED_MODE_INST  : constant BBS.embed.uint16 := 16#0400#;
   LED_MODE_DATA  : constant BBS.embed.uint16 := 16#0200#;
   LED_MODE_INTR  : constant BBS.embed.uint16 := 16#0100#;
   LED_CTRL_RUN   : constant BBS.embed.uint16 := 16#0080#;
   LED_CTRL_START : constant BBS.embed.uint16 := 16#0040#;
   LED_CTRL_AUTO  : constant BBS.embed.uint16 := 16#0020#;
   LED_CTRL_ADDR  : constant BBS.embed.uint16 := 16#0010#;
   LED_CTRL_DEP   : constant BBS.embed.uint16 := 16#0008#;
   LED_CTRL_EXAM  : constant BBS.embed.uint16 := 16#0004#;
   LED_CTRL_READY : constant BBS.embed.uint16 := 16#0002#;
   LED_CTRL_POWER : constant BBS.embed.uint16 := 16#0001#;
   --
   --  Flags for control switches
   --
   ctl_run      : Boolean := False;
   ctl_start    : Boolean := False;
   ctl_starting : Boolean := False;  --  When ctl_start changes to True
   ctl_auto     : Boolean := False;
   ctl_addr     : Boolean := False;
   ctl_dep      : Boolean := False;
   ctl_deposit  : Boolean := False;  --  when ctl_dep changes to True
   ctl_exam     : Boolean := False;
   ctl_examine  : Boolean := False;  --  when ctl_exam changes to True
   --
   --  Note that the power LED is actually connected to the power rail and not
   --  under program control.
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
   ad_counter : BBS.embed.uint32 := 0;
   ctl_counter : BBS.embed.uint16 := 0;

   type bounce_type is (left, right);
   ad_bouncer : BBS.embed.uint32 := 0;
   ad_bounce_dir : bounce_type := left;
   ctl_bouncer : BBS.embed.uint16 := 0;
   ctl_bounce_dir : bounce_type := left;

   ad_scanner : BBS.embed.uint32 := 0;
   ctl_scanner : BBS.embed.uint16 := 0;

   ad_fib_1 : BBS.embed.uint32 := 1;
   ad_fib_2 : BBS.embed.uint32 := 1;
   ctl_fib_1 : BBS.embed.uint16 := 1;
   ctl_fib_2 : BBS.embed.uint16 := 2;

   err  : BBS.embed.i2c.err_code;
   res  : i2c.result;
   --
   --  Code for the various patterns.
   --
   procedure count(d : Duration);
   procedure bounce(d : Duration);
   procedure scan(d : Duration);
   procedure fibonacci(d : Duration);
   procedure copy_sw(d : Duration);
   --
   --  Process the control switches and set flags as appropriate
   --
   procedure process_ctrl(d : BBS.embed.uint16);
   --
   --  Initialize the various test patterns to their initial state
   --
   procedure init_test;
end;
