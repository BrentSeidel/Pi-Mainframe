with BBS.embed;
--use type BBS.embed.uint8;
--use type BBS.embed.uint16;
--use type BBS.embed.uint32;
with BBS.embed.i2c;
with i2c;
with sim;
--with sim.example;
--
--  This package contains information and code for the simulator.  Right now,
--  it just blinks lights in interesting patterns.
--
package Panel is
   --
   --  Public data for the simulation
   --
   --
   --  Simulator
   --
   simulate : Sim.sim_access;
   --
   --  Processor modes
   --
   type proc_mode is (PROC_NONE, PROC_KERN, PROC_EXEC, PROC_SUP, PROC_USER);
   --
   for proc_mode use (PROC_NONE => 0,
                      PROC_KERN => 16#1#,
                      PROC_EXEC => 16#2#,
                      PROC_SUP  => 16#4#,
                      PROC_USER => 16#8#);
   for proc_mode'Size use 4;
   --
   --  Address types
   --
   type addr_type is (ADDR_NONE, ADDR_INTR, ADDR_DATA, ADDR_INST);
   --
   for addr_type use (ADDR_NONE => 0,
                      ADDR_INTR => 16#01#,
                      ADDR_DATA => 16#02#,
                      ADDR_INST => 16#04#);
   for addr_type'Size use 3;
   --
   --  Control and mode switches and LEDs
   type ctrl_mode is record
      unused0 : Boolean;    --  LED/Switch 0 is hardwired to power
      ready   : Boolean;    --  LED only
      exam    : Boolean;    --  Examine
      dep     : Boolean;    --  Deposit
      addr    : Boolean;    --  Address/Data
      auto    : Boolean;    --  Auto/Man, enable remote control via web server
      start   : Boolean;    --  Start
      run     : Boolean;    --  Run
      atype   : addr_type;  --  LED only, address type
      blank   : Boolean;    --  LED only, blank
      mode    : proc_mode;  --  LED only, processor mode
   end record;
   --
   for ctrl_mode use record
      unused0 at 0 range  0 ..  0;
      ready   at 0 range  1 ..  1;
      exam    at 0 range  2 ..  2;
      dep     at 0 range  3 ..  3;
      addr    at 0 range  4 ..  4;
      auto    at 0 range  5 ..  5;
      start   at 0 range  6 ..  6;
      run     at 0 range  7 ..  7;
      atype   at 0 range  8 .. 10;
      blank   at 0 range 11 .. 11;
      mode    at 0 range 12 .. 15;
   end record;
   --
   --  Is selection automatic (True) or manual (False).  This is set by the web
   --  interface and can only be changed when ctl_auto is True.
   --
   auto_man : Boolean := False;
   --
   --  Switch settings (switch registers).  These are read only
   --
   function sr_ad   return BBS.embed.uint32;  --  Address/Data switches
   function sr_ctl  return BBS.embed.uint16;  --  Control switches
   function sw_ctrl return ctrl_mode;         --  Control switches
   --
   --  LED settings (LED registers)
   --
   lr_ad  : BBS.embed.uint32 := 0;          -- Address/Data LED register
   lr_ctl : aliased BBS.embed.uint16 := 0;  -- Control/Mode LED register
   lr_ctrl : ctrl_mode := (atype => ADDR_DATA, mode => PROC_USER,
                           others => False) with
     Address => lr_ctl'Address;
   --
   --  Flag to exit simulator loop
   --
   exit_sim : Boolean := False;
   --
   --  Flags for control switch changes to True state.  Functions are used for
   --  these so that the flags can only be written by this package.
   --
   function ctl_starting return Boolean;
   function ctl_deposit  return Boolean;
   function ctl_examine  return Boolean;
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
   --
   --  Simulator external interfaces
   --
   --  Get and set the selected test pattern
   --
   procedure set_pattern(p : Natural);
   function get_pattern return Natural;
private
   --
   --  Switch settings (switch registers).  These are read only
   --
   pvt_sr_ad  : BBS.embed.uint32 := 0;          -- Address/Data switch register
   pvt_sr_ctl : aliased BBS.embed.uint16 := 0;  -- Control switch register
   pvt_sw_ctrl : ctrl_mode with
     Address => pvt_sr_ctl'Address;
   function sr_ad   return BBS.embed.uint32 is (pvt_sr_ad);
   function sr_ctl  return BBS.embed.uint16 is (pvt_sr_ctl);
   function sw_ctrl return ctrl_mode is (pvt_sw_ctrl);
   --
   --  Local switch flags for detecting switch changes
   --
   ctl_start    : Boolean := False;
   ctl_dep      : Boolean := False;
   ctl_exam     : Boolean := False;
   pvt_starting : Boolean := False;  --  When ctl_start changes to True
   pvt_deposit  : Boolean := False;  --  when ctl_dep changes to True
   pvt_examine  : Boolean := False;  --  when ctl_exam changes to True
   function ctl_starting return Boolean is (pvt_starting);
   function ctl_deposit  return Boolean is (pvt_deposit);
   function ctl_examine  return Boolean is (pvt_examine);
   --
   --  Which pattern to select:
   --     0 - Copy switches
   --     1 - count
   --     2 - scan 16-bit
   --     3 - bounce 16-bit
   --     4 - fibbonacci
   --     9 - count
   --    10 - scan 32-bit
   --    11 - bounce 32-bit
   --    12 - fibbonacci
   --    others - Copy switches
   --
--   pattern : BBS.embed.uint32 := 0;
   --
   --  Data for the various patterns.
   --
--   ad_counter : BBS.embed.uint32 := 0;
--   ctl_counter : BBS.embed.uint16 := 0;

--   type bounce_type is (left, right);
--   ad_bouncer : BBS.embed.uint32 := 0;
--   ad_bounce_dir : bounce_type := left;
--   ctl_bouncer : BBS.embed.uint16 := 0;
--   ctl_bounce_dir : bounce_type := left;

--   ad_scanner : BBS.embed.uint32 := 0;
--   ctl_scanner : BBS.embed.uint16 := 0;

--   ad_fib_1 : BBS.embed.uint32 := 1;
--   ad_fib_2 : BBS.embed.uint32 := 1;
--   ctl_fib_1 : BBS.embed.uint16 := 1;
--   ctl_fib_2 : BBS.embed.uint16 := 2;

   err  : BBS.embed.i2c.err_code;
   res  : i2c.result;
   --
   --  Code for the various patterns.
   --
--   procedure count(d : Duration);
--   procedure bounce16(d : Duration);
--   procedure bounce32(d : Duration);
--   procedure scan16(d : Duration);
--   procedure scan32(d : Duration);
--   procedure fibonacci(d : Duration);
   procedure copy_sw(d : Duration);
   procedure copy_sw_ad;
   procedure copy_sw_ctl;
   --
   --  Initialize the various test patterns to their initial state
   --
--   procedure init_test;
   --
   --  Process the control switches and set flags as appropriate
   --
   procedure process_switch;
   --
   --  Process the mode and control LEDs
   --
   procedure process_mode_ctrl(m : proc_mode; a : addr_type; c : BBS.embed.uint16);
end;
