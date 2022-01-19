with BBS.embed;
with BBS.embed.i2c;
with i2c;
with BBS.Sim;
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
   simulate : BBS.Sim.sim_access;
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
   function sw_ctrl return BBS.Sim.ctrl_mode;         --  Control switches
   --
   --  LED settings (LED registers)
   --
   lr_data : BBS.embed.uint32 := 0;
   lr_addr : BBS.embed.uint32 := 0;
   lr_ad  : BBS.embed.uint32 := 0;          -- Address/Data LED register
   lr_ctl : aliased BBS.embed.uint16 := 0;  -- Control/Mode LED register
   lr_ctrl : BBS.Sim.ctrl_mode := (atype => BBS.Sim.ADDR_DATA, mode => BBS.Sim.PROC_USER,
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
   pvt_sw_ctrl : BBS.Sim.ctrl_mode with
     Address => pvt_sr_ctl'Address;
   function sr_ad   return BBS.embed.uint32 is (pvt_sr_ad);
   function sr_ctl  return BBS.embed.uint16 is (pvt_sr_ctl);
   function sw_ctrl return BBS.Sim.ctrl_mode is (pvt_sw_ctrl);
   --
   --  Local switch flags for detecting switch changes
   --
   ctl_start    : Boolean := False;
   ctl_dep      : Boolean := False;
   ctl_exam     : Boolean := False;
   pvt_starting : Boolean := False;  --  When ctl_start changes to True
   pvt_deposit  : Boolean := False;  --  when ctl_dep changes to True
   pvt_examine  : Boolean := False;  --  when ctl_exam changes to True
   last_addr    : Boolean := False;  --  Last value of sw_ctrl.addr
   function ctl_starting return Boolean is (pvt_starting);
   function ctl_deposit  return Boolean is (pvt_deposit);
   function ctl_examine  return Boolean is (pvt_examine);
   --
   err  : BBS.embed.i2c.err_code;
   res  : i2c.result;
   --
   --  Process the control switches and set flags as appropriate
   --
   procedure process_switch;
   --
   --  Process the mode and control LEDs
   --
   procedure process_mode_ctrl(m : BBS.Sim.proc_mode; a : BBS.Sim.addr_type; c : BBS.embed.uint16);
end;
