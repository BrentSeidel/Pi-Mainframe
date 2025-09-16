with BBS;
use type BBS.uint32;
with BBS.embed;
with BBS.embed.i2c;
with i2c;
with BBS.Sim_CPU;
with BBS.Sim_CPU.bus;
with BBS.Sim_CPU.CPU;
--with BBS.Sim_CPU.CPU.example;
--with BBS.Sim_CPU.CPU.i8080;
--with BBS.Sim_CPU.CPU.m68000;
with BBS.Sim_CPU.io.Clock;
with BBS.Sim_CPU.io.serial;
with BBS.Sim_CPU.io.serial.mux;
with BBS.Sim_CPU.io.serial.telnet;
with BBS.Sim_CPU.io.disk;
--with BBS.Sim_CPU.bus.mem8;
--
--  This package contains information and code for the front panel of the simulator.
--
package Panel is
   --
   --  Public data for the simulation
   --
   --
   --  Simulator.  Change this to which ever simulator is being used.
   --
--   sim_example : aliased BBS.Sim_CPU.CPU.example.simple;
--   sim_8080    : aliased BBS.Sim_CPU.CPU.i8080.i8080;
--   sim_68000   : aliased BBS.Sim_CPU.CPU.m68000.m68000;
   cpu : BBS.Sim_CPU.CPU.sim_access;
   bus : BBS.Sim_CPU.bus.bus_access;
   --
   --  Instantiate disk controller
   --
  package floppy_ctrl is new BBS.Sim_CPU.io.disk(sector_size => 128);
   --
   --  Devices for processor simulator
   --
   tel   : aliased BBS.Sim_CPU.io.serial.telnet.tel_tty;
   tel0  : aliased BBS.Sim_CPU.io.serial.telnet.tel_tty;
   tel1  : aliased BBS.Sim_CPU.io.serial.telnet.tel_tty;
   tel2  : aliased BBS.Sim_CPU.io.serial.telnet.tel_tty;
   mux   : aliased BBS.Sim_CPU.io.serial.mux.mux_tty;
--   fd    : aliased floppy_ctrl.fd_ctrl(max_num => 7);
   fd    : aliased floppy_ctrl.fd_access := new floppy_ctrl.fd_ctrl(max_num => 7);
   clock : aliased BBS.Sim_CPU.io.Clock.clock_device;
   --
   --
   --  Is selection automatic (True) or manual (False).  This is set by the web
   --  interface and can only be changed when ctl_auto is True.
   --
   auto_man : Boolean := False;
   --
   --  Switch settings (switch registers).  These are read only
   --
   function sr_ad   return BBS.uint32;  --  Address/Data switches
   function sr_ctl  return BBS.uint16;  --  Control switches
   function sw_ctrl return BBS.Sim_CPU.ctrl_mode;         --  Control switches
   --
   --  LED settings (LED registers)
   --
   lr_ad  : BBS.uint32 := 0;          -- Address/Data LED register
   lr_ctl : aliased BBS.uint16 := 0;  -- Control/Mode LED register
   lr_ctrl : BBS.Sim_CPU.ctrl_mode := (atype => BBS.Sim_CPU.ADDR_DATA,
                                       mode => BBS.Sim_CPU.PROC_USER,
                                       others => False) with
     Address => lr_ctl'Address;
   --
   --  Flag to exit simulator loop
   --
   exit_sim : Boolean := False;
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
   --
   --  Initializations for different simulations
   --
   procedure init_sim_example;
   procedure init_sim_8080;
   procedure init_sim_68000;
private
   --
   --  Switch settings (switch registers).  These are read only
   --
   pvt_sr_ad  : BBS.uint32 := 0;          -- Address/Data switch register
   pvt_sr_ctl : aliased BBS.uint16 := 0;  -- Control switch register
   pvt_sw_ctrl : BBS.Sim_CPU.ctrl_mode with
     Address => pvt_sr_ctl'Address;
   function sr_ad   return BBS.uint32 is (pvt_sr_ad);
   function sr_ctl  return BBS.uint16 is (pvt_sr_ctl);
   function sw_ctrl return BBS.Sim_CPU.ctrl_mode is (pvt_sw_ctrl);
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
   procedure process_mode_ctrl(m : BBS.Sim_CPU.proc_mode; a : BBS.Sim_CPU.addr_type; c : BBS.uint16);
end;
