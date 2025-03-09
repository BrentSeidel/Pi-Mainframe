with Ada.Text_IO;
package body Panel is
   --
   --  Run the panel interface and simulator control.
   --
   task body run is
      package Hex_IO is new Ada.Text_IO.Integer_IO(Integer);
   begin
      --
      --  Wait for I2C initialization, MCP23017 detection and other
      --  initializations in the main thread before starting.
      --
      accept Start;
      --
      --  Configure MCP23017 devices
      --
      Ada.Text_IO.Put_Line("Setting LED ports to output and displaying data");
      i2c.MCP23017_info(i2c.LED_LSW).set_dir(16#0000#, err);
      Hex_IO.Default_Base := 16;
      Hex_IO.Default_Width := 1;
      if i2c.MCP23017_found(i2c.LED_LSW) then
         i2c.MCP23017_info(i2c.LED_LSW).set_dir(16#0000#, err);
         Ada.Text_IO.Put_Line("LED least significant word configured");
      end if;
      if i2c.MCP23017_found(i2c.LED_MSW) then
         i2c.MCP23017_info(i2c.LED_MSW).set_dir(16#0000#, err);
         Ada.Text_IO.Put_Line("LED most significant word configured");
      end if;
      if i2c.MCP23017_found(i2c.LED_CTRL) then
         i2c.MCP23017_info(i2c.LED_CTRL).set_dir(16#0000#, err);
         Ada.Text_IO.Put_Line("LED mode and control configured");
      end if;
      i2c.set_addr_data(lr_ad, res);
      i2c.set_ctrl(lr_ctl, res);
      --
      --  Processing loop
      --
      loop
         i2c.read_addr_data(pvt_sr_ad, res);
         i2c.read_ctrl(pvt_sr_ctl, res);
         CPU.all.set_sr_ad(pvt_sr_ad);
         CPU.all.set_sr_ctrl(pvt_sw_ctrl);
         process_switch;
         if pvt_starting then
            CPU.all.start;
         end if;
         if sw_ctrl.run and sw_ctrl.start then
            CPU.all.run;
         else
--            process_mode_ctrl(BBS.Sim_CPU.PROC_KERN, BBS.Sim_CPU.ADDR_INST, sr_ctl);
            if pvt_deposit then
               CPU.all.deposit;
            elsif pvt_examine then
               CPU.all.examine;
            end if;
         end if;
         lr_ctrl := CPU.all.get_lr_ctrl;
         lr_ctrl.ready := True;
         if CPU.all.halted then
            lr_ctrl.run   := False;
            lr_ctrl.start := False;
         else
            lr_ctrl.run   := sw_ctrl.run;
            lr_ctrl.start := sw_ctrl.start;
         end if;
         lr_ctrl.exam := sw_ctrl.exam;
         lr_ctrl.dep  := sw_ctrl.dep;
         lr_ctrl.addr := sw_ctrl.addr;
         if sw_ctrl.addr then
            lr_ad := CPU.all.get_lr_addr;
         else
            lr_ad := CPU.all.get_lr_data;
         end if;
         i2c.set_addr_data(lr_ad, res);
         i2c.set_ctrl(lr_ctl, res);
         --
         --  Reset change flags
         --
         pvt_deposit := False;
         pvt_examine := False;
         pvt_starting := False;
         exit when exit_sim;
      end loop;
      i2c.set_addr_data(0, res);
      i2c.set_ctrl(0, res);
   end run;
   --
   --  --------------------------------------------------------------------
   --
   --  Initializations for different simulations
   --
   procedure init_sim_example is
   begin
     sim_example.start;
   end;
   --
   procedure init_sim_8080 is
   begin
     sim_8080.init;
     sim_8080.attach_io(tel'Access, 0, BBS.Sim_CPU.BUS_IO);
     tel.setOwner(cpu);
     tel.init(tel'Access, 2171);
     sim_8080.attach_io(fd'Access, 4, BBS.Sim_CPU.BUS_IO);
     fd.setOwner(sim_8080'Access);
     fd.open(0, floppy_ctrl.hd_geom, "test.img");
     fd.open(1, floppy_ctrl.floppy8_geom, "user.img");
     fd.open(2, floppy_ctrl.floppy8_geom, "fortran.img");
     fd.open(3, floppy_ctrl.floppy8_geom, "mbasic.img");
     sim_8080.load("boot.ihx");
     sim_8080.start(0);
   end;
   --
   procedure init_sim_68000 is
   begin
      sim_68000.init;
      sim_68000.attach_io(clock'Access, 16#400#, BBS.Sim_CPU.BUS_MEMORY);
      clock.setOwner(CPU);
      BBS.Sim_CPU.Clock.setBaseRate(1.0);
      clock.init(clock'Access);
      clock.setException(256+64);
      sim_68000.attach_io(tel0'Access, 16#402#, BBS.Sim_CPU.BUS_MEMORY);
      tel0.setOwner(CPU);
      tel0.init(tel0'Access, 2171);
      tel0.setException(2*256+65);
      sim_68000.attach_io(tel1'Access, 16#404#, BBS.Sim_CPU.BUS_MEMORY);
      tel1.setOwner(CPU);
      tel1.init(tel1'Access, 2172);
      tel1.setException(2*256+66);
      sim_68000.attach_io(tel2'Access, 16#406#, BBS.Sim_CPU.BUS_MEMORY);
      tel2.setOwner(CPU);
      tel2.init(tel2'Access, 2173);
      tel2.setException(2*256+67);
      sim_68000.attach_io(mux'Access, 16#408#, BBS.Sim_CPU.BUS_MEMORY);
      mux.setOwner(CPU);
      mux.init(mux'Access, 3141);
      mux.setException(2*256+68);
      sim_68000.load("Tasks.S");
      sim_68000.load("OS68k.S");
      sim_68000.start(16#2000#);
   end;
   --
   --  Process the control switches that have action based on a transition to
   --  the True state.
   --
   procedure process_switch is
   begin
      if not ctl_start then
         if sw_ctrl.start then
            pvt_starting := True;
         end if;
      end if;
      if not ctl_dep then
         if sw_ctrl.dep then
            pvt_deposit := True;
         end if;
      end if;
      if not ctl_exam then
         if sw_ctrl.exam then
            pvt_examine := True;
         end if;
      end if;
      ctl_start := sw_ctrl.start;
      ctl_dep   := sw_ctrl.dep;
      ctl_exam  := sw_ctrl.exam;
   end;
   --
   --  Process the mode and control LEDs
   --
   procedure process_mode_ctrl(m : BBS.Sim_CPU.proc_mode; a : BBS.Sim_CPU.addr_type; c : BBS.uint16) is
   begin
      lr_ctl := c;
      lr_ctrl.ready := True;
      lr_ctrl.mode  := m;
      lr_ctrl.atype := a;
   end;
   --
   --  Get and set the selected test pattern.  These will probably need to change
   --  if any simulators beyond the simple example sim get added.
   --
   procedure set_pattern(p : Natural) is
   begin
      CPU.all.set_mem(0, BBS.uint32(p));
   end;
   --
   function get_pattern return Natural is
   begin
      return Natural(CPU.all.read_mem(0));
   end;
   --
end;
