with Ada.Text_IO;
with Ada.Unchecked_Conversion;
package body Panel is
   --
   --  Run the LED patterns
   --
   task body run is
      package Hex_IO is new Ada.Text_IO.Integer_IO(Integer);
   begin
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
      --
      --  Processing loop
      --
      loop
         i2c.read_addr_data(pvt_sr_ad, res);
         i2c.read_ctrl(pvt_sr_ctl, res);
         process_switch;
         if ctl_starting then
            simulate.all.start;
         end if;
         if sw_ctrl.run and sw_ctrl.start then
            simulate.all.run;
         else
            process_mode_ctrl(BBS.Sim.PROC_KERN, BBS.Sim.ADDR_INTR, sr_ctl);
            if pvt_deposit then
               simulate.all.deposit;
            elsif pvt_examine then
               simulate.all.examine;
            end if;
         end if;
         if sw_ctrl.addr then
            lr_ad := lr_addr;
         else
            lr_ad := lr_data;
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
   procedure process_mode_ctrl(m : BBS.Sim.proc_mode; a : BBS.Sim.addr_type; c : BBS.embed.uint16) is
   begin
      lr_ctl := c;
      lr_ctrl.ready := True;
      lr_ctrl.mode  := m;
      lr_ctrl.blank := False;
      lr_ctrl.atype := a;
   end;
   --
   --  Get and set the selected test pattern.  These will probably need to change
   --  if any simulators beyond the simple example sim get added.
   --
   procedure set_pattern(p : Natural) is
   begin
      simulate.all.set_mem(0, BBS.embed.uint32(p));
   end;
   --
   function get_pattern return Natural is
   begin
      return Natural(simulate.all.read_mem(0));
   end;
   --
end;
