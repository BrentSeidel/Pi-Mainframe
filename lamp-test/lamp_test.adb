with Ada.Text_IO;
with BBS;
use type BBS.uint16;
use type BBS.uint32;
with BBS.embed.i2c;
with i2c;

--
--  This is a routine to test the simulator LEDs and switches.
--
procedure lamp_test is
   ad_data  : BBS.uint32;
   ctl_data : BBS.uint16;
   res      : i2c.result;
   err      : BBS.embed.i2c.err_code;
   --
   --  Set all LEDS, mirror the switches, then turn LEDs off
   --
   procedure mirror is
   begin
     --
     --  Set all the LEDs on and wait for 5 seconds.
     --
     i2c.set_addr_data(16#FFFF_FFFF#, res);
     Ada.Text_IO.Put_Line("Set Addr/Data LEDs result is " & i2c.result'Image(res));
     i2c.set_ctrl(16#FFFF#, res);
     Ada.Text_IO.Put_Line("Set Mode/Ctl LEDs result is " & i2c.result'Image(res));
     delay 5.0;
     --
     --  Read the switch values
     --
     i2c.read_addr_data(ad_data, res);
     Ada.Text_IO.Put_Line("Read Addr/Data switches result is " & i2c.result'Image(res));
     Ada.Text_IO.Put_Line("  Addr/Data switch value is " & ad_data'Image);
     i2c.read_ctrl(ctl_data, res);
     Ada.Text_IO.Put_Line("Read control switches result is " & i2c.result'Image(res));
     Ada.Text_IO.Put_Line("  Control switch value is " & ctl_data'Image);
     --
     --  Set the LEDs to the switch values and wait for 5 seconds
     --
     i2c.set_addr_data(ad_data, res);
     Ada.Text_IO.Put_Line("Set Addr/Data LEDs result is " & i2c.result'Image(res));
     i2c.set_ctrl(ctl_data, res);
     Ada.Text_IO.Put_Line("Set Mode/Ctl LEDs result is " & i2c.result'Image(res));
     delay 5.0;
     --
     --  Turn the LEDs off.
     --
     i2c.set_addr_data(0, res);
     Ada.Text_IO.Put_Line("Set Addr/Data LEDs result is " & i2c.result'Image(res));
     i2c.set_ctrl(0, res);
     Ada.Text_IO.Put_Line("Set Mode/Ctl LEDs result is " & i2c.result'Image(res));
   end;
   --
   --  Sweep LEDs
   --
   procedure sweep is
     addr_lsw : BBS.uint32 := 1;
     addr_msw : BBS.uint32 := 1;
     mode_ctl : BBS.uint16 := 1;
   begin
     loop
       i2c.set_addr_data(addr_msw * 16#0001_0000# + addr_lsw, res);
       i2c.set_ctrl(mode_ctl, res);
       delay 0.1;
       if addr_lsw >= 16#8000# then
         addr_lsw := 1;
         addr_msw := 1;
         mode_ctl := 1;
       else
         addr_lsw := addr_lsw * 2;
         addr_msw := addr_msw * 2;
         mode_ctl := mode_ctl * 2;
       end if;
     end loop;
   end;
begin
   i2c.init_i2c;
   --
   -- Configure LED drivers
   --
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
   --  Select pattern for testing
   --
   if True then
     mirror;
   else
     sweep;
   end if;
end lamp_test;
