with Ada.Text_IO;
with BBS.embed;
with BBS.embed.i2c;
with i2c;

--
--  This is a routine to test the simulator LEDs and switches.
--
procedure Lamp is
   ad_data  : BBS.embed.uint32;
   ctl_data : BBS.embed.uint16;
   res      : i2c.result;
   err      : BBS.embed.i2c.err_code;
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
   i2c.read_ctrl(ctl_data, res);
   Ada.Text_IO.Put_Line("Read control switches result is " & i2c.result'Image(res));
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
end Lamp;
