with BBS.embed;
with i2c;
use type i2c.result;
--
--  This is a routine to test the simulator LEDs and switches.
--
procedure Lamp is
   ad_data  : BBS.embed.uint32;
   ctl_data : BBS.embed.uint16;
   res      : i2c.result;
begin
   i2c.init_i2c;
   --
   --  Set all the LEDs on and wait for 5 seconds.
   --
   i2c.set_addr_data(16#FFFF_FFFF#, res);
   if res = i2c.RES_ERR then
      return;
   end if;
   i2c.set_ctrl(16#FFFF#, res);
   if res = i2c.RES_ERR then
      return;
   end if;
   delay 5.0;
   --
   --  Read the switch values
   --
   i2c.read_addr_data(ad_data, res);
   if res = i2c.RES_ERR then
      return;
   end if;
   i2c.read_ctrl(ctl_data, res);
   if res = i2c.RES_ERR then
      return;
   end if;
   --
   --  Set the LEDs to the switch values and wait for 5 seconds
   --
   i2c.set_addr_data(ad_data, res);
   if res = i2c.RES_ERR then
      return;
   end if;
   i2c.set_ctrl(ctl_data, res);
   if res = i2c.RES_ERR then
      return;
   end if;
   delay 5.0;
   --
   --  Turn the LEDs off.
   --
   i2c.set_addr_data(0, res);
   if res = i2c.RES_ERR then
      return;
   end if;
   i2c.set_ctrl(0, res);
   if res = i2c.RES_ERR then
      return;
   end if;
end Lamp;
