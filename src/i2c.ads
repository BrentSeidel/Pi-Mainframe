with BBS.embed.i2c.linux;
with BBS.embed.i2c.MCP23017;
package i2c is
   --
   --  Info for I2C devices
   --
   MCP23017_found  : array (0 .. 7) of boolean;
   --
   MCP23017_info : array (0 .. 7) of aliased BBS.embed.i2c.MCP23017.MCP23017_record;
   --
   --  I2C Routines.
   --
   procedure init_i2c;
   --
   -- Constants
   --
   SWITCH_INDEX : constant Integer := 1;
   LED_INDEX    : constant Integer := 0;
private
   --
   --  Private data
   --
   --  I2C device record
   --
   i2c_rec : aliased BBS.embed.i2c.linux.linux_i2c_interface_record;
   i2c_ptr : BBS.embed.i2c.linux.linux_i2c_interface := i2c_rec'access;
end;
