with BBS.embed.i2c.linux;
with BBS.embed.i2c.MCP23017;
package i2c is
   --
   --  Info for I2C devices
   --
   --
   --  Define type for the valid addresses for MCP23017 and their assigned uses.
   --  This documents the device addresses and is the only place they need to be
   --  declared.
   --
   type MCP23017_use is (LED_LSW, SWITCH_LSW, LED_MSW, SWITCH_MSW, CTRL, SPARE_1,
                         SPARE_2, SPARE_3);
   for MCP23017_use use (LED_LSW    => BBS.embed.i2c.MCP23017.addr_0,
                         SWITCH_LSW => BBS.embed.i2c.MCP23017.addr_1,
                         LED_MSW    => BBS.embed.i2c.MCP23017.addr_2,
                         SWITCH_MSW => BBS.embed.i2c.MCP23017.addr_3,
                         CTRL       => BBS.embed.i2c.MCP23017.addr_4,
                         SPARE_1    => BBS.embed.i2c.MCP23017.addr_5,
                         SPARE_2    => BBS.embed.i2c.MCP23017.addr_6,
                         SPARE_3    => BBS.embed.i2c.MCP23017.addr_7);
   for MCP23017_use'Size use 7;
   --
   --  Control data for MCP23017 devices
   --
   MCP23017_found  : array (MCP23017_use) of boolean;
   MCP23017_info : array (MCP23017_use) of aliased BBS.embed.i2c.MCP23017.MCP23017_record;
   --
   --  I2C Routines.
   --
   procedure init_i2c;
   --
   --  Run the LED patterns.  This is a task so that it can run in parallel with
   --  the web server.
   --
   task run_patterns is
      entry Start;
   end run_patterns;
   --
private
   --
   --  Private data
   --
   --  I2C device record
   --
   i2c_rec : aliased BBS.embed.i2c.linux.linux_i2c_interface_record;
   i2c_ptr : BBS.embed.i2c.linux.linux_i2c_interface := i2c_rec'access;
end;
