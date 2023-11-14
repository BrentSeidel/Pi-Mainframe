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
   type MCP23017_use is (LED_LSW, SW_LSW, LED_MSW, SW_MSW, LED_CTRL, SW_CTRL,
                         SPARE_1, SPARE_2);
   for MCP23017_use use (LED_LSW  => BBS.embed.i2c.MCP23017.addr_0,
                         SW_LSW   => BBS.embed.i2c.MCP23017.addr_1,
                         LED_MSW  => BBS.embed.i2c.MCP23017.addr_2,
                         SW_MSW   => BBS.embed.i2c.MCP23017.addr_3,
                         LED_CTRL => BBS.embed.i2c.MCP23017.addr_4,
                         SW_CTRL  => BBS.embed.i2c.MCP23017.addr_5,
                         SPARE_1  => BBS.embed.i2c.MCP23017.addr_6,
                         SPARE_2  => BBS.embed.i2c.MCP23017.addr_7);
   for MCP23017_use'Size use 7;
   --
   --  Control data for MCP23017 devices
   --
   MCP23017_found  : array (MCP23017_use) of boolean;
   MCP23017_info : array (MCP23017_use) of aliased BBS.embed.i2c.MCP23017.MCP23017_record;
   --
   --  Result type
   --    RES_FULL - Full results
   --    RES_LSW  - Only least significant 16 bits provided (msw not installed)
   --    RES_MSW  - Only most significant 16 bits provided (lsw not installed)
   --    RES_NONE - No data provided (h/w not installed)
   --    RES_ERR  - I2C error occured
   --
   type result is (RES_FULL, RES_LSW, RES_MSW, RES_NONE, RES_ERR);
   --
   --  I2C Routines.
   --
   procedure init_i2c;
   --
   --  Read switches and write LEDs
   --
   procedure set_addr_data(d : BBS.embed.uint32; res : out result);
   procedure read_addr_data(d : out BBS.embed.uint32; res : out result);
   procedure set_ctrl(d : BBS.embed.uint16; res : out result);
   procedure read_ctrl(d : out BBS.embed.uint16; res : out result);
   --
private
   --
   --  Private data
   --
   --  I2C device record
   --
   i2c_rec : aliased BBS.embed.i2c.linux.linux_i2c_interface_record;
   i2c_ptr : BBS.embed.i2c.linux.linux_i2c_interface := i2c_rec'access;
   --
   --  Last LED values.  This is used to avoid an I2C bus request to set the
   --  LEDs to their current state.
   --
   last_ctrl_led : BBS.embed.uint16 := 0;
   last_msw_led  : BBS.embed.uint16 := 1;  --  This will cause the MSW to be cleared on 16 bit systems
   last_lsw_led  : BBS.embed.uint16 := 0;
end;
