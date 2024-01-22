with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with BBS;
--use type BBS.int16;
use type BBS.uint16;
use type BBS.uint32;
with BBS.embed;
use type BBS.embed.addr7;
with BBS.embed.i2c;
use type BBS.embed.i2c.err_code;
with BBS.embed.RPI;
package body i2c is
   --
   --  Configure the I2C bus and search for MCP23017 devices.  Other devices may
   --  be added at some time.
   --
   procedure init_i2c is
      err  : BBS.embed.i2c.err_code;
      temp : BBS.uint8;
      pragma Unreferenced(temp);
      addr : BBS.embed.addr7;
      i2c_bus : constant BBS.embed.i2c.i2c_interface := BBS.embed.i2c.i2c_interface(i2c_ptr);
      function MCP23017_addr is new Ada.Unchecked_Conversion(Source => MCP23017_use,
                                                             Target => BBS.embed.addr7);
   begin
      i2c_rec.configure(BBS.embed.RPI.I2C_1);
      --
      --  Look for MCP23017 devices on the bus.
      --
      for i in MCP23017_use loop
         addr := MCP23017_addr(i);
         temp := i2c_rec.read(addr, BBS.embed.i2c.MCP23017.IOCON, err);
         if err = BBS.embed.i2c.NONE then
            MCP23017_info(i).configure(i2c_bus, addr, err);
            MCP23017_found(i) := True;
            Ada.Text_IO.put_line("MCP23017(" & MCP23017_use'Image(i) &
               ") Found at address " & Integer'image(Integer(addr)));
            MCP23017_info(i).set_dir(16#FFFF#, err);
            if err /= BBS.embed.i2c.NONE then
               Ada.Text_IO.Put("  Set dir ");
               Ada.Text_IO.put_line("failed: " & BBS.embed.i2c.err_code'Image(err));
            end if;
            MCP23017_info(i).set_pullup(16#FFFF#, err);
            if err /= BBS.embed.i2c.NONE then
               Ada.Text_IO.Put("  Set pullup ");
               Ada.Text_IO.put_line("failed: " & BBS.embed.i2c.err_code'Image(err));
            end if;
         else
            Ada.Text_IO.put_line("MCP23017(" & MCP23017_use'Image(i) &
                ") Not found at address " & Integer'image(Integer(addr))
                & " - " & BBS.embed.i2c.err_code'Image(err));
            MCP23017_found(i) := False;
         end if;
      end loop;
   end;
   --
   --  Read switches and write LEDs
   --
   procedure read_addr_data(d : out BBS.uint32; res : out result) is
      err : BBS.embed.i2c.err_code;
      lsw : BBS.uint16 := 0;
      msw : BBS.uint16 := 0;
   begin
      d := 0;
      if MCP23017_found(SW_LSW) then
         lsw := MCP23017_info(SW_LSW).get_data(err);
         if err /= BBS.embed.i2c.none then
            res := RES_ERR;
            return;
         end if;
      end if;
      if MCP23017_found(SW_MSW) then
         msw := MCP23017_info(SW_MSW).get_data(err);
         if err /= BBS.embed.i2c.none then
            res := RES_ERR;
            return;
         end if;
      end if;
      d := BBS.uint32(msw)*16#10000# + BBS.uint32(lsw);
      if MCP23017_found(LED_LSW) and MCP23017_found(LED_MSW) then
         res := RES_FULL;
      elsif MCP23017_found(LED_LSW) then
         res := RES_LSW;
      elsif MCP23017_found(LED_MSW) then
         res := RES_MSW;
      else
         res := RES_NONE;
      end if;
   end;
   --
   procedure set_addr_data(d : BBS.uint32; res : out result) is
      err : BBS.embed.i2c.err_code;
      lsw : constant BBS.uint16 := BBS.uint16(d and 16#FFFF#);
      msw : constant BBS.uint16 := BBS.uint16((d/16#10000#) and 16#FFFF#);
   begin
      if last_lsw_led /= lsw then
         if MCP23017_found(LED_LSW) then
            MCP23017_info(LED_LSW).set_data(lsw, err);
            if err /= BBS.embed.i2c.none then
               res := RES_ERR;
               return;
            end if;
         end if;
         last_lsw_led := lsw;
      end if;
      if last_msw_led /= msw then
         if MCP23017_found(LED_MSW) then
            MCP23017_info(LED_MSW).set_data(msw, err);
            if err /= BBS.embed.i2c.none then
               res := RES_ERR;
               return;
            end if;
         end if;
         last_msw_led := msw;
      end if;
      if MCP23017_found(LED_LSW) and MCP23017_found(LED_MSW) then
         res := RES_FULL;
      elsif MCP23017_found(LED_LSW) then
         res := RES_LSW;
      elsif MCP23017_found(LED_MSW) then
         res := RES_MSW;
      else
         res := RES_NONE;
      end if;
   end;
   --
   --  Read control switches and write control/mode LEDs
   --
   procedure read_ctrl(d : out BBS.uint16; res : out result) is
      err : BBS.embed.i2c.err_code;
      temp : BBS.uint16;
   begin
      d := 0;
      if MCP23017_found(SW_CTRL) then
         temp := MCP23017_info(SW_CTRL).get_data(err);
         if err /= BBS.embed.i2c.none then
            res := RES_ERR;
         else
            d := temp;
            res := RES_FULL;
         end if;
      else
         res := RES_NONE;
      end if;
   end;
   --
   procedure set_ctrl(d : BBS.uint16; res : out result) is
      err : BBS.embed.i2c.err_code;
   begin
     if MCP23017_found(LED_CTRL) then
       if last_ctrl_led /= d then
         MCP23017_info(LED_CTRL).set_data(d, err);
         if err /= BBS.embed.i2c.none then
           res := RES_ERR;
         else
           res := RES_FULL;
         end if;
       else
         res := RES_FULL;
       end if;
      last_ctrl_led := d;
     else
       res := RES_NONE;
     end if;
   end;
   --
end;
