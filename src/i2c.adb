with Ada.Text_IO;
with BBS.embed;
use type BBS.embed.addr7;
with BBS.embed.i2c;
use type BBS.embed.i2c.err_code;
with BBS.embed.RPI;
package body i2c is

   procedure init_i2c is
      err  : BBS.embed.i2c.err_code;
      temp : BBS.embed.uint8;
      pragma Unreferenced(temp);
      addr : BBS.embed.addr7;
      i2c_bus : constant BBS.embed.i2c.i2c_interface := BBS.embed.i2c.i2c_interface(i2c_ptr);
   begin
      i2c_rec.configure(BBS.embed.RPI.I2C_1);
      --
      --  Look for MCP23017 devices on the bus.
      --
      for i in MCP23017_found'range loop
         addr := BBS.embed.i2c.MCP23017.addr_0 + BBS.embed.addr7(i);
         temp := i2c_rec.read(addr, BBS.embed.i2c.MCP23017.IOCON, err);
         if err = BBS.embed.i2c.NONE then
            MCP23017_info(i).configure(i2c_bus, addr, err);
            MCP23017_found(i) := True;
            Ada.Text_IO.put_line("MCP23017(" & Integer'Image(i) &
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
            Ada.Text_IO.put_line("MCP23017(" & Integer'Image(i) &
                ") Not found at address " & Integer'image(Integer(addr))
                & " - " & BBS.embed.i2c.err_code'Image(err));
            MCP23017_found(i) := False;
         end if;
      end loop;
   end;
   --
 end;
