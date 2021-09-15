with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with BBS.embed;
use type BBS.embed.addr7;
use type BBS.embed.uint16;
with BBS.embed.i2c;
use type BBS.embed.i2c.err_code;
with BBS.embed.RPI;
with patterns;
package body i2c is

   procedure init_i2c is
      err  : BBS.embed.i2c.err_code;
      temp : BBS.embed.uint8;
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
   --  Run the LED patterns
   --
   task body run_patterns is
      package Hex_IO is new Ada.Text_IO.Integer_IO(Integer);
      data      : BBS.embed.uint16 := 0;
      last_data : BBS.embed.uint16 := 0;
      err       : BBS.embed.i2c.err_code;
   begin
      accept Start;
      Ada.Text_IO.Put_Line("Setting LED ports to output and displaying data");
      MCP23017_info(LED_LSW).set_dir(16#0000#, err);
      loop
         data := MCP23017_info(SWITCH_LSW).get_data(err);
         if last_data /= data then
            Ada.Text_IO.Put("Value change from ");
            Hex_IO.Put(Integer(last_data));
            Ada.Text_IO.Put(" to ");
            Hex_IO.Put(Integer(data));
            Ada.Text_IO.New_Line;
            last_data := data;
         end if;
         case data is
         when 1 =>
            patterns.count(0.1);
         when 2 =>
            patterns.scan(0.05);
         when 3 =>
            patterns.bounce(0.05);
         when 4 =>
            patterns.fibonacci(0.05);
         when others =>
            MCP23017_info(LED_LSW).set_data(data, err);
         end case;
      end loop;
   end;
   --
end;
