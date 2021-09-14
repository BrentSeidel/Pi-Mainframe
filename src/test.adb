with Ada.Text_IO;
with BBS.embed;
use type BBS.embed.uint16;
with BBS.embed.i2c;
use type BBS.embed.i2c.err_code;
with i2c;
with patterns;
--
--  This is a simple shell routine to call the embedded lisp interpreter.
--
procedure Test is
   package Hex_IO is new Ada.Text_IO.Integer_IO(Integer);
   data : BBS.embed.uint16;
   last_data : BBS.embed.uint16;
   err  : BBS.embed.i2c.err_code;
begin
   i2c.init_i2c;
   Hex_IO.Default_Width := 4;
   Hex_IO.Default_Base  := 16;
   for i in i2c.MCP23017_use loop
      if i2c.MCP23017_found(i) then
         data := i2c.MCP23017_info(i).get_data(err);
         Ada.Text_IO.Put("MCP23017(" & i2c.MCP23017_use'Image(i) &
            ") read ");
         if err /= BBS.embed.i2c.NONE then
            Ada.Text_IO.put_line("failed: " &
               BBS.embed.i2c.err_code'Image(err));
         else
            Ada.Text_IO.Put("data: ");
            Hex_IO.put(Integer(data));
            Ada.Text_IO.New_Line;
         end if;
      end if;
   end loop;
   last_data := data;
   Ada.Text_IO.Put_Line("Setting LED ports to output and displaying data");
   i2c.MCP23017_info(i2c.LED_LSW).set_dir(16#0000#, err);
   loop
      data := i2c.MCP23017_info(i2c.SWITCH_MSW).get_data(err);
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
            i2c.MCP23017_info(i2c.LED_LSW).set_data(data, err);
      end case;
   end loop;
end Test;
