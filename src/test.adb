with Ada.Text_IO;
with BBS.embed;
with BBS.embed.i2c;
use type BBS.embed.i2c.err_code;
with i2c;
with Sim;
with web;
--
--  This is a simple shell routine to call the embedded lisp interpreter.
--
procedure Test is
   package Hex_IO is new Ada.Text_IO.Integer_IO(Integer);
   data : BBS.embed.uint16;
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
   Sim.run.Start;
   web.start_server;
end Test;
