with Ada.Exceptions;
with Ada.Text_IO;
with BBS.embed;
with BBS.embed.i2c;
use type BBS.embed.i2c.err_code;
with i2c;
with Panel;
with web;
--
--  The main routine does some initialization and then starts the
--  simulation ands web server.
--
procedure Test is
   package Hex_IO is new Ada.Text_IO.Integer_IO(Integer);
   data : BBS.embed.uint16;
   err  : BBS.embed.i2c.err_code;
   res  : i2c.result;
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
   Panel.run.Start;
   web.start_server;
exception
   when error : others =>
      --
      --  Whatever else happens, turn the LEDs off.
      --
      i2c.set_addr_data(0, res);
      i2c.set_ctrl(0, res);
      Ada.Text_IO.Put_Line("Unexpected exeption in simulator: " &
                             Ada.Exceptions.Exception_Information(error));
      raise;
end Test;
