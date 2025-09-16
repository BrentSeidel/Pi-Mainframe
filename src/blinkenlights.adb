with Ada.Exceptions;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with BBS;
use type BBS.uint32;
with BBS.Sim_CPU;
with BBS.Sim_CPU.bus;
with BBS.Sim_CPU.bus.mem8;
with BBS.Sim_CPU.CPU.example;
with BBS.Sim_CPU.CPU.i8080;
with BBS.Sim_CPU.CPU.m68000;
with BBS.embed;
with BBS.embed.i2c;
use type BBS.embed.i2c.err_code;
with i2c;
with Panel;
with BBS.web;
with BBS.web.server;
--
--  The main routine does some initialization and then starts the
--  simulation and web server.
--
procedure blinkenlights is
   package Hex_IO is new Ada.Text_IO.Integer_IO(Integer);
   data : BBS.uint16;
   err  : BBS.embed.i2c.err_code;
   res  : i2c.result;
   selection : Integer := 0;
   internal_map : BBS.web.proc_tables.Map;
begin
   Ada.Text_IO.Put_Line("Raspberry Pi based mainframe simulator");
   loop
      Ada.Text_IO.Put_Line("Available simulators are:");
      Ada.Text_IO.Put_Line("1. Blinkenlights");
      Ada.Text_IO.Put_Line("2. Zilog Z-80");
      Ada.Text_IO.Put_Line("3. Motorola 68000");
      Ada.Text_IO.Put("Selection: ");
      Ada.Integer_Text_IO.Get(selection, 0);
      --
      --  This is just to clear out any text on the rest of the line.
      --
      declare
         dummy : String := Ada.Text_IO.Get_line;
      begin
         null;  --  Nothing to do here.
      end;
      exit when (selection > 0) and (selection < 4);
   end loop;
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
   case selection is
      when 1 =>
         Panel.cpu := new BBS.Sim_CPU.CPU.example.simple;
--         Panel.CPU := Panel.sim_example'Access;
         Panel.init_sim_example;
         Ada.Text_IO.Put_Line("Blinkenlights selected.");
      when 2 =>
         Panel.cpu := new BBS.Sim_CPU.CPU.i8080.i8080;
         Panel.bus := new BBS.Sim_CPU.bus.mem8.mem8io(2**16);
         Panel.cpu.attach_bus(Panel.bus, 1);
--         Panel.CPU := Panel.sim_8080'Access;
--         Panel.sim_8080.variant(2);
--         Panel.init_sim_8080;
         Panel.cpu.variant(2);
         Panel.init_sim_8080;
         Ada.Text_IO.Put_Line("Zilog Z-80 selected.");
      when 3 =>
         Panel.cpu := new BBS.Sim_CPU.CPU.m68000.m68000;
         Panel.bus := new BBS.Sim_CPU.bus.mem8.mem8mem(2**24);
         Panel.cpu.attach_bus(Panel.bus, 1);
         Panel.cpu.variant(0);
--         Panel.CPU := Panel.sim_68000'Access;
         Panel.init_sim_68000;
         Ada.Text_IO.Put_Line("Motorola 68000 selected.");
      when others =>  --  Should never happen
         null;
   end case;
   Panel.run.Start;
   BBS.web.server.server(internal_map, "web/config.txt", 31415);
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
end blinkenlights;
