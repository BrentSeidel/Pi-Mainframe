with Ada.Text_IO;
with i2c;
package body patterns is
   --
   --  Run the LED patterns
   --
   task body run is
      package Hex_IO is new Ada.Text_IO.Integer_IO(Integer);
      data      : BBS.embed.uint16 := 0;
      last_data : BBS.embed.uint16 := 0;
   begin
      accept Start;
      Ada.Text_IO.Put_Line("Setting LED ports to output and displaying data");
      i2c.MCP23017_info(i2c.LED_LSW).set_dir(16#0000#, err);
      loop
         data := i2c.MCP23017_info(i2c.SWITCH_LSW).get_data(err);
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
   end run;
   --
   --  Code for the various patterns.
   --
   procedure count(d : Duration) is
   begin
      counter := counter + 1;
      i2c.MCP23017_info(i2c.LED_LSW).set_data(counter, err);
      delay d;
   end;
   --
   procedure bounce(d : Duration) is
   begin
      if bounce_dir = left then
         if bouncer = 0 then
            bounce_dir := right;
            bouncer := 16#8000#;
         else
            bouncer := bouncer * 2;
         end if;
      else
         if bouncer = 0 then
            bounce_dir := left;
            bouncer := 16#0001#;
         else
            bouncer := bouncer / 2;
         end if;
      end if;
      i2c.MCP23017_info(i2c.LED_LSW).set_data(bouncer, err);
      delay d;
   end;
   --
   procedure scan(d : Duration) is
   begin
      if scanner = 0 then
         scanner := 1;
      else
         scanner := scanner * 2;
      end if;
      i2c.MCP23017_info(i2c.LED_LSW).set_data(scanner, err);
      delay d;
   end;
   --
   procedure fibonacci(d : Duration) is
      temp : constant BBS.embed.uint16 := fib_1 + fib_2;
   begin
      i2c.MCP23017_info(i2c.LED_LSW).set_data(temp, err);
      fib_2 := fib_1;
      fib_1 := temp;
      if (fib_1 = 0) and (fib_2 = 0) then
         fib_1 := 1;
         fib_2 := 1;
      end if;
      delay d;
   end;
   --
end;
