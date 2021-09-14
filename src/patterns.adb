with i2c;
package body patterns is
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
