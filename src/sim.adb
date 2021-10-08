with Ada.Text_IO;
package body Sim is
   --
   --  Run the LED patterns
   --
   task body run is
      package Hex_IO is new Ada.Text_IO.Integer_IO(Integer);
      data      : BBS.embed.uint32 := 0;
      last_data : BBS.embed.uint32 := 0;
   begin
      accept Start;
      --
      --  Configure MCP23017 devices
      --
      Ada.Text_IO.Put_Line("Setting LED ports to output and displaying data");
      i2c.MCP23017_info(i2c.LED_LSW).set_dir(16#0000#, err);
      Hex_IO.Default_Base := 16;
      Hex_IO.Default_Width := 1;
      if i2c.MCP23017_found(i2c.LED_LSW) then
         i2c.MCP23017_info(i2c.LED_LSW).set_dir(16#0000#, err);
         Ada.Text_IO.Put_Line("LED least significant word condigured");
      end if;
      if i2c.MCP23017_found(i2c.LED_MSW) then
         i2c.MCP23017_info(i2c.LED_MSW).set_dir(16#0000#, err);
         Ada.Text_IO.Put_Line("LED most significant word condigured");
      end if;
      if i2c.MCP23017_found(i2c.LED_CTRL) then
         i2c.MCP23017_info(i2c.LED_CTRL).set_dir(16#0000#, err);
         Ada.Text_IO.Put_Line("LED mode and control condigured");
      end if;
      if i2c.MCP23017_found(i2c.LED_LSW) and i2c.MCP23017_found(i2c.SW_LSW) then
         loop
            i2c.read_addr_data(data, res);
            sr_ad := data;
            if last_data /= data then
               Ada.Text_IO.Put("Value change from ");
               Hex_IO.Put(Integer(last_data));
               Ada.Text_IO.Put(" to ");
               Hex_IO.Put(Integer(data));
               Ada.Text_IO.New_Line;
               last_data := data;
            end if;
            if not auto_man then
               pattern := Natural(data);
            end if;
            case pattern is
            when 1 =>
               count(0.1);
            when 2 =>
               scan(0.05);
            when 3 =>
               bounce(0.05);
            when 4 =>
               fibonacci(0.05);
            when others =>
               i2c.set_addr_data(data, res);
               i2c.set_ctrl(BBS.embed.uint16(data and 16#FFFF#), res);
               lr_ad := data;
            end case;
         end loop;
      else
         Ada.Text_IO.Put_Line("Minimal required hardware not present for simulator.");
      end if;
   end run;
   --
   --  Code for the various patterns.
   --
   procedure count(d : Duration) is
   begin
      ad_counter := ad_counter + 1;
      ctl_counter := ctl_counter + 1;
      i2c.set_addr_data(ad_counter, res);
      i2c.set_ctrl(ctl_counter, res);
      lr_ad := ad_counter;
      lr_ctl := ctl_counter;
      delay d;
   end;
   --
   procedure bounce(d : Duration) is
   begin
      if ad_bounce_dir = left then
         if ad_bouncer = 0 then
            ad_bounce_dir := right;
            ad_bouncer := 16#8000_0000#;
         else
            ad_bouncer := ad_bouncer * 2;
         end if;
      else
         if ad_bouncer = 0 then
            ad_bounce_dir := left;
            ad_bouncer := 16#0000_0001#;
         else
            ad_bouncer := ad_bouncer / 2;
         end if;
      end if;
      if ctl_bounce_dir = left then
         if ctl_bouncer = 0 then
            ctl_bounce_dir := right;
            ctl_bouncer := 16#8000#;
         else
            ctl_bouncer := ctl_bouncer * 2;
         end if;
      else
         if ctl_bouncer = 1 then
            ctl_bounce_dir := left;
            ctl_bouncer := 16#0002#;
         else
            ctl_bouncer := ctl_bouncer / 2;
         end if;
      end if;
      i2c.set_addr_data(ad_bouncer, res);
      lr_ad := ad_bouncer;
      i2c.set_ctrl(ctl_bouncer, res);
      lr_ctl := ctl_bouncer;
      delay d;
   end;
   --
   procedure scan(d : Duration) is
   begin
      if ad_scanner = 0 then
         ad_scanner := 1;
      else
         ad_scanner := ad_scanner * 2;
      end if;
      if ctl_scanner = 0 then
         ctl_scanner := 2;
      else
         ctl_scanner := ctl_scanner * 2;
      end if;
      i2c.set_addr_data(ad_scanner, res);
      lr_ad := ad_scanner;
      i2c.set_ctrl(ctl_scanner, res);
      lr_ctl := ctl_scanner;
      delay d;
   end;
   --
   procedure fibonacci(d : Duration) is
      ad_temp : constant BBS.embed.uint32 := ad_fib_1 + ad_fib_2;
      ctl_temp : constant BBS.embed.uint16 := ctl_fib_1 + ctl_fib_2;
   begin
      i2c.set_addr_data(ad_temp, res);
      i2c.set_ctrl(ctl_temp, res);
      lr_ad := ad_temp;
      ad_fib_2 := ad_fib_1;
      ad_fib_1 := ad_temp;
      lr_ctl := ctl_temp;
      ctl_fib_2 := ctl_fib_1;
      ctl_fib_1 := ctl_temp;
      if (ad_fib_1 = 0) and (ad_fib_2 = 0) then
         ad_fib_1 := 1;
         ad_fib_2 := 1;
      end if;
      if (ctl_fib_1 = 0) and (ctl_fib_2 = 0) then
         ctl_fib_1 := 1;
         ctl_fib_2 := 2;
      end if;
      delay d;
   end;
   --
end;
