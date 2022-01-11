with Ada.Text_IO;
with Ada.Unchecked_Conversion;
package body Sim is
   --
   --  Run the LED patterns
   --
   task body run is
      package Hex_IO is new Ada.Text_IO.Integer_IO(Integer);
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
         Ada.Text_IO.Put_Line("LED least significant word configured");
      end if;
      if i2c.MCP23017_found(i2c.LED_MSW) then
         i2c.MCP23017_info(i2c.LED_MSW).set_dir(16#0000#, err);
         Ada.Text_IO.Put_Line("LED most significant word configured");
      end if;
      if i2c.MCP23017_found(i2c.LED_CTRL) then
         i2c.MCP23017_info(i2c.LED_CTRL).set_dir(16#0000#, err);
         Ada.Text_IO.Put_Line("LED mode and control configured");
      end if;
      --
      --  Processing loop
      --
      loop
         i2c.read_addr_data(sr_ad, res);
         i2c.read_ctrl(sr_ctl, res);
         process_ctrl;
         if ctl_starting then
            init_test;
         end if;
         if sw_ctrl.run and sw_ctrl.start then
--         if ctl_run and ctl_start then
            case pattern is
               when 1 =>
                  count(0.1);
               when 2 =>
                  scan16(0.05);
               when 3 =>
                  bounce16(0.05);
               when 4 =>
                  fibonacci(0.05);
               when 9 =>
                  count(0.1);
               when 10 =>
                  scan32(0.05);
               when 11 =>
                  bounce32(0.05);
               when 12 =>
                  fibonacci(0.05);
               when others =>
                  copy_sw(0.01);
            end case;
         else
            process_mode_ctrl(PROC_USER, ADDR_DATA, sr_ctl);
            if ctl_deposit then
               pattern := sr_ad;
            end if;
            copy_sw_ad;
         end if;
         --
         --  Reset change flags
         --
         pvt_deposit := False;
         pvt_examine := False;
         pvt_starting := False;
         exit when exit_sim;
      end loop;
      i2c.set_addr_data(0, res);
      i2c.set_ctrl(0, res);
   end run;
   --
   --  Code for the various patterns.
   --
   procedure count(d : Duration) is
   begin
      ad_counter := ad_counter + 1;
      ctl_counter := ctl_counter + 2;
      i2c.set_addr_data(ad_counter, res);
      i2c.set_ctrl(ctl_counter, res);
      lr_ad := ad_counter;
      lr_ctl := ctl_counter;
      delay d;
   end;
   --
   procedure bounce16(d : Duration) is
   begin
      if ad_bounce_dir = left then
         if (ad_bouncer and 16#FFFF#) = 0 then
            ad_bounce_dir := right;
            ad_bouncer := 16#8000#;
         else
            ad_bouncer := ad_bouncer * 2;
         end if;
      else
         if (ad_bouncer and 16#FFFF#) = 0 then
            ad_bounce_dir := left;
            ad_bouncer := 16#0001#;
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
   procedure bounce32(d : Duration) is
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
   procedure scan16(d : Duration) is
   begin
      if (ad_scanner and 16#FFFF#) = 0 then
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
   procedure scan32(d : Duration) is
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
   procedure copy_sw_ad is
   begin
      i2c.set_addr_data(sr_ad, res);
      lr_ad := sr_ad;
   end;
   --
   procedure copy_sw_ctl is
   begin
      i2c.set_ctrl(sr_ctl, res);
      lr_ctl := sr_ctl;
   end;
   --
   procedure copy_sw(d : Duration) is
   begin
      copy_sw_ad;
      copy_sw_ctl;
      delay d;
   end;
   --  --------------------------------------------------------------------
   --
   --  Initialize the various test patterns to their initial state
   --
   procedure init_test is
   begin
      ad_counter     := 0;
      ctl_counter    := 0;
      ad_bouncer     := 0;
      ad_bounce_dir  := left;
      ctl_bouncer    := 0;
      ctl_bounce_dir := left;
      ad_scanner     := 0;
      ctl_scanner    := 0;
      ad_fib_1       := 1;
      ad_fib_2       := 1;
      ctl_fib_1      := 1;
      ctl_fib_2      := 2;
   end;
   --
   --  Process the control switches that have action based on a transition to
   --  the True state.
   --
   procedure process_ctrl is
   begin
      if not ctl_start then
         if ctl_start then
            pvt_starting := True;
         end if;
      end if;
      if not ctl_dep then
         if ctl_dep then
            pvt_deposit := True;
         end if;
      end if;
      if not ctl_exam then
         if ctl_exam then
            pvt_examine := True;
         end if;
      end if;
      ctl_start := sw_ctrl.start;
      ctl_dep   := sw_ctrl.dep;
      ctl_exam  := sw_ctrl.exam;
   end;
   --
   --  Process the mode and control LEDs
   --
   procedure process_mode_ctrl(m : proc_mode; a : addr_type; c : BBS.embed.uint16) is
      data : aliased BBS.embed.uint16 := c;
      reg : ctrl_mode with address => data'Address;
   begin
      reg.ready := True;
      reg.mode  := m;
      reg.atype := a;
      i2c.set_ctrl(data, res);
   end;
   --
   --  Get and set the selected test pattern
   --
   procedure set_pattern(p : Natural) is
   begin
      pattern := BBS.embed.uint32(p);
   end;
   --
   function get_pattern return Natural is
   begin
      return Natural(pattern);
   end;
   --
end;
