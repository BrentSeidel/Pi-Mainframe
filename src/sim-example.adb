with Panel;
with BBS.embed;
use type BBS.embed.uint16;
use type BBS.embed.uint32;
package body Sim.example is
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out simple) is
   begin
      self.pattern := 0;
      self.ad_counter := 0;
      self.ctl_counter := 0;
      self.ad_bouncer := 0;
      self.ad_bounce_dir := left;
      self.ctl_bouncer := 0;
      self.ctl_bounce_dir := left;
      self.ad_scanner := 0;
      self.ctl_scanner := 0;
      self.ad_fib_1 := 1;
      self.ad_fib_2 := 1;
      self.ctl_fib_1 := 1;
      self.ctl_fib_2 := 2;
   end;
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out simple) is
      d : Duration := 0.05;
   begin
      case self.pattern is
         when 1 =>
            self.count;
         when 2 =>
            self.scan16;
         when 3 =>
            self.bounce16;
         when 4 =>
            self.fibonacci;
         when 9 =>
            self.count;
         when 10 =>
            self.scan32;
         when 11 =>
            self.bounce32;
         when 12 =>
            self.fibonacci;
         when others =>
            copy_sw;
            d := 0.01;
      end case;
      delay d;
   end;
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   overriding
   procedure deposit(self : in out simple) is
   begin
      self.pattern := Panel.sr_ad;
   end;
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
--   overriding
--   procedure examine(self : in out simple) is null;

   --
   --  Code for the various patterns.
   --
   procedure count(self : in out simple) is
   begin
      self.ad_counter := self.ad_counter + 1;
      self.ctl_counter := self.ctl_counter + 2;
      Panel.lr_ad := self.ad_counter;
      Panel.lr_ctl := self.ctl_counter;
   end;
   --
   procedure bounce16(self : in out simple) is
   begin
      if self.ad_bounce_dir = left then
         if (self.ad_bouncer and 16#FFFF#) = 0 then
            self.ad_bounce_dir := right;
            self.ad_bouncer := 16#8000#;
         else
            self.ad_bouncer := self.ad_bouncer * 2;
         end if;
      else
         if (self.ad_bouncer and 16#FFFF#) = 0 then
            self.ad_bounce_dir := left;
            self.ad_bouncer := 16#0001#;
         else
            self.ad_bouncer := self.ad_bouncer / 2;
         end if;
      end if;
      if self.ctl_bounce_dir = left then
         if self.ctl_bouncer = 0 then
            self.ctl_bounce_dir := right;
            self.ctl_bouncer := 16#8000#;
         else
            self.ctl_bouncer := self.ctl_bouncer * 2;
         end if;
      else
         if self.ctl_bouncer = 1 then
            self.ctl_bounce_dir := left;
            self.ctl_bouncer := 16#0002#;
         else
            self.ctl_bouncer := self.ctl_bouncer / 2;
         end if;
      end if;
      Panel.lr_ad := self.ad_bouncer;
      Panel.lr_ctl := self.ctl_bouncer;
   end;
   --
   procedure bounce32(self : in out simple) is
   begin
      if self.ad_bounce_dir = left then
         if self.ad_bouncer = 0 then
            self.ad_bounce_dir := right;
            self.ad_bouncer := 16#8000_0000#;
         else
            self.ad_bouncer := self.ad_bouncer * 2;
         end if;
      else
         if self.ad_bouncer = 0 then
            self.ad_bounce_dir := left;
            self.ad_bouncer := 16#0000_0001#;
         else
            self.ad_bouncer := self.ad_bouncer / 2;
         end if;
      end if;
      if self.ctl_bounce_dir = left then
         if self.ctl_bouncer = 0 then
            self.ctl_bounce_dir := right;
            self.ctl_bouncer := 16#8000#;
         else
            self.ctl_bouncer := self.ctl_bouncer * 2;
         end if;
      else
         if self.ctl_bouncer = 1 then
            self.ctl_bounce_dir := left;
            self.ctl_bouncer := 16#0002#;
         else
            self.ctl_bouncer := self.ctl_bouncer / 2;
         end if;
      end if;
      Panel.lr_ad := self.ad_bouncer;
      Panel.lr_ctl := self.ctl_bouncer;
   end;
   --
   procedure scan16(self : in out simple) is
   begin
      if (self.ad_scanner and 16#FFFF#) = 0 then
         self.ad_scanner := 1;
      else
         self.ad_scanner := self.ad_scanner * 2;
      end if;
      if self.ctl_scanner = 0 then
         self.ctl_scanner := 2;
      else
         self.ctl_scanner := self.ctl_scanner * 2;
      end if;
      Panel.lr_ad := self.ad_scanner;
      Panel.lr_ctl := self.ctl_scanner;
   end;
   --
   procedure scan32(self : in out simple) is
   begin
      if self.ad_scanner = 0 then
         self.ad_scanner := 1;
      else
         self.ad_scanner := self.ad_scanner * 2;
      end if;
      if self.ctl_scanner = 0 then
         self.ctl_scanner := 2;
      else
         self.ctl_scanner := self.ctl_scanner * 2;
      end if;
      Panel.lr_ad := self.ad_scanner;
      Panel.lr_ctl := self.ctl_scanner;
   end;
   --
   procedure fibonacci(self : in out simple) is
      ad_temp : constant BBS.embed.uint32 := self.ad_fib_1 + self.ad_fib_2;
      ctl_temp : constant BBS.embed.uint16 := self.ctl_fib_1 + self.ctl_fib_2;
   begin
      Panel.lr_ad := ad_temp;
      self.ad_fib_2 := self.ad_fib_1;
      self.ad_fib_1 := ad_temp;
      Panel.lr_ctl := ctl_temp;
      self.ctl_fib_2 := self.ctl_fib_1;
      self.ctl_fib_1 := ctl_temp;
      if (self.ad_fib_1 = 0) and (self.ad_fib_2 = 0) then
         self.ad_fib_1 := 1;
         self.ad_fib_2 := 1;
      end if;
      if (self.ctl_fib_1 = 0) and (self.ctl_fib_2 = 0) then
         self.ctl_fib_1 := 1;
         self.ctl_fib_2 := 2;
      end if;
   end;
   --
   procedure copy_sw_ad is
   begin
      Panel.lr_ad := Panel.sr_ad;
   end;
   --
   procedure copy_sw_ctl is
   begin
      Panel.lr_ctl := Panel.sr_ctl;
   end;
   --
   procedure copy_sw is
   begin
      copy_sw_ad;
      copy_sw_ctl;
   end;

end;
