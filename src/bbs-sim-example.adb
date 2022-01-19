with Panel;
with BBS.embed;
use type BBS.embed.uint16;
use type BBS.embed.uint32;
package body BBS.Sim.example is
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out simple) is
   begin
      self.reg(pattern) := 0;
      self.reg(ad_counter) := 0;
      self.reg(ctl_counter) := 0;
      self.reg(ad_bouncer) := 0;
      self.ad_bounce_dir := left;
      self.reg(ctl_bouncer) := 0;
      self.ctl_bounce_dir := left;
      self.reg(ad_scanner) := 0;
      self.reg(ctl_scanner) := 0;
      self.reg(ad_fib1) := 1;
      self.reg(ad_fib2) := 1;
      self.reg(ctl_fib1) := 1;
      self.reg(ctl_fib2) := 2;
   end;
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out simple) is
      d : Duration := 0.05;
   begin
      Panel.lr_addr := self.reg(pattern);
      case self.reg(pattern) is
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
      if Panel.sw_ctrl.addr then
         self.reg(addr) := Panel.sr_ad;
      else
         self.reg(pattern) := Panel.sr_ad;
         Panel.lr_data := self.reg(pattern);
         self.reg(addr) := self.reg(addr) + 1;
      end if;
      Panel.lr_addr := self.reg(addr);
   end;
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   overriding
   procedure examine(self : in out simple) is
   begin
      Panel.lr_addr := self.reg(addr);
      Panel.lr_data := self.reg(pattern);
      if not Panel.sw_ctrl.addr then
         self.reg(addr) := self.reg(addr) + 1;
      end if;
   end;
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get number of registers
   --
   overriding
   function registers(self : in out simple) return BBS.embed.uint32 is
      pragma Unreferenced(self);
   begin
      return reg_id'Pos(reg_id'Last) + 1;
   end;
   --
   --  ----------------------------------------------------------------------
   --  Simulator data
   --
   --  Called to set a memory value
   --
   overriding
   procedure set_mem(self : in out simple; mem_addr : BBS.embed.uint32;
                     data : BBS.embed.uint32) is
      pragma Unreferenced(mem_addr);
   begin
      self.reg(pattern) := data;
   end;
   --
   --  Called to read a memory value
   --
   overriding
   function read_mem(self : in out simple; mem_addr : BBS.embed.uint32) return
     BBS.embed.uint32 is
      pragma Unreferenced(mem_addr);
   begin
      return self.reg(pattern);
   end;
   --
   --  Called to get register name
   --
   overriding
   function reg_name(self : in out simple; num : BBS.embed.uint32)
                     return String is
      pragma Unreferenced(self);
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         return reg_id'Image(reg_id'Val(num));
      else
         return "*invalid*";
      end if;
   end;
   --
   --  Called to get register value
   --
   overriding
   function read_reg(self : in out simple; num : BBS.embed.uint32)
                     return BBS.embed.uint32 is
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         return self.reg(reg_id'Val(num));
      else
         return 0;
      end if;
   end;
   --
   --  Called to set register value
   --
--   overriding
--   procedure set_reg(self : in out simple; num : BBS.embed.uint32;
--                     data : BBS.embed.uint32) is null;
   --  --------------------------------------------------------------------
   --
   --  Code for the various patterns.
   --
   procedure count(self : in out simple) is
   begin
      self.reg(ad_counter) := self.reg(ad_counter) + 1;
      self.reg(ctl_counter) := self.reg(ctl_counter) + 2;
      Panel.lr_data := self.reg(ad_counter);
      Panel.lr_ctl := BBS.embed.uint16(self.reg(ctl_counter) and 16#FFFF#);
   end;
   --
   procedure bounce16(self : in out simple) is
   begin
      if self.ad_bounce_dir = left then
         if (self.reg(ad_bouncer) and 16#FFFF#) = 0 then
            self.ad_bounce_dir := right;
            self.reg(ad_bouncer) := 16#8000#;
         else
            self.reg(ad_bouncer) := self.reg(ad_bouncer) * 2;
         end if;
      else
         if (self.reg(ad_bouncer) and 16#FFFF#) = 0 then
            self.ad_bounce_dir := left;
            self.reg(ad_bouncer) := 16#0001#;
         else
            self.reg(ad_bouncer) := self.reg(ad_bouncer) / 2;
         end if;
      end if;
      if self.ctl_bounce_dir = left then
         if self.reg(ctl_bouncer) = 0 then
            self.ctl_bounce_dir := right;
            self.reg(ctl_bouncer) := 16#8000#;
         else
            self.reg(ctl_bouncer) := self.reg(ctl_bouncer) * 2;
         end if;
      else
         if self.reg(ctl_bouncer) = 1 then
            self.ctl_bounce_dir := left;
            self.reg(ctl_bouncer) := 16#0002#;
         else
            self.reg(ctl_bouncer) := self.reg(ctl_bouncer) / 2;
         end if;
      end if;
      Panel.lr_data := self.reg(ad_bouncer);
      Panel.lr_ctl := BBS.embed.uint16(self.reg(ctl_bouncer) and 16#FFFF#);
   end;
   --
   procedure bounce32(self : in out simple) is
   begin
      if self.ad_bounce_dir = left then
         if self.reg(ad_bouncer) = 0 then
            self.ad_bounce_dir := right;
            self.reg(ad_bouncer) := 16#8000_0000#;
         else
            self.reg(ad_bouncer) := self.reg(ad_bouncer) * 2;
         end if;
      else
         if self.reg(ad_bouncer) = 0 then
            self.ad_bounce_dir := left;
            self.reg(ad_bouncer) := 16#0000_0001#;
         else
            self.reg(ad_bouncer) := self.reg(ad_bouncer) / 2;
         end if;
      end if;
      if self.ctl_bounce_dir = left then
         if self.reg(ctl_bouncer) = 0 then
            self.ctl_bounce_dir := right;
            self.reg(ctl_bouncer) := 16#8000#;
         else
            self.reg(ctl_bouncer) := self.reg(ctl_bouncer) * 2;
         end if;
      else
         if self.reg(ctl_bouncer) = 1 then
            self.ctl_bounce_dir := left;
            self.reg(ctl_bouncer) := 16#0002#;
         else
            self.reg(ctl_bouncer) := self.reg(ctl_bouncer) / 2;
         end if;
      end if;
      Panel.lr_data := self.reg(ad_bouncer);
      Panel.lr_ctl := BBS.embed.uint16(self.reg(ctl_bouncer) and 16#FFFF#);
   end;
   --
   procedure scan16(self : in out simple) is
   begin
      if (self.reg(ad_scanner) and 16#FFFF#) = 0 then
         self.reg(ad_scanner) := 1;
      else
         self.reg(ad_scanner) := self.reg(ad_scanner) * 2;
      end if;
      if self.reg(ctl_scanner) = 0 then
         self.reg(ctl_scanner) := 2;
      else
         self.reg(ctl_scanner) := self.reg(ctl_scanner) * 2;
      end if;
      Panel.lr_data := self.reg(ad_scanner);
      Panel.lr_ctl := BBS.embed.uint16(self.reg(ctl_scanner) and 16#FFFF#);
   end;
   --
   procedure scan32(self : in out simple) is
   begin
      if self.reg(ad_scanner) = 0 then
         self.reg(ad_scanner) := 1;
      else
         self.reg(ad_scanner) := self.reg(ad_scanner) * 2;
      end if;
      if self.reg(ctl_scanner) = 0 then
         self.reg(ctl_scanner) := 2;
      else
         self.reg(ctl_scanner) := self.reg(ctl_scanner) * 2;
      end if;
      Panel.lr_data := self.reg(ad_scanner);
      Panel.lr_ctl := BBS.embed.uint16(self.reg(ctl_scanner) and 16#FFFF#);
   end;
   --
   procedure fibonacci(self : in out simple) is
      ad_temp : constant BBS.embed.uint32 := self.reg(ad_fib1) + self.reg(ad_fib2);
      ctl_temp : constant BBS.embed.uint16 := BBS.embed.uint16((self.reg(ctl_fib1) + self.reg(ctl_fib2)) and 16#FFFF#);
   begin
      Panel.lr_data := ad_temp;
      self.reg(ad_fib2) := self.reg(ad_fib1);
      self.reg(ad_fib1) := ad_temp;
      Panel.lr_ctl := ctl_temp;
      self.reg(ctl_fib2) := self.reg(ctl_fib1);
      self.reg(ctl_fib1) := BBS.embed.uint32(ctl_temp);
      if (self.reg(ad_fib1) = 0) and (self.reg(ad_fib2) = 0) then
         self.reg(ad_fib1) := 1;
         self.reg(ad_fib2) := 1;
      end if;
      if (self.reg(ctl_fib1) = 0) and (self.reg(ctl_fib2) = 0) then
         self.reg(ctl_fib1) := 1;
         self.reg(ctl_fib2) := 2;
      end if;
   end;
   --
   procedure copy_sw_ad is
   begin
      Panel.lr_data := Panel.sr_ad;
      Panel.lr_addr := Panel.sr_ad;
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

end BBS.Sim.example;
