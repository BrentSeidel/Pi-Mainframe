with BBS.embed;
with BBS.embed.i2c;
with i2c;
package Sim.example is
   type simple is new simulator with private;
   --
   --  Called first to initialize the simulator
   --
   overriding
   procedure init(self : in out simple) is null;
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out simple);
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out simple);
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   overriding
   procedure deposit(self : in out simple);
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   overriding
   procedure examine(self : in out simple);
   --
   --  Called to set a memory value
   --
   overriding
   procedure set_mem(self : in out simple; addr : BBS.embed.uint32;
                     data : BBS.embed.uint32);
   --
   --  Called to read a memory value
   --
   overriding
   function read_mem(self : in out simple; addr : BBS.embed.uint32) return
     BBS.embed.uint32;

private
   --
   --  Which pattern to select:
   --     0 - Copy switches
   --     1 - count
   --     2 - scan 16-bit
   --     3 - bounce 16-bit
   --     4 - fibbonacci
   --     9 - count
   --    10 - scan 32-bit
   --    11 - bounce 32-bit
   --    12 - fibbonacci
   --    others - Copy switches
   --
   type bounce_type is (left, right);

   type simple is new simulator with record
      pattern : BBS.embed.uint32 := 0;
      --
      --  Data for the various patterns.
      --
      ad_counter : BBS.embed.uint32 := 0;
      ctl_counter : BBS.embed.uint16 := 0;
      --
      ad_bouncer : BBS.embed.uint32 := 0;
      ad_bounce_dir : bounce_type := left;
      ctl_bouncer : BBS.embed.uint16 := 0;
      ctl_bounce_dir : bounce_type := left;
      --
      ad_scanner : BBS.embed.uint32 := 0;
      ctl_scanner : BBS.embed.uint16 := 0;
      --
      ad_fib_1 : BBS.embed.uint32 := 1;
      ad_fib_2 : BBS.embed.uint32 := 1;
      ctl_fib_1 : BBS.embed.uint16 := 1;
      ctl_fib_2 : BBS.embed.uint16 := 2;
   end record;

   err  : BBS.embed.i2c.err_code;
   res  : i2c.result;
   --
   --  Code for the various patterns.
   --
   procedure count(self : in out simple);
   procedure bounce16(self : in out simple);
   procedure bounce32(self : in out simple);
   procedure scan16(self : in out simple);
   procedure scan32(self : in out simple);
   procedure fibonacci(self : in out simple);
   procedure copy_sw;
   procedure copy_sw_ad;
   procedure copy_sw_ctl;
end;
