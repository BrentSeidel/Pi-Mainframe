with BBS.embed;
package Sim is
   --
   --  This package describes an interaface that the panel can used to control
   --  a simulator.  Specifying this interface should allow easier implementation
   --  of multiple simulator.
   --
   type simulator is abstract tagged private;
   type sim_access is access all simulator'Class;
   --
   --  The actual interface.  These are routines that are called under specific
   --  circumstances.  They can examine the switch register to further decide
   --  their actions and set the LED registers as desired.
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called first to initialize the simulator
   --
   procedure init(self : in out simulator) is abstract;
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   procedure start(self : in out simulator) is abstract;
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   procedure run(self : in out simulator) is abstract;
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   procedure deposit(self : in out simulator) is abstract;
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   procedure examine(self : in out simulator) is abstract;
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get simulator name
   --
   function name(self : in out simulator) return String is ("No simulator");
   --
   --  Called to get simulator memory size
   --
   function mem_size(self : in out simulator) return BBS.embed.uint32 is (0);
   --
   --  Called to get number of registers
   --
   function registers(self : in out simulator) return BBS.embed.uint32 is (0);
   --
   --  ----------------------------------------------------------------------
   --  Simulator data
   --
   --  Called to set a memory value
   --
   procedure set_mem(self : in out simulator; mem_addr : BBS.embed.uint32;
                     data : BBS.embed.uint32) is abstract;
   --
   --  Called to read a memory value
   --
   function read_mem(self : in out simulator; mem_addr : BBS.embed.uint32) return
     BBS.embed.uint32 is abstract;
   --
   --  Called to get register name
   --
   function reg_name(self : in out simulator; num : BBS.embed.uint32)
                     return String is abstract;
   --
   --  Called to get register value
   --
   function read_reg(self : in out simulator; num : BBS.embed.uint32)
                     return BBS.embed.uint32 is abstract;
   --
   --  Called to set register value
   --
   procedure set_reg(self : in out simulator; num : BBS.embed.uint32;
                     data : BBS.embed.uint32) is abstract;

private
   type simulator is abstract tagged record
      null;
   end record;
end Sim;
