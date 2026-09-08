with Ada.Text_IO;
with BBS.Sim_CPU.bus.mem8;
with BBS.Sim_CPU.bus.pdp11;
with BBS.Sim_CPU.cpu.example;
with BBS.Sim_CPU.cpu.i8080;
with BBS.Sim_CPU.cpu.m68000;
with BBS.Sim_CPU.cpu.pdp11;
with BBS.Sim_CPU.io.serial.DL11;
with BBS.Sim_CPU.io.disk.RK11;
with BBS.Sim_CPU.io.disk.RK611;
--with BBS.Sim_CPU.io.disk.TM11;
--with BBS.Sim_CPU.io.tape.PC11;
with BBS.Sim_CPU.io.Clock.KW11;
with BBS.Sim_CPU.io.KT11;
package body Panel is
   --
   --  Run the panel interface and simulator control.
   --
   task body run is
      package Hex_IO is new Ada.Text_IO.Integer_IO(Integer);
   begin
      --
      --  Wait for I2C initialization, MCP23017 detection and other
      --  initializations in the main thread before starting.
      --
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
      i2c.set_addr_data(lr_ad, res);
      i2c.set_ctrl(lr_ctl, res);
      --
      --  Processing loop
      --
      loop
         i2c.read_addr_data(pvt_sr_ad, res);
         i2c.read_ctrl(pvt_sr_ctl, res);
         bus.all.set_sr_ad(pvt_sr_ad);
         bus.all.set_sr_ctrl(pvt_sw_ctrl);
         process_switch;
         if pvt_starting then
            CPU.all.start;
         end if;
         if sw_ctrl.run and sw_ctrl.start then
            CPU.all.run;
         else
--            process_mode_ctrl(BBS.Sim_CPU.PROC_KERN, BBS.Sim_CPU.ADDR_INST, sr_ctl);
            if pvt_deposit then
               CPU.all.deposit;
            elsif pvt_examine then
               CPU.all.examine;
            end if;
         end if;
         lr_ctrl := bus.all.get_lr_ctrl;
         lr_ctrl.ready := True;
         if CPU.all.halted then
            lr_ctrl.run   := False;
            lr_ctrl.start := False;
         else
            lr_ctrl.run   := sw_ctrl.run;
            lr_ctrl.start := sw_ctrl.start;
         end if;
         lr_ctrl.exam := sw_ctrl.exam;
         lr_ctrl.dep  := sw_ctrl.dep;
         lr_ctrl.addr := sw_ctrl.addr;
         if sw_ctrl.addr then
            lr_ad := bus.all.get_lr_addr;
         else
            lr_ad := bus.all.get_lr_data;
         end if;
         i2c.set_addr_data(lr_ad, res);
         i2c.set_ctrl(lr_ctl, res);
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
   --  --------------------------------------------------------------------
   --
   --  Initializations for different simulations
   --
   procedure init_sim_example is
   begin
     cpu := new BBS.Sim_CPU.CPU.example.simple;
     cpu.all.start;
   end;
   --
   procedure init_sim_8080 is
   begin
     cpu := new BBS.Sim_CPU.CPU.i8080.i8080;
     bus := new BBS.Sim_CPU.bus.mem8.mem8io(2**16);
     cpu.all.init;
     cpu.all.attach_bus(bus, 1);
     cpu.all.variant(2);
     bus.all.attach_io(tel'Access, 0, BBS.Sim_CPU.BUS_IO);
     tel.setOwner(cpu);
     tel.init(tel'Access, 2171);
     bus.all.attach_io(fd'Access, 4, BBS.Sim_CPU.BUS_IO);
     fd.setOwner(cpu);
     fd.open(0, BBS.Sim_CPU.io.disk.hd_geom, "images/test.img");
     fd.open(1, BBS.Sim_CPU.io.disk.floppy8_geom, "images/user.img");
     fd.open(2, BBS.Sim_CPU.io.disk.floppy8_geom, "images/fortran.img");
     fd.open(3, BBS.Sim_CPU.io.disk.floppy8_geom, "images/mbasic.img");
     cpu.all.load("boot.ihx");
     cpu.all.start(0);
   end;
   --
   procedure init_sim_68000 is
   begin
      cpu := new BBS.Sim_CPU.CPU.m68000.m68000;
      bus := new BBS.Sim_CPU.bus.mem8.mem8mem(2**20);
      cpu.all.attach_bus(bus, 1);
      cpu.all.variant(1);
      cpu.all.init;
      bus.all.attach_io(clock'Access, 16#400#, BBS.Sim_CPU.BUS_MEMORY);
      clock.setOwner(CPU);
      BBS.Sim_CPU.io.Clock.setBaseRate(1.0);
      clock.init(clock'Access);
      clock.setException(256+64);
      bus.all.attach_io(tel0'Access, 16#402#, BBS.Sim_CPU.BUS_MEMORY);
      tel0.setOwner(CPU);
      tel0.init(tel0'Access, 2171);
      tel0.setException(2*256+65);
      bus.all.attach_io(tel1'Access, 16#404#, BBS.Sim_CPU.BUS_MEMORY);
      tel1.setOwner(CPU);
      tel1.init(tel1'Access, 2172);
      tel1.setException(2*256+66);
      bus.all.attach_io(tel2'Access, 16#406#, BBS.Sim_CPU.BUS_MEMORY);
      tel2.setOwner(CPU);
      tel2.init(tel2'Access, 2173);
      tel2.setException(2*256+67);
      bus.all.attach_io(mux'Access, 16#408#, BBS.Sim_CPU.BUS_MEMORY);
      mux.setOwner(CPU);
      mux.init(mux'Access, 3141);
      mux.setException(2*256+68);
      cpu.all.load("Tasks.S");
      cpu.all.load("OS68k.S");
      cpu.all.start(16#2000#);
   end;
   --
   procedure init_sim_pdp11 is
      dl11  : BBS.Sim_CPU.io.serial.DL11.dl11_access;
      kw11  : BBS.Sim_CPU.io.clock.KW11.kw11_access;
      kt11  : BBS.Sim_CPU.io.KT11.kt11_access;
      disk  : BBS.Sim_CPU.io.disk.disk_access;
      base  : constant BBS.Sim_CPU.addr_bus := 16#1000#;
      temp  : BBS.Sim_CPU.bus_stat;
   begin
      cpu := new BBS.Sim_CPU.CPU.pdp11.pdp11;
      bus := new BBS.Sim_CPU.bus.pdp11.unibus(2**18);
      cpu.all.attach_bus(bus, 1);
      cpu.all.variant(3);
      --  KW11 always attaches to memory address 777546 and vector 100, BR6.
      kw11 := new BBS.Sim_CPU.io.clock.kw11.kw11;
      bus.all.attach_io(BBS.Sim_CPU.io.io_access(kw11), 8#777546#, BBS.Sim_CPU.BUS_MEMORY);
      kw11.all.setOwner(cpu);
      kw11.all.setException(16#06_0000# + 8#100#);
      kw11.all.init(kw11, BBS.Sim_CPU.io.clock.kw11.Hz1);
      --  KT11 has its own attachment mechanism through the bus.
      kt11 := new BBS.Sim_CPU.io.kt11.kt11;
      kt11.all.setOwner(cpu);
--      kt11.all.setExeption(8#250#);
      bus.all.attach_io(BBS.Sim_CPU.io.io_access(kt11), 0, BBS.Sim_CPU.BUS_MEMORY);
      --  DL11 Rx vector is #o060, Tx vector is #o064, both at BR4.  Combined value is #o15000060
      dl11 := new BBS.Sim_CPU.io.serial.DL11.DL11x;
      bus.all.attach_io(BBS.Sim_CPU.io.io_access(dl11), 8#777560#, BBS.Sim_CPU.BUS_MEMORY);
      dl11.all.setOwner(cpu);
      dl11.all.init(dl11, 2171);
      dl11.all.setException(8#15000060#);
      --  RK11 vector is 220 at BR5
      disk := new BBS.Sim_CPU.io.disk.RK11.RK11;
      bus.all.attach_io(BBS.Sim_CPU.io.io_access(disk), 8#777400#, BBS.Sim_CPU.BUS_MEMORY);
      disk.all.setOwner(cpu);
      disk.all.setException(16#05_0000# + 8#220#);
      disk.all.open(0, BBS.Sim_CPU.io.disk.null_geom, "images/rk_boot.dsk");
      disk.all.open(1, BBS.Sim_CPU.io.disk.null_geom, "images/rk_scratch.dsk");
      --  RK611 Vector is 210 at BR5
      disk := new BBS.Sim_CPU.io.disk.RK611.RK611;
      bus.all.attach_io(BBS.Sim_CPU.io.io_access(disk), 8#777440#, BBS.Sim_CPU.BUS_MEMORY);
      disk.all.setOwner(cpu);
      disk.all.setException(16#05_0000# + 8#210#);
      disk.all.open(0, BBS.Sim_CPU.io.disk.null_geom, "images/rk07_boot.dsk");
      disk.all.open(1, BBS.Sim_CPU.io.disk.null_geom, "images/rk07_working.dsk");
      --  Insert boot code here...
      bus.all.writel16l(base +  0, 8#012706#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  MOV #START,SP     ; Initialize stack pointer
      bus.all.writel16l(base +  2, 8#002000#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base +  4, 8#012700#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  MOV #0,R0         ; Move unit number to R0
      bus.all.writel16l(base +  6, 8#000000#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  Change if a different unit is desired
      bus.all.writel16l(base +  8, 8#012701#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  MOV #RKCS1,R1     ; Point to RK611 CSR
      bus.all.writel16l(base + 10, 8#177440#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base + 12, 8#012761#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  MOV #CS2_CLR,10(R1) ; Subsystem clear in CSR2
      bus.all.writel16l(base + 14, 8#000040#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base + 16, 8#000010#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base + 18, 8#010061#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  MOV R0,10(R1)     ; Set drive number in CSR2
      bus.all.writel16l(base + 20, 8#000010#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base + 22, 8#016102#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  MOV 12(R1),R2     ; Get drive type
      bus.all.writel16l(base + 24, 8#000012#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base + 26, 8#100375#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  BPL .-4           ; Valid?
      bus.all.writel16l(base + 28, 8#042702#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  BIC #177377,R2    ; Clear the rest
      bus.all.writel16l(base + 30, 8#177377#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base + 32, 8#006302#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  ASL R2
      bus.all.writel16l(base + 34, 8#006302#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  ASL R2
      bus.all.writel16l(base + 36, 8#012703#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  MOV #PACK+GO,R3
      bus.all.writel16l(base + 38, 8#000003#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base + 40, 8#050203#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  BIS R2,R3         ; Merge drive type
      bus.all.writel16l(base + 42, 8#010311#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  MOV R3,(R1)       ; Pack Ack command
      bus.all.writel16l(base + 44, 8#105711#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  TSTB (R1)         ; Check for complete
      bus.all.writel16l(base + 46, 8#100376#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  BPL .-2
      bus.all.writel16l(base + 48, 8#012761#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  MOV #-512.,2(R1)  ; Set RKWC
      bus.all.writel16l(base + 50, 8#177000#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base + 52, 8#000002#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base + 54, 8#005061#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  CLR 4(R1)         ; Clear RKBA
      bus.all.writel16l(base + 56, 8#000004#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base + 58, 8#005061#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  CLR 6(R1)         ; Clear RKDA
      bus.all.writel16l(base + 60, 8#000006#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base + 62, 8#005061#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  CLR 20(R1)        ; Clear RKDC
      bus.all.writel16l(base + 64, 8#000020#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base + 66, 8#012703#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  MOV #READ+GO,R3
      bus.all.writel16l(base + 68, 8#000021#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base + 70, 8#050203#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  BIS R2,R3         ; Merge drive type
      bus.all.writel16l(base + 72, 8#010311#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  MOV R3,(R1)       ; Read command
      bus.all.writel16l(base + 74, 8#105711#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  TSTB (R1)         ; Check for complete
      bus.all.writel16l(base + 76, 8#100376#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  BPL .-2
      bus.all.writel16l(base + 78, 8#005002#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  CLR R2
      bus.all.writel16l(base + 80, 8#005003#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  CLR R3
      bus.all.writel16l(base + 82, 8#012704#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  MOV #START+020,R4
      bus.all.writel16l(base + 84, BBS.Sim_CPU.word(base + 8#020#), BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);
      bus.all.writel16l(base + 86, 8#005005#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  CLR R5
      bus.all.writel16l(base + 88, 8#005007#, BBS.Sim_CPU.PROC_USER, BBS.Sim_CPU.ADDR_DATA, temp);  --  CLR PC
      cpu.all.start(base);
   end;
   --
   --  Process the control switches that have action based on a transition to
   --  the True state.
   --
   procedure process_switch is
   begin
      if not ctl_start then
         if sw_ctrl.start then
            pvt_starting := True;
         end if;
      end if;
      if not ctl_dep then
         if sw_ctrl.dep then
            pvt_deposit := True;
         end if;
      end if;
      if not ctl_exam then
         if sw_ctrl.exam then
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
   procedure process_mode_ctrl(m : BBS.Sim_CPU.proc_mode; a : BBS.Sim_CPU.addr_type; c : BBS.uint16) is
   begin
      lr_ctl := c;
      lr_ctrl.ready := True;
      lr_ctrl.mode  := m;
      lr_ctrl.atype := a;
   end;
   --
   --  Get and set the selected test pattern.  These will probably need to change
   --  if any simulators beyond the simple example sim get added.
   --
   procedure set_pattern(p : Natural) is
   begin
      CPU.all.set_mem(0, BBS.uint32(p));
   end;
   --
   function get_pattern return Natural is
   begin
      return Natural(CPU.all.read_mem(0));
   end;
   --
end;
