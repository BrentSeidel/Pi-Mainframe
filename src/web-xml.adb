with BBS.embed;
with BBS.http;
with BBS.internal;
with Panel;
package body web.xml is
   --
   --  Get and optionally set the state of the auto_man flag
   --
   procedure auto_man(s : GNAT.Sockets.Stream_Access;
                      h : BBS.web_common.params.Map;
                      p : BBS.web_common.params.Map) is
      pragma Unreferenced(h);
      value : Boolean := Panel.auto_man;
   begin
      BBS.http.ok(s, "application/xml");
      if (BBS.web_common.params.Contains(p, "auto-man")) then
         begin
            value := Boolean'Value(BBS.web_common.params.Element(p, "auto-man"));
         exception
            when others =>
               value := Panel.auto_man;
         end;
         if Panel.sw_ctrl.auto then
            Panel.auto_man := value;
         end if;
      end if;
      String'Write(s, "<xml><auto-enable>" & Boolean'Image(Panel.sw_ctrl.auto) &
                   "</auto-enable><auto-man>" & Boolean'Image(Panel.auto_man) & "</auto-man></xml>");
   end auto_man;
   --
   --  Get and optionally set the type of the simulation
   --
   procedure sim_type(s : GNAT.Sockets.Stream_Access;
                      h : BBS.web_common.params.Map;
                      p : BBS.web_common.params.Map) is
      pragma Unreferenced(h);
      value : Natural := Panel.get_pattern;
   begin
      BBS.http.ok(s, "application/xml");
      if (BBS.web_common.params.Contains(p, "sim-type")) then
         begin
            value := Natural'Value(BBS.web_common.params.Element(p, "sim-type"));
         exception
            when others =>
               value := Panel.get_pattern;
         end;
         Panel.set_pattern(value);
      end if;
      String'Write(s, "<xml><pattern>" & Natural'Image(Panel.get_pattern) & "</pattern></xml>");
   end sim_type;
   --
   --  Get switch and LED register values.  Currently read-only
   --
   procedure sw_led_reg(s : GNAT.Sockets.Stream_Access;
                        h : BBS.web_common.params.Map;
                        p : BBS.web_common.params.Map) is
      pragma Unreferenced(h);
      pragma Unreferenced(p);
   begin
      BBS.http.ok(s, "application/xml");
      String'Write(s, "<xml>");
      String'Write(s, "<lr-ad>" & BBS.embed.uint32'Image(Panel.lr_ad) & "</lr-ad>");
      String'Write(s, "<lr-ctl>" & BBS.embed.uint16'Image(Panel.lr_ctl) & "</lr-ctl>");
      String'Write(s, "<sr-ad>" & BBS.embed.uint32'Image(Panel.sr_ad) & "</sr-ad>");
      String'Write(s, "<sr-ctl>" & BBS.embed.uint16'Image(Panel.sr_ctl) & "</sr-ctl>");
      String'Write(s, "</xml>");
   end sw_led_reg;
   --
   --  Get simulated CPU information (currently name, num reg, and mem size)
   --
   procedure Get_CPU_info(s : GNAT.Sockets.Stream_Access;
                          h : BBS.web_common.params.Map;
                          p : BBS.web_common.params.Map) is
      pragma Unreferenced(h);
      pragma Unreferenced(p);
   begin
      BBS.http.ok(s, "application/xml");
      String'Write(s, "<xml>");
      String'Write(s, "<cpu-name>" & Panel.simulate.all.name & "</cpu-name>");
      String'Write(s, "<cpu-reg>" & BBS.embed.uint32'Image(Panel.simulate.all.registers) & "</cpu-reg>");
      String'Write(s, "<cpu-mem>" & BBS.embed.uint32'Image(Panel.simulate.all.mem_size) & "</cpu-mem>");
      String'Write(s, "</xml>");
   end Get_CPU_info;
   --
   --  Get simulated CPU register name and value
   --
   procedure Get_CPU_reg(s : GNAT.Sockets.Stream_Access;
                         h : BBS.web_common.params.Map;
                         p : BBS.web_common.params.Map) is
      pragma Unreferenced(h);
      value : Natural;
   begin
      BBS.http.ok(s, "application/xml");
      if (BBS.web_common.params.Contains(p, "register")) then
         begin
            value := Natural'Value(BBS.web_common.params.Element(p, "register"));
         exception
            when others =>
               value := 0;
         end;
      end if;
      String'Write(s, "<xml><reg-num>" & Natural'Image(value) & "</reg-num>");
      String'Write(s, "<reg-name>" & Panel.simulate.all.reg_name(BBS.embed.uint32(value))
                   & "</reg-name>");
      String'Write(s, "<reg-value>" & BBS.embed.uint32'Image(Panel.simulate.all.read_reg(BBS.embed.uint32(value)))
                     & "</reg-value></xml>");
   end Get_CPU_reg;
   --
   --  Set exit flags.  Sets the simulator exit flag and then request that the
   --  web server exit.
   --
   procedure set_exits(s : GNAT.Sockets.Stream_Access;
                       h : BBS.web_common.params.Map;
                       p : BBS.web_common.params.Map)is
   begin
      Panel.exit_sim := True;
      BBS.internal.html_set_exit(s, h, p);
   end set_exits;
   --
end;
