with BBS.embed;
with BBS.http;
with BBS.internal;
with sim;
package body web.xml is
   --
   --  Get and optionally set the state of the auto_man flag
   --
   procedure auto_man(s : GNAT.Sockets.Stream_Access;
                      h : bbs.web_common.params.Map;
                      p : bbs.web_common.params.Map) is
      pragma Unreferenced(h);
      value : Boolean := Sim.auto_man;
   begin
      bbs.http.ok(s, "application/xml");
      if (bbs.web_common.params.Contains(p, "auto-man")) then
         begin
            value := Boolean'Value(bbs.web_common.params.Element(p, "auto-man"));
         exception
            when others =>
               value := Sim.auto_man;
         end;
         if sim.sw_ctrl.auto then
            Sim.auto_man := value;
         end if;
      end if;
      String'Write(s, "<xml><auto-enable>" & Boolean'Image(sim.sw_ctrl.auto) &
                   "</auto-enable><auto-man>" & Boolean'Image(sim.auto_man) & "</auto-man></xml>");
   end auto_man;
   --
   --  Get and optionally set the type of the simulation
   --
   procedure sim_type(s : GNAT.Sockets.Stream_Access;
                      h : bbs.web_common.params.Map;
                      p : bbs.web_common.params.Map) is
      pragma Unreferenced(h);
      value : Natural := Sim.get_pattern;
   begin
      bbs.http.ok(s, "application/xml");
      if (bbs.web_common.params.Contains(p, "sim-type")) then
         begin
            value := Natural'Value(bbs.web_common.params.Element(p, "sim-type"));
         exception
            when others =>
               value := Sim.get_pattern;
         end;
         Sim.set_pattern(value);
      end if;
      String'Write(s, "<xml><pattern>" & Natural'Image(sim.get_pattern) & "</pattern></xml>");
   end sim_type;
   --
   --  Get switch and LED register values.  Currently read-only
   --
   procedure sw_led_reg(s : GNAT.Sockets.Stream_Access;
                        h : bbs.web_common.params.Map;
                        p : bbs.web_common.params.Map) is
      pragma Unreferenced(h);
      pragma Unreferenced(p);
   begin
      bbs.http.ok(s, "application/xml");
      String'Write(s, "<xml>");
      String'Write(s, "<lr-ad>" & BBS.embed.uint32'Image(sim.lr_ad) & "</lr-ad>");
      String'Write(s, "<lr-ctl>" & BBS.embed.uint16'Image(sim.lr_ctl) & "</lr-ctl>");
      String'Write(s, "<sr-ad>" & BBS.embed.uint32'Image(sim.sr_ad) & "</sr-ad>");
      String'Write(s, "<sr-ctl>" & BBS.embed.uint16'Image(sim.sr_ctl) & "</sr-ctl>");
      String'Write(s, "</xml>");
   end sw_led_reg;
   --
   --  Set exit flags.  Sets the simulator exit flag and then request that the
   --  web server exit.
   --
   procedure set_exits(s : GNAT.Sockets.Stream_Access;
                       h : bbs.web_common.params.Map;
                       p : bbs.web_common.params.Map)is
   begin
      sim.exit_sim := True;
      BBS.internal.html_set_exit(s, h, p);
   end set_exits;
   --
end;
