--
--  This package contains the XML responders for the server side of AJAX.
--
with GNAT.Sockets;
with BBS.web_common;
package web.xml is
   --
   --  Get and optionally set the state of the auto_man flag
   --
   procedure auto_man(s : GNAT.Sockets.Stream_Access;
                      h : BBS.web_common.params.Map;
                      p : BBS.web_common.params.Map);
   --
   --  Get and optionally set the type of the simulation
   --
   procedure sim_type(s : GNAT.Sockets.Stream_Access;
                      h : BBS.web_common.params.Map;
                      p : BBS.web_common.params.Map);
   --
   --  Get switch and LED register values.  Currently read-only
   --
   procedure sw_led_reg(s : GNAT.Sockets.Stream_Access;
                        h : BBS.web_common.params.Map;
                        p : BBS.web_common.params.Map);
   --
   --  Get simulated CPU information (currently name, num reg, and mem size)
   --
   procedure Get_CPU_info(s : GNAT.Sockets.Stream_Access;
                          h : BBS.web_common.params.Map;
                          p : BBS.web_common.params.Map);
   --
   --  Get simulated CPU register name and value
   --
   procedure Get_CPU_reg(s : GNAT.Sockets.Stream_Access;
                         h : BBS.web_common.params.Map;
                         p : BBS.web_common.params.Map);
   --
   --  Set exit flags
   --
   procedure set_exits(s : GNAT.Sockets.Stream_Access;
                       h : BBS.web_common.params.Map;
                       p : BBS.web_common.params.Map);
end;
