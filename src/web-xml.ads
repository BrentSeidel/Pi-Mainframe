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
                      h : bbs.web_common.params.Map;
                      p : bbs.web_common.params.Map);
   --
   --  Get and optionally set the type of the simulation
   --
   procedure sim_type(s : GNAT.Sockets.Stream_Access;
                      h : bbs.web_common.params.Map;
                      p : bbs.web_common.params.Map);
end;
