with GNAT.Sockets;
with bbs.web_common;
--
--  Internally generated pages for the mainframe simulator
--
package web.internal is
   --
   --  Display the hardware and software configuration
   --
   procedure show_config(s : GNAT.Sockets.Stream_Access;
                         h : bbs.web_common.params.Map;
                         p : bbs.web_common.params.Map);

private
   CRLF : String renames bbs.web_common.CRLF;

end;
