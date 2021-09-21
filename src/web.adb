with Ada.Text_IO;
with Ada.Exceptions;
--with bbs.svg;
with bbs.internal;
with bbs.http;
with bbs.web_server;
--with bbs.web_common;
with web.internal;
with web.xml;
package body web is
   --
   --  Build the map for internal procedure calls.  The key strings must match
   --  the identifications in the configuration file.  The generated map is
   --  used by both the GET and POST methods.
   --
   procedure build_internal_map is
   begin
      null;
      --
      --  ******************************************************
      --  Customization goes here to add any internally routines
      --  to generate responses.
      --
      internal_map.Insert("show_config", web.internal.show_config'Access);
      internal_map.Insert("exit", BBS.internal.html_set_exit'Access);
      --
      internal_map.Insert("auto_man", web.xml.auto_man'Access);
      internal_map.Insert("sim_type", web.xml.sim_type'Access);
   end;
   --
   procedure start_server is
   begin
      --
      -- Set debugging flags to appropriate values.
      --
      bbs.web_server.debug.set;
      bbs.http.debug_req.set;
      bbs.http.debug_head.clear;
      --
      -- Build the map for internal routines.
      --
      build_internal_map;
      --
      -- Start the web server.  This does not return normally.
      --
      bbs.web_server.server(internal_map, "web/config.txt", 31415);
      --
   exception
      when err: Others =>
         Ada.Text_IO.Put_Line("Unhandled exception occured during operation.");
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(err));
   end start_server;
end;
