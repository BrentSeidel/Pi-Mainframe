with Ada.Text_IO;
with Ada.Exceptions;
with BBS.http;
with BBS.web_server;
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
      internal_map.Insert("exit", web.xml.set_exits'Access);
      --
      internal_map.Insert("auto_man", web.xml.auto_man'Access);
      internal_map.Insert("sim_type", web.xml.sim_type'Access);
      internal_map.Insert("panel_reg", web.xml.sw_led_reg'Access);
      internal_map.Insert("cpu_info", web.xml.Get_CPU_info'Access);
      internal_map.Insert("cpu_reg", web.xml.Get_CPU_reg'Access);
   end;
   --
   procedure start_server is
   begin
      --
      -- Set debugging flags to appropriate values.
      --
      BBS.web_server.debug.set;
      BBS.http.debug_req.set;
      BBS.http.debug_head.clear;
      --
      -- Build the map for internal routines.
      --
      build_internal_map;
      --
      -- Start the web server.  This does not return normally.
      --
      BBS.web_server.server(internal_map, "web/config.txt", 31415);
      --
   exception
      when err: Others =>
         Ada.Text_IO.Put_Line("Unhandled exception occured during operation.");
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(err));
   end start_server;
end;
