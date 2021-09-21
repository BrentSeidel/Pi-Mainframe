with bbs.web_common;
package web is
   --
   --  Start the web server
   --
   procedure start_server;
private
   internal_map : bbs.web_common.proc_tables.Map;
   --
   --  Build the map for internal procedure calls.  The key strings must match
   --  the identifications in the configuration file.  The generated map is
   --  used by both the GET and POST methods.
   --
   procedure build_internal_map;

end;
