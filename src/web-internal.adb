with bbs.http;
with bbs.html;
with i2c;
--with sim;
--with Ada.Text_IO;
package body web.internal is
   --
   --  Display information sent in a form
   --
   procedure show_config(s : GNAT.Sockets.Stream_Access;
                         h : bbs.web_common.params.Map;
                         p : bbs.web_common.params.Map) is
      pragma Unreferenced(h);
      pragma Unreferenced(p);
   begin
      --
      --  Page headers and introduction
      --
      bbs.http.ok(s, "text/html");
      bbs.html.html_head(s, "System Configuration", "Style");
      String'Write(s, "<h1>Detected hardware</h1>" & CRLF);
      --
      --  Write a table for the MCP23017 devices
      --
      String'Write(s, "<h2>MCP23017</h2>" & CRLF);
      String'Write(s, "<p>Each MCP23017 provides 16 discrete I/O lines</p>" & CRLF);
      String'Write(s, "<table>" & CRLF);
      String'Write(s, "<tr><th>Unit</th><th>Present</th></tr></tr>" & CRLF);
      for i in i2c.MCP23017_use loop
         String'Write(s, "<tr><td>" & i2c.MCP23017_use'Image(i) & "</td><td>" &
                        Boolean'Image(i2c.MCP23017_found(i)) & "</td></tr>" & CRLF);
      end loop;
      String'Write(s, "</table>" & CRLF);
      --
      --  End the page
      --
      bbs.html.html_end(s, "web/footer.html");
   end show_config;
end;
