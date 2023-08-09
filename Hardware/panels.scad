//
//  These are a collection of panels for the Raspberry Pi mainframe simulator.  The panels
//  include switches and LEDs and a couple of LCD displays.  Select which panels you
//  want, then you can render them for printing.  My printer is not big enough to print
//  them all in one batch.
//
use <../../Things/bbs_connectors.scad>
use <../../Things/bbs_leds.scad>
use <../../Things/bbs_panel.scad>
use <../../Things/bbs_switches.scad>

width = 220;
//title_font = "Blackmoor LET";
title_font = "Liberation Sans::style=Bold";
//number_font = "Blackmoor LET";
number_font = "Liberation Sans::style=Bold";
//
//  This panel has mounting for 8 switches and LEDs.  Numbers are embossed between the
//  switch and LEDs.  The number values increase from right to left starting with the
//  passed "start" value.
//
module panel_switch(start)
{
  spacing = (width-30)/8;
  union()
  {
    difference()
    {
      bbs_panel(10, 4);
      union()
      {
        for(a = [1:8])
        {
          y = width/2 - (4.5-a)*spacing;
          translate([15, y, -0.1]) bbs_spdt_switch_cutout(2.2);
          if(a < 8)
          {
            translate([43, y + spacing/2 - 2.5, -0.1]) cube([2, 5, 2.2]);
            translate([35, y + spacing/2 - 2.5, -0.1]) cube([2, 5, 2.2]);
          }
          translate([40, y, -0.1]) bbs_led_oval(2.2);
          translate([28, y, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text(str(a + start - 1), halign="center", valign="center", size=6,
                font=number_font);
        }
        translate([5, width/2-1, 1.5]) cube([25, 2, 1]);
        translate([50, width/2-1, 1.5]) cube([25, 2, 1]);
        translate([60, 200, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text("Addr/Data", halign="left", valign="center", size=7,
                font=title_font);
      }
    }
  }
}
//
//  This panel just has the specified text on a 1 frame high panel.  It is used as a
//  nameplate.
//
module panel_nameplate(name)
{
  difference()
  {
    bbs_panel(10, 1);
    translate([10, 200, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text(name, halign="left", valign="center", size=7,
                font=title_font);

  }
}
//
//  This panel has various switches and LEDs for controls and status
//
module panel_control()
{
  spacing = (width-30)/8;
  led_x = 55;
  switch_x = 25;
  on_labels  = ["Run", "Start", "Auto", "Addr", "Dep", "Exam", "Rdy", "Power"];
  off_labels = ["Pause", "Stop", "Man", "Data", "", "", "", "Off"];
  union()
  {
    difference()
    {
      bbs_panel(10, 4);
      union()
      {
        for(a = [1:8])
        {
          y = width/2 - (4.5-a)*spacing;
          if(a != 2)
          {
            translate([switch_x, y, -0.1]) bbs_spdt_switch_cutout(2.2);
          }
          if(a < 8)
          {
            translate([led_x + 3, y + spacing/2 - 2.5, -0.1]) cube([2, 5, 2.2]);
            translate([led_x - 4, y + spacing/2 - 2.5, -0.1]) cube([2, 5, 2.2]);
          }
          translate([led_x, y, -0.1]) bbs_led_oval(2.2);
          translate([switch_x + 11, y, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text(on_labels[8 - a], halign="center", valign="baseline", size=6,
                font=number_font);
          translate([switch_x - 15, y, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text(off_labels[8 - a], halign="center", valign="baseline", size=6,
                font=number_font);
        }
        translate([70, 200, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text("Control", halign="left", valign="center", size=7,
                font=title_font);
      }
    }
  }
}

module panel_mode()
{
  spacing = (width-30)/8;
  led_x = 30;
  led_labels  = ["User", "Sup", "Exec", "Kern", "I/O", "Inst", "Data", "Intr"];
  union()
  {
    difference()
    {
      bbs_panel(10, 3);
      union()
      {
        for(a = [1:8])
        {
          y = width/2 - (4.5-a)*spacing;
          if(a < 8)
          {
            translate([led_x + 3, y + spacing/2 - 2.5, -0.1]) cube([2, 5, 2.2]);
            translate([led_x - 4, y + spacing/2 - 2.5, -0.1]) cube([2, 5, 2.2]);
          }
          translate([led_x, y, -0.1]) bbs_led_oval(2.2);
          translate([led_x - 15, y, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text(led_labels[8 - a], halign="center", valign="baseline", size=6,
                font=number_font);
        }
        translate([led_x + 15, 200, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text("Modes", halign="left", valign="center", size=7,
                font=title_font);
      }
    }
  }
}

module panel_power()
{
  difference()
  {
    bbs_panel(10, 2);
    union()
    {
      translate([15, 40, -0.1]) rotate([0, 0, 90]) bbs_usb_b_cutout(4);
      translate([15, 80, -0.1]) rotate([0, 0, 90]) bbs_micro_usb_cutout(4);
      translate([34, 200, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text("Do not engergize both", halign="left", valign="center",
              size=7, font=title_font);
      translate([24, 200, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text("connectors simultaneously", halign="left", valign="center",
              size=7, font=title_font);
      translate([7, 80, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text("USB-C", halign="center", valign="center",
              size=5, font=title_font);
      translate([25, 40, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text("USB-B", halign="center", valign="center",
              size=5, font=title_font);
    }
  }
}

rotate([0, 0, 90])
{
//  translate([00, 0, 0]) panel_control();
  translate([90, 0, 0]) panel_mode();
//  translate([00, 0, 0]) panel_switch(0);
//  translate([90, 0, 0]) panel_switch(8);
//  translate([90, 0, 0]) panel_switch(16);
//  translate([00, 0, 0]) panel_switch(24);
//  translate([100, 0, 0]) panel_nameplate("RPi-3/1 Mainframe");
//  translate([00, 0, 0]) panel_power();
}
