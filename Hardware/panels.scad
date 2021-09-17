//
//  These are a collection of panels for the Raspberry Pi mainframe simulator.  The panels
//  include switches and LEDs and a couple of LCD displays.  Select which panels you
//  want, then you can render them for printing.  My printer is not big enough to print
//  them all in one batch.
//
use <../../Things/bbs_panel.scad>
use <../../Things/bbs_leds.scad>
use <../../Things/bbs_switches.scad>
use <../../Things/bbs_lcd_20x4.scad>
use <../../Things/bbs_lcd7.scad>

width = 220;
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
//          translate([40, y, -0.1]) bbs_led_cutout(5, 2.2);
          translate([40, y, -0.1]) bbs_led_oval(2.2);
          translate([28, y, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text(str(a + start - 1), halign="center", valign="center", size=6,
                font="Liberation Sans::style=Bold");
        }
        translate([5, width/2-1, 1.5]) cube([25, 2, 1]);
        translate([50, width/2-1, 1.5]) cube([25, 2, 1]);
        translate([60, 200, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text("Addr/Data", halign="left", valign="center", size=6,
                font="Liberation Sans::style=Bold");
      }
    }
//    for(a = [1:8])
//    {
//      y = width/2 - (4.5-a)*spacing;
//      translate([40, y, 0]) bbs_led_mount(5, 2);
//    }
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
              text(name, halign="left", valign="center", size=6,
                font="Liberation Sans::style=Bold");

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
  on_labels  = ["Run", "Ready", "Auto", "Addr", "Dep", "Exam", "", ""];
  off_labels = ["Pause", "Halt", "Man", "Data", "", "", "", ""];
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
          translate([switch_x, y, -0.1]) bbs_spdt_switch_cutout(2.2);
          if(a < 8)
          {
            translate([led_x + 3, y + spacing/2 - 2.5, -0.1]) cube([2, 5, 2.2]);
            translate([led_x - 4, y + spacing/2 - 2.5, -0.1]) cube([2, 5, 2.2]);
          }
          translate([led_x, y, -0.1]) bbs_led_oval(2.2);
//          translate([led_x, y, -0.1]) bbs_led_cutout(5, 2.2);
          translate([switch_x + 11, y, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text(on_labels[8 - a], halign="center", valign="baseline", size=6,
                font="Liberation Sans::style=Bold");
          translate([switch_x - 15, y, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text(off_labels[8 - a], halign="center", valign="baseline", size=6,
                font="Liberation Sans::style=Bold");
        }
        translate([70, 200, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text("Control", halign="left", valign="center", size=6,
                font="Liberation Sans::style=Bold");
      }
    }
  }
//    for(a = [1:8])
//    {
//      y = width/2 - (4.5-a)*spacing;
//      translate([led_x, y, 0]) bbs_led_mount(5, 2);
//    }
}

module panel_lcd7()
{
  difference()
  {
    bbs_panel(10, 6);
    union()
    {
      translate([110, 30, -0.1]) rotate([0, 0, 90]) bbs_lcd7();
    }
  }
}

module panel_lcd20x4()
{
  switch_x = 15;
  switch_y = 150;
  switch_space_y = 35;
  switch_space_x = 25;
  union()
  {
    difference()
    {
      union()
      {
        bbs_panel(10, 4);
      }
      union()
      {
        translate([70, 30, -0.1]) rotate([0, 0, 90]) bbs_20x4_lcd_cutouts(2.2, 10);
        translate([switch_x, switch_y, -0.1]) bbs_spdt_switch_cutout(2.2);
        translate([switch_x, switch_y + switch_space_y, -0.1]) bbs_spdt_switch_cutout(2.2);
        translate([switch_x+switch_space_x, switch_y, -0.1]) rotate([0, 0, 270]) bbs_pot2_cutout(2.2);
        translate([switch_x+switch_space_x, switch_y + switch_space_y, -0.1]) rotate([0, 0, 270]) bbs_pot2_cutout(2.2);
        for(a = [6:8])
        {
          y = a*(width - 30)/8;
          translate([65, y, -0.1]) bbs_led_cutout(5, 3);
        }
      }
    }
    for(a = [6:8])
    {
      y = a*(width - 30)/8;
      translate([65, y, 0]) bbs_led_mount(5, 2);
    }
  }
}

//
module test()
{
  difference()
  {
    translate([0, 0, 1]) cube([15, 10, 2], center=true);
    bbs_led_oval(4);
  }
}
rotate([0, 0, 90])
{
//  translate([100, 0, 0]) panel_nameplate("RPi-3/1 Mainframe");
  translate([00, 0, 0]) panel_control();
//  translate([90, 0, 0]) panel_switch(8);
//  translate([00, 0, 0]) panel_switch(0);
//  translate([140, 0, 0]) panel_lcd7();
//  translate([0, 0, 0]) panel_lcd20x4();
//    bbs_pot2_knob();
//  translate([40, 150, -0.1]) rotate([0, 0, 270]) color("red") bbs_pot2();
//  translate([40, 185, -0.1]) rotate([0, 0, 270]) color("red") bbs_pot2();
//  translate([15, 150, -0.1]) color("red") bbs_spdt_switch();
//  translate([15, 185, -0.1]) color("red") bbs_spdt_switch();
}
