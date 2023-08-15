//
//  Tray for holding the Arduino Due and some extra boards.
//
use <../../Things/bbs_tray.scad>
use <../../Things/bbs_breadboard.scad>
use <../../Things/bbs_arduino.scad>
use <../../Things/bbs_constants.scad>
use <../../Things/bbs_connectors.scad>
use <../../Things/bbs_boards.scad>
use <../../Things/bbs_RaspberryPi.scad>

module motherboard_tray()
{
  difference()
  {
    union()
    {
      bbs_tray(10, 5, false);
    }
    union()
    {
      translate([85, 25, -1]) rotate([0, 0, 90]) bbs_d_cutout(1, 5);
      translate([63, 25, -1]) rotate([0, 0, 90]) bbs_d_cutout(1, 5);
      translate([41, 25, -1]) rotate([0, 0, 90]) bbs_d_cutout(1, 5);
      translate([10, 50, -1]) minkowski()
      {
        cube([100, 150, 10]);
        cylinder(r=1, h=10);
      }
    }
  }
}

rpi_x = 60;
rpi_y = 30;
q_proto_x = 70;
q_proto_y = 120;
module rpi_tray()
{
  screw_size = 3*screw_6_size()/4;
  screw_hole = screw_6_size()/2;
  difference()
  {
    union()
    {
      bbs_tray(10, 3, false);
      translate([rpi_x, rpi_y, 3]) rotate([0, 0, 90]) rPi3_standoffs(5, screw_size, 12);
      translate([q_proto_x, q_proto_y, 3]) rotate([0, 0, 90]) bbs_quarter_permaprotoboard_standoffs(5, screw_size, 12);
      translate([73, 8, 0]) cube([5, 35, 20]);
    }
    union()
    {
      translate([rpi_x, rpi_y, -2]) rotate([0, 0, 90]) rPi3_standoffs(5 + 6, screw_hole, 12);
      translate([q_proto_x, q_proto_y, -2]) rotate([0, 0, 90]) bbs_quarter_permaprotoboard_standoffs(5 + 6, screw_hole, 12);
      translate([80, 25, 12]) rotate([0, -90, 0]) rotate([0, 0, 90]) bbs_d_cutout(1, 10);
      translate([10, 10, -1]) minkowski()
      {
        cube([60, 35, 10]);
        cylinder(r=1, h=10);
      }
      translate([10, 60, -1]) minkowski()
      {
        cube([60, 45, 10]);
        cylinder(r=1, h=10);
      }
      translate([10, 130, -1]) minkowski()
      {
        cube([60, 20, 10]);
        cylinder(r=1, h=10);
      }
      translate([10, 170, -1]) minkowski()
      {
        cube([60, 25, 10]);
        cylinder(r=1, h=10);
      }
    }
  }
}

module BreakoutBoard()
{
  difference()
  {
    color("red") cube([36.83, 24.13, 2]);
    union()
    {
      translate([5.08, 5.08, -0.1]) cylinder(5, 2.418/2);
      translate([31.75, 5.08, -0.1]) cylinder(5, 2.418/2);
    }
  }
}

module Breakout_standoffs(h, r, f)
{
  union()
  {
    translate([5.08, 5.08, -0.1]) cylinder($fn=f, h=h, r=r);
    translate([31.75, 5.08, -0.1]) cylinder($fn=f, h=h, r=r);
  }
}
breakout_x = 75;
breakout_y = 120;
module rpi_tray_new()
{
  screw_size = 3*screw_6_size()/4;
  screw_hole = screw_6_size()/2;
  difference()
  {
    union()
    {
      bbs_tray(10, 3, false);
      translate([rpi_x, rpi_y, 3]) rotate([0, 0, 90]) rPi3_standoffs(5, screw_size, 12);
      translate([breakout_x, breakout_y, 3]) rotate([0, 0, 90]) Breakout_standoffs(5, screw_size, 12);
    }
    union()
    {
      translate([rpi_x, rpi_y, -2]) rotate([0, 0, 90]) rPi3_standoffs(5 + 6, screw_hole, 12);
      translate([breakout_x, breakout_y, -2]) rotate([0, 0, 90]) Breakout_standoffs(5 + 6, screw_hole, 12);
      translate([10, 10, -1]) minkowski()
      {
        cube([60, 35, 10]);
        cylinder(r=1, h=10);
      }
      translate([10, 60, -1]) minkowski()
      {
        cube([60, 45, 10]);
        cylinder(r=1, h=10);
      }
      translate([10, 120, -1]) minkowski()
      {
        cube([50, 70, 10]);
        cylinder(r=1, h=10);
      }
    }
  }
}

half_proto_x = 55;
half_proto_y = 10;
module panel_tray()
{
  screw_size = 3*screw_6_size()/4;
  screw_hole = screw_6_size()/2;
  difference()
  {
    union()
    {
      bbs_tray(10, 3, false);
      translate([half_proto_x, half_proto_y, 3]) rotate([0, 0, 90])
        bbs_half_permaprotoboard_standoffs(3, screw_size, 12);
      translate([half_proto_x, half_proto_y+90, 3]) rotate([0, 0, 90])
        bbs_half_permaprotoboard_standoffs(3, screw_size, 12);
      translate([73, 8, 0]) cube([5, 35, 20]);
    }
    union()
    {
      translate([half_proto_x, half_proto_y, -2]) rotate([0, 0, 90]) bbs_half_permaprotoboard_standoffs(5 + 6, screw_hole, 12);
      translate([half_proto_x, half_proto_y+90, -2]) rotate([0, 0, 90]) bbs_half_permaprotoboard_standoffs(5 + 6, screw_hole, 12);
      translate([80, 25, 12]) rotate([0, -90, 0]) rotate([0, 0, 90]) bbs_d_cutout(1, 10);
      translate([70, 60, -1]) rotate([0, 0, 90]) bbs_d_cutout(1, 5);
      translate([70, 95, -1]) rotate([0, 0, 90]) bbs_d_cutout(1, 5);
      translate([70, 130, -1]) rotate([0, 0, -90]) bbs_d_cutout(1, 5);
      translate([70, 165, -1]) rotate([0, 0, -90]) bbs_d_cutout(1, 5);
      translate([35, 9, -1]) minkowski()
      {
        cube([25, 191, 10]);
        cylinder(r=1, h=10);
      }
      translate([0, 9, -1]) minkowski()
      {
        cube([20, 191, 10]);
        cylinder(r=1, h=10);
      }
    }
  }
}

module IO_tray()
{
  screw_size = 3*screw_6_size()/4;
  screw_hole = screw_6_size()/2;
  difference()
  {
    union()
    {
      bbs_tray(10, 3, false);
      translate([77, 50, 0.1]) rotate([0, 0, 90]) bbs_GPIO_standoffs(7, screw_size, 10);
    }
    union()
    {
      translate([77, 50, 0]) rotate([0, 0, 90]) bbs_GPIO_standoffs(10, screw_hole, 10);
      translate([0, 9, -1]) minkowski()
      {
        cube([25, 191, 10]);
        cylinder(r=1, h=10);
      }
      translate([42, 49, -1]) minkowski()
      {
        cube([26, 100, 10]);
        cylinder(r=1, h=10);
      }
    }
  }
}

//rotate([0, 0, 90])
union()
{
//  rpi_tray_new();
//  color("red") translate([rpi_x, rpi_y, 10]) rotate([0, 0, 90]) bbs_rPi3();
//  color("red") translate([breakout_x, breakout_y, 10]) rotate([0, 0, 90]) BreakoutBoard();
  IO_tray();
  color("red") translate([77, 50, 10]) rotate([0, 0, 90]) bbs_GPIO();
//  color("white") translate([half_proto_x, half_proto_y, 5]) rotate([0, 0, 90]) bbs_half_permaprotoboard();
//  color("white") translate([half_proto_x, half_proto_y+90, 5]) rotate([0, 0, 90]) bbs_half_permaprotoboard();
}

