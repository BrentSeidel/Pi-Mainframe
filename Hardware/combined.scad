use <Trays.scad>
use <panels.scad>
use <../../Things/bbs_rack.scad>

rotate([0, 0, 90]) bbs_rack2(10, 5, 4);
translate([-6, 74, 93]) rotate([-90, 0, 0]) rotate([0, 0, 90]) color("red") panel_tray();
translate([-6, 52, 93]) rotate([-90, 0, 0]) rotate([0, 0, 90]) color("red") panel_tray();
translate([-6, 30, 93]) rotate([-90, 0, 0]) rotate([0, 0, 90]) color("red") panel_tray();
translate([-5.5, 2, 8]) rotate([0, 0, 90]) color("green") motherboard_tray();
