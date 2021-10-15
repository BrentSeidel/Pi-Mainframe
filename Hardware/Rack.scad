use <../../Things/bbs_rack.scad>

screw_rad = 4.5/2;
hole_rad = 5.0;

module flange(frames, off)
{
  difference()
  {
    union()
    {
      cube([frames*20, 37.3, 2]);
      cube([frames*20, 2, 20]);
    }
    union()
    {
      for(a = [10:20:(frames*20)])
      {
        translate([a, -1, 10]) rotate([-90, 0, 0]) cylinder(r=screw_rad, h=6, $fn=12);
      }
      for(a = [off:25.2:(frames*20)])
      {
        translate([a, 37.2-12, -1]) cylinder(r=hole_rad, h=6);
      }
    }
  }
}

//rotate([0, 0, 90]) bbs_rack2(10, 6, 4);
flange(10, 10);
