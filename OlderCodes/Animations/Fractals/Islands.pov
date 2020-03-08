/* From "A SURVEY OF COMPUTATIONAL PHYSICS", Python eBook Version
   by RH Landau, MJ Paez, and CC Bordeianu
   Copyright Princeton University Press, Princeton, 2012; Book  Copyright R Landau, 
   Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2012.
   Support by National Science Foundation , Oregon State Univ, Microsoft Corp */
   
// Islands.pov  Pov-Ray program to create Islands, by Manuel J Paez
plane {
  <0, 1, 0>, 0                                                      // Sky
  pigment { color rgb <0, 0, 1> }
  scale 1
  rotate <0, 0, 0>
  translate y*0.2
}
global_settings {
  adc_bailout 0.00392157
  assumed_gamma 1.5
  noise_generator 2
}
#declare Island_texture = texture {
  pigment {
    gradient <0, 1, 0>                               // Vertical direction
    color_map {                                       // Color the islands
      [ 0.15 color rgb <1, 0.968627, 0> ]
      [ 0.2  color rgb <0.886275, 0.733333, 0.180392>   ]
      [ 0.3  color rgb <0.372549, 0.643137, 0.0823529>  ]
      [ 0.4  color rgb <0.101961, 0.588235, 0.184314>   ]
      [ 0.5  color rgb <0.223529, 0.666667, 0.301961>   ]
      [ 0.6  color rgb <0.611765, 0.886275, 0.0196078>  ]
      [ 0.69 color rgb <0.678431, 0.921569, 0.0117647>  ]
      [ 0.74 color rgb <0.886275, 0.886275, 0.317647>   ]
      [ 0.86 color rgb <0.823529, 0.796078, 0.0196078>  ]
      [ 0.93 color rgb <0.905882, 0.545098, 0.00392157> ]
      }
   }
   finish {
     ambient rgbft <0.2, 0.2, 0.2, 0.2, 0.2>
     diffuse 0.8
   }
}
camera {                            // Camera characteristics and location
  perspective
  location <-15, 6, -20>                                   // Located here
  sky <0, 1, 0>
  direction <0, 0, 1>
  right <1.3333, 0, 0>
  up <0, 1, 0>
  look_at <-0.5, 0, 4>                             //looking at that point
  angle 36
}
light_source {<-10, 20, -25>, rgb <1, 0.733333, 0.00392157>}      // Light
 
#declare Islands = height_field {          // Takes  gif and finds heights
  gif "d:\pov\montania.gif"                    // Windows directory naming
  scale <50, 2, 50>
  translate <-25, 0, -25>
}
object {                                                        // Islands
  Islands
  texture {
    Island_texture
    scale 2
  }
}
box {                                  // Upper face of the box is the sea
  <-50, 0, -50>, <50, 0.3, 50>          // Location of 2 opposite vertices
  translate <-25, 0, -25>
  texture {                                              // Simulate waves
    normal {
      spotted
      0.4
      scale <0.1, 1, 0.1>
    }
    pigment { color rgb <0.164706, 0.556863, 0.901961> }
  }
}
fog {                                         // A constant fog is defined
   fog_type 1
   distance 30
    rgb <0.984314, 1, 0.964706> 
}
