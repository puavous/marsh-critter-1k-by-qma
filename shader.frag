#version 120
uniform ivec4 u;

// Placeholder as of now.. old codes dug up from various hobby events..

// From the Fuzzdealer entry of 2016.
void main()
{
    //    Build a vec4 with time - varying integral pars; r.t used as iterative output
    vec4 u = vec4(u); u.r /= 1000.; 

    vec4 r=vec4(int(u.r/8/4),2,2,2+int(u.r/8));
    for(int a=6;--a>0;){
        vec4 o=1-2*gl_FragCoord/u.tptt;
        o.r/=u.p/u.t; // cartesian, with correct aspect
        // Main "Movement" (could have been stripped down more ? ) :
        o = sin(a * 
                     (o
                       +2*a*sin(u.r/8+o.grrr*fract(u.r/8/3))
                       +u.r/8/3/2*a*sin(u.r/8+r.arrr*fract(u.r/8/3))
                     )
                  )
              /(1+fract(u.r/2*r.r)) // pulsation
            ; 
      	r.t=r.t*.4+sin(dot(o.st,o.st))*(.4-sin(r.a*atan(o.t,o.s)));
     }
     gl_FragColor=
            vec4(r.t*r.t*r.t,r.ttt) // cyan - ish specular glow
//              vec4(r.t,r.t,0,1)      // plain B& W(many bytes smaller)
             * sin(u.r / 8 / 3 / 2) // cheap - ish fade in(redundant ? too much, even ? )
            +u.r/8/4/3*u.r/8/4/3*u.r/8/4/3*u.r/8/4/3; // ; fade - to - white in end
}

/* From a beginner workshop, one of many.. */

// // Some Shadertoys for the absolute beginner - with thorough
// // explanations in comments, but optimized for a tutor-driven
// // group learning session where syntax errors are the problem
// // of the tutor, not the student :).
// // 
// // To my students, with love, on 2021-12-15 Winter Garden Day,
// // Faculty of Information Technology, University of Jyväskylä,
// // Finland.
// //
// // qma

// /* Checker-board with 'density' squares on unit length.
//  * For usual texture uv coordinates, [0,1]x[0,1] is a black square.
//  * When uv are normalized screen coordinates, [-1,1]x[-1,1] has 4 squares.
//  *
//  * Algorithm:
//  *
//  * Convert scaled x and y to integers, then take x+y modulo 2.
//  * Draw numbers on grid paper to believe it works.
//  * Simplest I came up with today.
//  */
// vec3 i_checkerBoardTexture(vec2 uv, float density){
//   vec2 i_iuv = floor(density * uv);
//   return vec3(mod(i_iuv.x + i_iuv.y, 2.0));
// }

// /* Normalized pixel coordinates (y in [-1,1], x follows aspect ratio)
//  *
//  * Algorithm:
//  *
//  * The math reads, starting from within the first parentheses:
//  * Center pixels, then divide in order to scale centered x and y
//  * uniformly so that the screen coordinate of y, in particular,
//  * remains bounded by -1 and 1. A beginner should notice the use
//  * of "yy" in "res.yy" - Repeating coordinate identifiers 
//  * is business as usual. It is called swizzling. "V.xyyx", "V.zzz",
//  * and "V.zxxx" are all very fine ways to access a vector named V.
//  * And here "V.rag" means the same as "V.xwy". In the long run, you
//  * will want to read the specification.
//  */
// vec2 i_screenCoord(vec2 pix, vec2 res){
//     return (pix - res / 2.0) / (res.yy / 2.0);
// }

// void mainImage( out vec4 fragColor, in vec2 fragCoord )
// {
//     float iTime = u.x/1000.;
//     vec2 iResolution = u.yz;
//     vec2 s = i_screenCoord(fragCoord.xy, iResolution.xy);

//     // Just to know where we are at - Checkerboard for tick marks:
//     bool i_debug = true;
//     vec3 bg;
//     if (i_debug){
//       bg =  0.2  * i_checkerBoardTexture(s, 1.0)
//           + 0.1 * i_checkerBoardTexture(s, 10.0);
//           // Further location visualization: paint x as red and y as green:
//           bg = max(bg, vec3(s.xy, 0.0));
//     } else {
//       bg = vec3(0);
//     }
//     // ALWAYS start with exact knowledge of:
//     //   Where is your screen center, which way is up and right, how far
//     //   is length 1.0 and how far is each screen edge,
//     //   which dimension will be scaled when aspect ratio changes, etc...
//     //   Use color and shape to figure it out, ANY TIME you're unsure.
//     //   Additionally, have pen and paper prepared and ready for use.
//     //
//     // ALWAYS write a small sanity check / memory refresher code like the above.
//     // Because: A platform can define coordinates, resolutions, and other
//     // things differently from the next! Also, it will take some time for
//     // you to learn the idea of "screen coordinates" if you are a beginner.
    
//     // Normally, you delete such a test/helper picture later. While still
//     // working and learning, you can easily toggle between the helper grapic
//     // and blackness by using comments or #if 0 ... #else ... #endif tricks.

//     vec2 loc1 = vec2(0.,0.);
//     vec2 loc2 = vec2(0.0 + sin(1.00*iTime),0.0+cos(1.60*iTime));
//     vec2 loc3 = vec2(0.1 + sin(.3*iTime),-0.2+cos(.7*iTime));
    
//     vec3 vari1 = -.2+abs(sin(20.0*length(s+loc1))) * vec3(.4, 0, .1);
//     vec3 vari2 = -.2+abs(sin(20.0*length(s+loc2))) * vec3(.4,.2, .1);
//     vec3 vari3 = -.2+abs(sin(20.0*length(s+loc3))) * vec3(.4,.2, .1);

//     vec3 jumpu = vec3(1.-mod(iTime, 1.0));
//     vec3 col = bg + jumpu * vari1;
//     //vec3 col = 0.2  * i_checkerBoardTexture (s, 1.0)
//     //          + 0.1 * i_checkerBoardTexture (s, 10.0);    
    
//     // Output to screen
//     fragColor = vec4(col,1.0);
// }

// void main() {
//   mainImage(gl_FragColor, gl_FragCoord.xy);
// }
