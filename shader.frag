#version 120
uniform ivec4 u;

// Placeholder as of now.. old codes dug up from various hobby events..

// // From the Fuzzdealer entry of 2016.
// void main()
// {
//     //    Build a vec4 with time - varying integral pars; r.t used as iterative output
//     vec4 u = vec4(u); u.r /= 1000.; 

//     vec4 r=vec4(int(u.r/8/4),2,2,2+int(u.r/8));
//     for(int a=6;--a>0;){
//         vec4 o=1-2*gl_FragCoord/u.tptt;
//         o.r/=u.p/u.t; // cartesian, with correct aspect
//         // Main "Movement" (could have been stripped down more ? ) :
//         o = sin(a * 
//                      (o
//                        +2*a*sin(u.r/8+o.grrr*fract(u.r/8/3))
//                        +u.r/8/3/2*a*sin(u.r/8+r.arrr*fract(u.r/8/3))
//                      )
//                   )
//               /(1+fract(u.r/2*r.r)) // pulsation
//             ; 
//       	r.t=r.t*.4+sin(dot(o.st,o.st))*(.4-sin(r.a*atan(o.t,o.s)));
//      }
//      gl_FragColor=
//             vec4(r.t*r.t*r.t,r.ttt) // cyan - ish specular glow
// //              vec4(r.t,r.t,0,1)      // plain B& W(many bytes smaller)
//              * sin(u.r / 8 / 3 / 2) // cheap - ish fade in(redundant ? too much, even ? )
//             +u.r/8/4/3*u.r/8/4/3*u.r/8/4/3*u.r/8/4/3; // ; fade - to - white in end
// }

/* From a beginner workshop, one of many.. */

// Some Shadertoys for the absolute beginner - with thorough
// explanations in comments, but optimized for a tutor-driven
// group learning session where syntax errors are the problem
// of the tutor, not the student :).
// 
// To my students, with love, on 2021-12-15 Winter Garden Day,
// Faculty of Information Technology, University of Jyväskylä,
// Finland.
//
// qma

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
vec3 i_checkerBoardTexture(vec2 uv, float density){
  return vec3(mod(floor(density*uv.x)+floor(density*uv.y), 2.));
}

vec3 i_testTexture(vec2 uv){
   return max( 0.2  * i_checkerBoardTexture(uv, 1.0)
               + 0.1 * i_checkerBoardTexture(uv, 10.0),
               vec3(uv.xy,0.0));
}

/* Normalized pixel coordinates (y in [-1,1], x follows aspect ratio).
 */
vec2 i_screenCoord(vec2 pix, vec2 resolution){
    return (2*pix - resolution) / resolution.yy;
}

/* Ray-trace to a sphere.
 *
 * Algorithm:
 *   ..
 */
struct Hit{
   vec3 P; // World position of hit
   vec3 n; // Normal vector of hit
};

Hit rtSphere(vec3 Ro, vec3 Rd, float tmin, vec4 ball){
  Ro -= ball.xyz;
  float a = dot(Rd,Rd);
  float b = 2 * dot(Rd, Ro);
  float c = dot(Ro,Ro) - ball.w * ball.w;
  float D = b * b - 4 * a * c;
  // if (D < .0) return Hit(vec3(0),vec3(0));
  // float tres = min( (-b + sqrt(D) ) / (2*a),
  //                   (-b - sqrt(D) ) / (2*a)  );
  float tres = (D<0)?1e9:min( (-b + sqrt(D) ) / (2*a),
                          (-b - sqrt(D) ) / (2*a)  );

  return Hit(ball.xyz + Ro + tres*Rd, normalize(Ro + tres*Rd)) ;
}

void main()
{
    vec3 col;

    float iTime = u.x/1000.;
    vec2 s = i_screenCoord(gl_FragCoord.xy, u.yz);

    // Just to know where we are at - Checkerboard for tick marks:
    //fragColor = vec4(i_testTexture(s),1); return;

    //vec4 sph1 = vec4(sin(iTime),cos(iTime),0,1);
    vec4 sph1 = vec4(3*sin(iTime),3*cos(iTime),10*sin(iTime*3),1);
    vec3 eye = vec3(0,0,30);
    vec3 rd = normalize(vec3(s.xy, -4));
    Hit h = rtSphere(eye, rd, 0., sph1);
    col = sin(h.n*16);
//    vec3 point = eye+t*rd;    
//    col = point;
    
    // Output to screen
    gl_FragColor = vec4(col,1.0);
}
