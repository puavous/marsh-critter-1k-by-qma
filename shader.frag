#version 140
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

/** A test texture: 'tick marks' at 0.1 and 1.0 intervals +colors for xy orientation.*/
vec3 i_testTexture(vec2 uv){
   return max( 0.2  * i_checkerBoardTexture(uv, 1.0)
               + 0.1 * i_checkerBoardTexture(uv, 10.0),
               vec3(uv.xy,0.0));
}

/* Normalized pixel coordinates (y in [-1,1], x follows aspect ratio).
 */
vec2 i_screenCoord(vec2 pix, vec2 resolution){
    return (2*pix - resolution) / resolution.y;
}

/** Intersect ray with plane defined by triangle abc as a=a, u=b-a, v=c-a. Return barycentric coordinates and distance to hit as (beta,gamma,t). */
vec3 i_rtMiniPlane2(vec3 Ro, vec3 Rd, vec3 a, vec3 u, vec3 v){
  return inverse(mat3(-u,-v,Rd))*(a-Ro);
}

/* Structure for ray-tracing state.. */
// struct Hit{
//   float d; // Distance of hit along ray direction.
//   vec3 P; // World position of hit
//   vec3 n; // Normal vector of hit
// };


/* Ray-trace to a sphere.
 *
 * Algorithm:
 *   ..
 */

// void rtSphere(inout Hit hit, vec3 Ro, vec3 Rd, float tmin, vec4 ball){
//   Ro -= ball.xyz;
//   float a = dot(Rd,Rd);
//   float b = 2 * dot(Rd, Ro);
//   float c = dot(Ro,Ro) - ball.w * ball.w;
//   float D = b * b - 4 * a * c;
//   // if (D < .0) return Hit(vec3(0),vec3(0));
//   // float tres = min( (-b + sqrt(D) ) / (2*a),
//   //                   (-b - sqrt(D) ) / (2*a)  );
//   if (D >= 0) {
//     float tres = min( (-b + sqrt(D) ) / (2*a),
//                      (-b - sqrt(D) ) / (2*a)  );
//     if ((tmin < tres) && (tres < hit.d )){
//       hit = Hit(tres, ball.xyz + Ro + tres*Rd, normalize(Ro + tres*Rd));
//     }
//   }
//   //float tres = (D<0)?1e9:min( (-b + sqrt(D) ) / (2*a),
//   //                        (-b - sqrt(D) ) / (2*a)  );
//   //return Hit(ball.xyz + Ro + tres*Rd, normalize(Ro + tres*Rd)) ;
// }

// void rtPlane(inout Hit hit, vec3 Ro, vec3 Rd, vec3 plane_p, vec3 plane_n){
//   float D = dot(Rd, plane_n);
//   if (D>0.){
//     float tres = dot(plane_p - Ro, plane_n) / D;
//     hit = Hit(tres, Ro + tres*Rd, plane_n);
//   }
// }

/** Intersect ray with plane; give barycentric coordinates to triangle abc.
* Algorithm.. course notes http://users.jyu.fi/~nieminen/tgp21/tiea311_2019_lec17.pdf
*/
// vec2 rtPlane3(inout Hit hit, vec3 Ro, vec3 Rd, vec3 a, vec3 b, vec3 c){
//   mat3 A=mat3(a-b, a-c, Rd);
//   mat3 Ainv=inverse(A);
//   vec3 x=Ainv*(a-Ro);
//   float t=x.z;
//   if ((0<t) && (t<hit.d)){
//      hit = Hit(t, Ro + t*Rd, cross(b-a,c-a));
//   }
//   return x.xy;
// }
// /** Super minimalistic triangle intersect, returns (alpha,beta,t)*/
// vec3 rtMiniPlane(vec3 Ro, vec3 Rd, vec3 a, vec3 b, vec3 c){
//   return inverse(mat3(a-b,a-c,Rd))*(a-Ro);
// }


// void raytrace_plane_experiment(float iTime, vec2 s){
//     vec3 col = vec3(0);

//     // Just to know where we are at - Checkerboard for tick marks:
//     //fragColor = vec4(i_testTexture(s),1); return;
    
//     const int nns = 3;
//     const int Nsamp = nns*nns; // Supersample at random points inside pixel
//     for (int isample = 0; isample < Nsamp; isample++){

//     //vec4 sph1 = vec4(sin(iTime),cos(iTime),0,1);
//     vec3 eye = vec3(0,0,30);
// //    vec3 rd = normalize(vec3(s.xy, -4));
//     // Sample from some, quite uncontrolled, points around pixel:
//     //vec2 i_ssamp = s.xy + 2*vec2(cos(5*isample+s.x*3),sin(2*isample+s.y*7))/u.yz;
//     int ix = isample/nns;
//     int iy = isample%nns;
//     vec2 i_ssamp = s.xy + vec2(ix,iy)/u.yz/nns;


//     vec3 rd = normalize(vec3(i_ssamp, -4));
//     // Hit h = Hit(1e9,vec3(0),vec3(1));

//     // vec4 sph1 = vec4(3*sin(iTime),3*cos(iTime),10*sin(iTime*3),1);
//     // rtSphere(h, eye, rd, 0., sph1);

// //    vec3 plane1p = vec3(0,-2-sin(iTime),0);
// //    vec3 plane2n = vec3(0,1,0);
// //    rtPlane(h, eye, rd, plane1p, plane2n);

//     // vec2 uv=rtPlane3(h, eye, rd,
//     //   vec3(0,-2,0),
//     //   vec3(1,-2,0),
//     //   vec3(0,-2,1));

//     // if (h.d<1e9){
//     //   col = vec3(sin(length(uv)+iTime));
//     // } else {
//     //   col = vec3(0,0,dot(rd,vec3(1,1,-1)));
//     // }
//     // vec3 uvt=rtMiniPlane(eye, rd,
//     //    vec3(0,-2,0),
//     //    vec3(1,-2,0),
//     //    vec3(0,-2,1));
//     // if (uvt.z>0){
//     //      col = vec3(sin(length(uvt.xy)-iTime));
//     //    } else {
//     //      col = vec3(0.3,0,dot(rd,vec3(1,1,-1)));
//     // }

//     vec3 uvt=i_rtMiniPlane2(eye, rd,
//        vec3(0,-2,iTime),
//        vec3(1,0,0),
//        vec3(0,0,1));

//     vec3 uvt2=i_rtMiniPlane2(eye, rd,
//        vec3(-9,1-iTime,iTime),
//        vec3(0,0,-1),
//        vec3(0,1,0));

//     if (uvt.z<0) uvt.z=1e9;
//     if (uvt2.z<0) uvt2.z=1e9;
//     // uvt = (uvt2.z < uvt.z)?uvt2:uvt;

// //    if ((0 < uvt2.z ) && (uvt2.z < uvt.z)) uvt = uvt2;
// //    uvt = uvt2;
// //    uvt = min(uvt,uvt2);

// //    col += uvt.z / Nsamp / 100; continue;

//     if (uvt.z < 1e9){
//          //col += 1./Nsamp * vec3(fract(uvt.x)<.1 || fract(uvt.y)<.1);
//          //col += 1./Nsamp * vec3(sin(length(uvt.xy)-iTime));
//          col += 1./Nsamp * i_checkerBoardTexture(uvt.xy, 1)/uvt.z*39;
//        } else {
//          float i_intens = max(0.1,dot(rd,vec3(.3,3-iTime/7,-1)));
//          col += 1./Nsamp * i_intens * vec3(0.3,0.1,i_intens);
//     }
//     if (uvt2.z < uvt.z){
//       col += 1./Nsamp * i_checkerBoardTexture(uvt2.xy, 1)/uvt2.z*39;
//     }


// //    col = sin(h.P);
// //    vec3 point = eye+t*rd;    
// //    col = point;

//     }
    
//     // Output to screen
//     gl_FragColor = vec4(col,1.0);

// }

/* Let's make it clear that, once again, I'm using Inigo's treasure
 * trove of tutorials and examples! */

/** Sphere s with center and radius: (x, y, z, radius) */
float i_sdSphereAt( vec3 p, vec4 s )
{
  return length(p-s.xyz)-s.w;
}

// /** Box; verbatim from Inigo's tutorial at https://iquilezles.org/articles/distfunctions/ */
// float sdBox( vec3 p, vec3 b )
// {
//   vec3 q = abs(p) - b;
//   return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
// }

/** From IQ's tutorial, but converted to one-liner.*/
float i_sdVerticalCapsule( vec3 p, float h, float r )
{
  //p.y -= clamp( p.y, 0.0, h );
  return length( p - vec3(0,clamp(p.y,0,h),0) ) - r;
}


/** The union operator for geometries: just pick closest.*/
float i_opUnion( float d1, float d2){
    return min(d1,d2);
}


/** Infinite repetition, as "transformation of traversal point, tp" */
vec3 i_tpRep( in vec3 p, in vec3 c)
{
    return mod(p+0.5*c,c)-0.5*c;
}

float sdf(vec3 p){
    float iTime = u.x/1000.;
    float s = sin(p.z/80*sin(iTime/6)), c = cos(p.z/80*sin(iTime/5)); mat3 rot=mat3(s,c,0,c,-s,0,0,0,1);
    p = rot*p;
    p = i_tpRep(p, vec3(10,12,40));
    return i_opUnion(
        i_sdVerticalCapsule(p, 1.5, 1),
        i_sdSphereAt(p, vec4(2,0,0,1))
        );
}

/** Numerical normal; again, from IQ's tutorial on the topic. */
vec3 normal_of_sdf(vec3 p)
{
    const float h = 0.001; // replace by an appropriate value
    const vec2 k = vec2(1,-1);
    return normalize( k.xyy*sdf( p + k.xyy*h ) + 
                      k.yyx*sdf( p + k.yyx*h ) + 
                      k.yxy*sdf( p + k.yxy*h ) + 
                      k.xxx*sdf( p + k.xxx*h ) );
}

vec3 rayMarch_experiment(vec2 s, float iTime){
    //return i_testTexture(s);
    const int max_steps = 80;
    float t = 0;
    vec3 Ro = vec3(5+sin(iTime),iTime*sin(iTime),30-iTime);
    vec3 Rd = normalize(vec3(s,-4));
    vec3 loc = Ro;
    int i;
    for(i = 0; i < max_steps; i++){
        float d = sdf(loc);
        if (d<=0.001) break;
        loc += d*Rd*.7;
    }
    vec3 n = normal_of_sdf(loc);
    return n / length(Ro-loc)*100;
}

void main()
{
    float iTime = u.x/1000.;
    vec2 s = i_screenCoord(gl_FragCoord.xy, u.yz);

    //raytrace_plane_experiment(iTime,s);
    //gl_FragColor = vec4(i_testTexture(s),1); return; 
    gl_FragColor = vec4(rayMarch_experiment(s,iTime),1); return;
}
