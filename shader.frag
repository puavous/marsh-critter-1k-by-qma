#version 140

/* Let's make it clear that, once again, I'm using Inigo's treasure
 * trove of tutorials and examples! */

// -----------------------------------i----------------------
// some one-liner functions that current Shader Minifier omits if unused.

/** Checker-board.. Once again.. Inigo did it properly ;) ... but here's my own crude helper.. */
vec3 i_checkerBoardTexture(vec2 uv, float density) {
  return vec3(mod(floor(density*uv.x)+floor(density*uv.y), 2.));
}

/** A test texture: 'tick marks' at 0.1 and 1.0 intervals +colors for xy orientation.*/
vec3 i_testTexture(vec2 uv) {
   return max( 0.2  * i_checkerBoardTexture(uv, 1.0)
               + 0.1 * i_checkerBoardTexture(uv, 10.0),
               vec3(uv.xy,0.0));
}

/** Normalized pixel coordinates (y in [-1,1], x follows aspect ratio). */
vec2 i_screenCoord(vec2 pix, vec2 resolution) {
    return (2*pix - resolution) / resolution.y;
}

/** Intersect ray with plane defined by triangle abc as a=a, u=b-a, v=c-a. Return barycentric coordinates and distance to hit as (beta,gamma,t). */
vec3 i_rtMiniPlane2(vec3 Ro, vec3 Rd, vec3 a, vec3 u, vec3 v) {
  return inverse(mat3(-u,-v,Rd))*(a-Ro);
}

/** Sphere s with center and radius: (x, y, z, radius) */
float i_sdSphereAt( vec3 p, vec4 s ) {
  return length(p-s.xyz)-s.w;
}

/** Sphere at origin; radius r */
float i_sdSphere( vec3 p, float r ) {
  return length(p)-r;
}

/** Torus. One-lined the implementation here.*/
float i_sdTorus( vec3 p, vec2 t ) {
  return length(vec2(length(p.xz)-t.x,p.y))-t.y;
}

/** Distance from a level "ground", i.e., xz-plane at height p.y=y.*/
float i_sdFlatEarth(vec3 p, float y) {
    return p.y-y;
}

/** From IQ's tutorial, but converted to one-liner.*/
float i_sdVerticalCapsule( vec3 p, float h, float r ) {
  return length( p - vec3(0,clamp(p.y,0,h),0) ) - r;
}

/** The union operator for geometries: just pick closest.*/
float i_opUnion( float d1, float d2) {
    return min(d1,d2);
}

// From IQ's treasures.. exponential smooth min (k=32) .. one-lined here.
float i_smin( float a, float b, float k ) {
    return -log2( exp2( -k*a ) + exp2( -k*b ) )/k;
}

// Version that uses e instead of 2.. maybe not correct, but minimal..
float i_smine( float a, float b, float k ) {
    return -log( exp( -k*a ) + exp( -k*b ) )/k;
}

// basic intersect
float i_intersect( float a, float b) {
    return max(a,b);
}

/** Infinite repetition, as "transformation of traversal point, tp" */
vec3 i_tpRep( in vec3 p, in vec3 c) {
    return mod(p+0.5*c,c)-0.5*c;
}

// /** Numerical normal; again, from IQ's tutorial on the topic. */
// vec3 normal_of_sdf(vec3 p)
// {
//     const float h = 0.001; // replace by an appropriate value
//     const vec2 k = vec2(1,-1);
//     return normalize( k.xyy*sdf( p + k.xyy*h ) + 
//                       k.yyx*sdf( p + k.yyx*h ) + 
//                       k.yxy*sdf( p + k.yxy*h ) + 
//                       k.xxx*sdf( p + k.xxx*h ) );
// }



// ---------------------------------------------------
// Done with one-liners. Following will always end up as code after minification.
const float eps = 0.001; // or some other value.. epsilon

// Idunno.. can we have some more definition stuff here? Should we? No idea.. just want an entry here and now 2022 asm...
// Go with this idea now:

uniform ivec3 u;

float iTime = u.x/1000.;  // Yep, name iTime is carried over from shadertoy :).

float extent = 1+iTime/9;

vec3 critter_position = vec3(sin(iTime), sin(iTime)+5*sin(iTime/7)-5, 2*iTime-40);

// Set up a light..
//vec3 light_dir = normalize(vec3(1-iTime/10,1,-1));
//vec3 light_pos = vec3(1-iTime/10,1,-1);
vec3 light_pos = vec3(6,6,0);

//vec2 h = vec2(eps,0);
vec3 h = vec3(eps,0,0);


//float fade = smoothstep(0,4,iTime) - smoothstep(20,24,iTime);
float fade = 1-smoothstep(20,24,iTime);


// Probably sticking with spheres this time, if I can fit 'em in the 1k..
float sdf(vec3 p) {
//    float d = i_sdFlatEarth(p, -2);
    float d = p.y+2+sin(p.z-iTime)/9;
    p -= critter_position;
    //d = i_smine(d, i_sdSphere(p - vec3(0,level,0), 2), 4);
    d = i_smine(d, i_sdSphere(p, 2), 4);
    for (int i=0;i++<6;){
        d = i_smine(d, i_sdSphere(p - vec3(extent*sin(i+iTime),sin(iTime+iTime*i),extent*cos(i+iTime)),1), 4);
    }

    //d = i_smine(d, i_sdFlatEarth(p, 0-iTime/7), 4);
    return d;
}

/** Numerical normal; again, from IQ's tutorial on the topic.
 * This _might_ compress best? At cost of +2 function evals.
 */
vec3 i_normal_of_sdf(vec3 p)
{
    return normalize( vec3(sdf(p+h.xyy) - sdf(p-h.xyy),
                           sdf(p+h.yxy) - sdf(p-h.yxy),
                           sdf(p+h.yyx) - sdf(p-h.yyx) ) );
}

const float max_t = 300;
float march_sdf(vec3 Ro, vec3 Rd){
    const int max_steps = 300;
    float t = 0;

    // Actual march from here to there.
//    for(int i = 0; (i++ < max_steps) && (t < max_t);){
    for(int i = 0; (i++ < max_steps);){
        float d = sdf(Ro+t*Rd);
        if (d<h.x*t) break;
        t += d;
    }
    return t;
}

void main()
{
    // Build the main logic here, inside main().
    vec2 i_s = (2*gl_FragCoord.xy-u.yz)/u.z;

    // Approach from positive z. orient screen as xy-plane:
//    vec3 Ro = vec3(0,1,14-iTime/9);
    vec3 Ro = vec3(0,0,9);
    vec3 Rd = normalize(vec3(i_s,-3));

    float t = march_sdf(Ro, Rd);
    vec3 loc = Ro + t*Rd;
    vec3 n = i_normal_of_sdf(loc);

    // Just color by diffuse component:
    vec3 light_dir = light_pos - loc;
    float diff = max(0,dot(n, normalize(light_dir)));
    vec3 c = vec3(.1,diff,.3);
    //vec3 c = vec3(diff);

    // // Try some reflection.. a lot of computation going on; slow..
    // vec3 rdir = reflect(Rd,n);
    // float t2 = march_sdf(loc +0.001*rdir, rdir);
    // vec3 loc2 = loc + t2*rdir;
    // vec3 n2 = normal_of_sdf(loc2);

    // float shadow = soft_shadow(loc, light_dir, .001, 100, 4);
    // float shadow2 = soft_shadow(loc2, light_dir, .001, 100, 4);

    // vec3 c =  shadow * vec3(max(0,dot(n, light_dir))); 
    // vec3 c2 = shadow2 * vec3(max(0,dot(n2, light_dir)));

    // c = c+.5*c2;

    // c = (max_t-t)/max_t * c;

    gl_FragColor = fade * vec4(c,1);
}


// Ok.. One more experiment from tutorial.. gotta try some soft shadows.
// From https://iquilezles.org/articles/rmshadows/ obviously..
// float sharp_shadow( in vec3 ro, in vec3 rd, float mint, float maxt )
// {
//     for( float t=mint; t<maxt; )
//     {
//         float h = sdf(ro + rd*t);
//         if( h<0.001 )
//             return 0.0;
//         t += h;
//     }
//     return 1.0;
// }

// And then the soft shadow "for free".
// Still from https://iquilezles.org/articles/rmshadows/ obviously..
// float soft_shadow( in vec3 ro, in vec3 rd, float mint, float maxt, float k )
// {
//     float res = 1.0;
//     for( float t=mint; t<maxt; )
//     {
//         float h = sdf(ro + rd*t);
//         if( h<0.001 )
//             return 0.0;
//         res = min( res, k*h/t );
//         t += h;
//     }
//     return res;
// }
