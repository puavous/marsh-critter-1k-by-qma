#version 140
uniform ivec4 u;
float iTime = u.x/1000.;  // Yep, name iTime is carried over from shadertoy :).

// ---------------------------------------------------------
// some one-liner functions that current Shader Minifier omits if unused.
// /** Checker-board with 'density' squares on unit length.
//  * For usual texture uv coordinates, [0,1]x[0,1] is a black square.
//  * When uv are normalized screen coordinates, [-1,1]x[-1,1] has 4 squares.
//  * Once again.. Inigo did it properly ;) ... but here's my own crude helper..
//  */
vec3 i_checkerBoardTexture(vec2 uv, float density) {
  return vec3(mod(floor(density*uv.x)+floor(density*uv.y), 2.));
}

/** A test texture: 'tick marks' at 0.1 and 1.0 intervals +colors for xy orientation.*/
vec3 i_testTexture(vec2 uv) {
   return max( 0.2  * i_checkerBoardTexture(uv, 1.0)
               + 0.1 * i_checkerBoardTexture(uv, 10.0),
               vec3(uv.xy,0.0));
}

/* Normalized pixel coordinates (y in [-1,1], x follows aspect ratio).
 */
vec2 i_screenCoord(vec2 pix, vec2 resolution) {
    return (2*pix - resolution) / resolution.y;
}

/** Intersect ray with plane defined by triangle abc as a=a, u=b-a, v=c-a. Return barycentric coordinates and distance to hit as (beta,gamma,t). */
vec3 i_rtMiniPlane2(vec3 Ro, vec3 Rd, vec3 a, vec3 u, vec3 v) {
  return inverse(mat3(-u,-v,Rd))*(a-Ro);
}


/* Let's make it clear that, once again, I'm using Inigo's treasure
 * trove of tutorials and examples! */

/** Sphere s with center and radius: (x, y, z, radius) */
float i_sdSphereAt( vec3 p, vec4 s ) {
  return length(p-s.xyz)-s.w;
}

/** Torus. One-lined the implementation here.*/
float i_sdTorus( vec3 p, vec2 t ) {
  return length(vec2(length(p.xz)-t.x,p.y))-t.y;
}

/** Distance from a level "ground", i.e., xz-plane at height p.y=y.*/
float i_sdFlatEarth(vec3 p, float y) {
    return p.y-y;
}

// /** Box; verbatim from Inigo's tutorial at https://iquilezles.org/articles/distfunctions/ */
// float sdBox( vec3 p, vec3 b )
// {
//   vec3 q = abs(p) - b;
//   return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
// }

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

// // Internally inlined, for size comparisons..
// float smin( float a, float b, float k )
// {
//     return -log2( exp2( -k*a ) + exp2( -k*b ) )/k;
// }

// // Verbatim version from IQ's tutorial, to debug inlining issues...
// float smin( float a, float b, float k )
// {
//     float res = exp2( -k*a ) + exp2( -k*b );
//     return -log2( res )/k;
// }

// Version that uses e instead of 2.. maybe not correct, but minimal..
float i_smine( float a, float b, float k )
{
    return -log( exp( -k*a ) + exp( -k*b ) )/k;
}

// basic intersect
float i_intersect( float a, float b)
{
    return max(a,b);
}

/** Infinite repetition, as "transformation of traversal point, tp" */
vec3 i_tpRep( in vec3 p, in vec3 c)
{
    return mod(p+0.5*c,c)-0.5*c;
}

// ---------------------------------------------------

// Probably sticking with spheres this time, if I can fit 'en in the 1k..
float sdf(vec3 p){
    //float d = 1e9; // Start agglomerating from "infinity".
    float d = i_sdSphereAt(p, vec4(0,0,0,2));
    //d = i_smine(d, i_sdSphereAt(p, vec4(0,0,0,2)), 3);
    //d = i_smine(d, i_sdTorus(p, vec2(1,.2)), 3);
    for (int i=0;i<6;i++){
        d = i_smine(d, i_sdSphereAt(p, vec4(iTime/3*sin(i+iTime),sin(iTime+iTime*i),iTime/3*cos(i+iTime),1)), 4);
    }
    d = i_smine(d, i_sdFlatEarth(p, 0-iTime/10), 6);
    //d = i_opUnion(d, i_sdFlatEarth(p, 0-iTime/10));
    //float i_a = i_sdSphereAt(p, vec4(0,0,0,2));
    // float i_b = i_sdSphereAt(p, vec4(3+2*sin(iTime),0,0,1));
    return d;
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

/** Numerical normal; again, from IQ's tutorial on the topic.
 * This _might_ compress best? At cost of +2 function evals.
 */
vec3 normal_of_sdf(vec3 p)
{
    const float eps = 0.001; // or some other value
    vec2 h = vec2(eps,0);
    return normalize( vec3(sdf(p+h.xyy) - sdf(p-h.xyy),
                           sdf(p+h.yxy) - sdf(p-h.yxy),
                           sdf(p+h.yyx) - sdf(p-h.yyx) ) );
}

const float max_t = 500;
float march_sdf(vec3 Ro, vec3 Rd){
    const int max_steps = 200;
    float t = 0;

    // Actual march from here to there.
    int i;
    for(i = 0; i < max_steps; i++){
        float d = sdf(Ro+t*Rd);
        if (d<=0.001*t || t > max_t) break;
        t += .95*d;
    }
    return t;
}

vec3 rayMarch_experiment(vec2 s){
    // Approach from positive z. orient screen as xy-plane:    
    vec3 Ro = vec3(0,1.2,17-iTime/10);
    vec3 Rd = normalize(vec3(s,-4));

    float t = march_sdf(Ro, Rd);
    vec3 loc = Ro+t*Rd;
    vec3 n = normal_of_sdf(loc);

    // Try some reflection.. a lot of computation going on; slow..
    vec3 rdir = reflect(Rd,n);
    float t2 = march_sdf(loc +0.001*rdir, rdir);
    vec3 loc2 = loc + t2*rdir;
    vec3 n2 = normal_of_sdf(loc2);

    vec3 c =  vec3(max(0,dot(n, normalize(vec3(1,1,-1))))); 
    vec3 c2 = vec3(max(0,dot(n2, normalize(vec3(1,1,-1)))));
    c = c+.5*c2;

    return (max_t-t)/max_t * c;
//    return (max_t-t)/max_t * vec3(max(0,dot(n, normalize(vec3(1,1,3)))));
}

void main()
{
    vec2 s = i_screenCoord(gl_FragCoord.xy, u.yz);
    gl_FragColor = vec4(rayMarch_experiment(s),1);
}
