import "prim"

type refraction = #no_refract | #refract vec3

let refract (v: vec3) (n: vec3) (ni_over_nt: f32) : refraction =
  let uv = vec3.normalise v
  let dt = vec3.dot uv n
  let discriminant = 1 - ni_over_nt*ni_over_nt*(1-dt*dt)
  in if discriminant > 0
     then #refract ((ni_over_nt `vec3.scale` (uv vec3.- (dt `vec3.scale` n)))
                    vec3.- (f32.sqrt discriminant `vec3.scale` n))
     else #no_refract

let schlick (cosine: f32) (ref_idx: f32) =
  let r0 = (1-ref_idx) / (1+ref_idx)
  let r0 = r0*r0
  in r0 + (1-r0)*(1-cosine)**5

import "lib/github.com/diku-dk/cpprandom/random"

module rnge = minstd_rand
module dist = uniform_real_distribution f32 rnge
type rng = rnge.rng

let rand : rng -> (rng, f32) = dist.rand (0,1)

let random_in_unit_sphere rng =
  let new rng = let (rng, x) = rand rng
                let (rng, y) = rand rng
                let (rng, z) = rand rng
                in (rng, vec(x,y,z))
  let outside_sphere = vec3.quadrance >-> (>=1)
  in iterate_while ((.2) >-> outside_sphere) ((.1) >-> new) (new rng)

type camera = { origin: vec3
              , lower_left_corner: vec3
              , horizontal: vec3
              , vertical: vec3
              , u: vec3, v: vec3, w: vec3
              , lens_radius: f32
              , time0: f32, time1: f32 }

let camera (lookfrom: vec3) (lookat: vec3) (vup: vec3) (vfov: f32) (aspect: f32)
           (aperture: f32) (focus_dist: f32) (time0: f32) (time1: f32) : camera =
  let theta = vfov * f32.pi / 180
  let half_height = f32.tan (theta / 2)
  let half_width = aspect * half_height
  let origin = lookfrom
  let w = vec3.normalise (lookfrom vec3.- lookat)
  let u = vec3.normalise (vec3.cross vup w)
  let v = vec3.cross w u
  in { lower_left_corner = origin vec3.-
                           (half_width * focus_dist `vec3.scale` u) vec3.-
                           (half_height * focus_dist `vec3.scale` v) vec3.-
                           (focus_dist `vec3.scale` w)
     , horizontal = (2*half_width*focus_dist) `vec3.scale` u
     , vertical = (2*half_height*focus_dist) `vec3.scale` v
     , origin, u, v, w
     , lens_radius = aperture / 2
     , time0, time1 }

let get_ray (c: camera) (s: f32) (t: f32) (rng: rng) : (rng, ray) =
  let {origin, lower_left_corner, horizontal, vertical,
       u, v, w=_, lens_radius, time0, time1} = c
  let (rng, p) = random_in_unit_sphere rng
  let rd = lens_radius `vec3.scale` p
  let offset = vec3.((rd.x `scale` u) + (rd.y `scale` v))
  let (rng, td) = rand rng
  let time = time0 + td * (time1 - time0)
  in (rng,
      { origin = offset vec3.+ c.origin
      , direction = vec3.(lower_left_corner +
                          (s `scale` horizontal) +
                          (t `scale` vertical) -
                          origin -
                          offset)
      , time })

let aabb_hit ({min, max}: aabb) ({origin, direction, time=_}: ray) (tmin: f32) (tmax: f32): bool =
  -- Unrolled loop.
  let iter min' max' origin' dir' tmin tmax =
    let invD = 1 / dir'
    let t0 = (min' - origin') * invD
    let t1 = (max' - origin') * invD
    let (t0, t1) = if invD < 0 then (t1, t0) else (t0, t1)
    let tmin = f32.max t0 tmin
    let tmax = f32.min t1 tmax
    in (tmin, tmax)
  let (tmin, tmax) =
    iter min.x max.x origin.x direction.x tmin tmax
  in if tmax <= tmin then false
     else let (tmin, tmax) =
            iter min.y max.y origin.y direction.y tmin tmax
          in if tmax <= tmin then false
             else let (tmin, tmax) =
                    iter min.z max.z origin.z direction.z tmin tmax
                  in !(tmax <= tmin)

-- Our checkered textures are not as expressive as in the C++
-- implementation (no recursion).
type texture = #constant {color: vec3}
             | #checkered {even: vec3, odd: vec3}
             | #noise {scale: f32}
             | #image

type texture_value = texture -> (u: f32) -> (v: f32) -> (p: vec3) -> vec3

let mk_texture_value [ny][nx] (turb: vec3 -> f32) (img: [ny][nx][3]u8) : texture_value =
  \(t: texture) (u: f32) (v: f32) (p: vec3) : vec3 ->
    match t
    case #constant {color} ->
      color
    case #checkered {even, odd} ->
      let sines = f32.sin(10*p.x) * f32.sin(10*p.y) * f32.sin(10*p.z)
      in if sines < 0 then odd else even
    case #noise {scale} ->
      vec3.scale (0.5 * (1 + f32.sin(scale * p.z + 10 * turb p))) (vec(1,1,1))
    case #image ->
      let i = t32 (u * r32 nx)
      let j = t32 ((1-v) * r32 ny - 0.001)
      let i = i32.min (i32.max i 0) (nx-1)
      let j = i32.min (i32.max j 0) (ny-1)
      let r = unsafe f32.u8 img[j,i,0] / 256
      let g = unsafe f32.u8 img[j,i,1] / 256
      let b = unsafe f32.u8 img[j,i,2] / 256
      in vec(r,g,b)

type material = #lambertian {albedo: texture}
              | #metal {albedo: vec3, fuzz: f32}
              | #dielectric {ref_idx: f32}
              | #diffuse_light {emit: texture}

type hit_info = {t: f32, p: vec3, normal: vec3, material: material,
                 u: f32, v: f32}

type hit = #no_hit | #hit hit_info

type sphere = {center0: vec3, center1: vec3,
               time0: f32, time1: f32,
               radius: f32, material: material}

let sphere_center (s: sphere) (time: f32): vec3 =
  if s.time0 == s.time1
  then s.center0
  else s.center0 vec3.+
       (((time - s.time0) / (s.time1 - s.time0)) `vec3.scale`
               (s.center1 vec3.- s.center0))

let sphere_uv (p: vec3) : {u: f32, v: f32} =
  let phi = f32.atan2 p.z p.x
  let theta = f32.asin p.y
  in {u = 1 - (phi + f32.pi)/(2*f32.pi),
      v = (theta + f32.pi/2) / f32.pi}

let sphere_hit (s: sphere) (r: ray) (t_min: f32) (t_max: f32) : hit =
  let oc = vec3.(r.origin - sphere_center s r.time)
  let a = vec3.dot r.direction r.direction
  let b = vec3.dot oc r.direction
  let c = vec3.dot oc oc - s.radius*s.radius
  let discriminant = b*b - a*c
  let try_hit (temp: f32) =
    if temp < t_max && temp > t_min
    then let p = point_at_parameter r temp
         let {u,v} = sphere_uv p
         in (#hit { t = temp
                  , p = point_at_parameter r temp
                  , normal = (1/s.radius) `vec3.scale` (p vec3.- sphere_center s r.time)
                  , material = s.material,
                  u, v})
    else #no_hit
  in if discriminant <= 0
     then #no_hit
     else match try_hit ((-b - f32.sqrt(b*b-a*c))/a)
          case #hit h -> #hit h
          case #no_hit -> try_hit ((-b + f32.sqrt(b*b-a*c))/a)

let sphere_aabb (s: sphere) (t0: f32) (t1: f32) : aabb =
  let sphere_box center radius =
    let min = center vec3.- vec(radius, radius, radius)
    let max = center vec3.+ vec(radius, radius, radius)
    in {min, max}
  in if s.time0 == s.time1
     then sphere_box s.center0 s.radius
     else let box0 = sphere_box (sphere_center s t0) s.radius
          let box1 = sphere_box (sphere_center s t1) s.radius
          in surrounding_box box0 box1

type xy_rect = {x0: f32, x1: f32, y0: f32, y1: f32, k: f32,
                material: material}

let xy_rect_hit (rect: xy_rect) (r: ray) (t0: f32) (t1: f32) : hit =
  let {x0, x1, y0, y1, k, material} = rect
  let t = (k - r.origin.z) / r.direction.z
  in if t < t0 || t > t1 then #no_hit
     else let x = r.origin.x + t*r.direction.x
          let y = r.origin.y + t*r.direction.y
          in if x < x0 || x > x1 || y < y0 || y > y1 then #no_hit
             else #hit { u = (x-x0)/(x1-x0),
                         v = (y-y0)/(y1-y0),
                         p = point_at_parameter r t,
                         normal = vec(0, 0, 1),
                         t, material}

let xy_rect_aabb ({x0, x1, y0, y1, k, material=_}: xy_rect) : aabb =
  { min = vec(x0, y0, k-0.0001), max = vec(x1, y1, k+0.0001) }

type obj = #sphere sphere
         | #xy_rect xy_rect

let obj_hit (obj: obj) (r: ray) (t0: f32) (t1: f32) : hit =
  match obj
  case #sphere s -> sphere_hit s r t0 t1
  case #xy_rect rect -> xy_rect_hit rect r t0 t1

type bounded = #unbounded | #bounded aabb
let obj_aabb (t0: f32) (t1: f32) (obj: obj) : aabb =
  match obj
  case #sphere s -> sphere_aabb s t0 t1
  case #xy_rect rect -> xy_rect_aabb rect

import "bvh"

type bvh [n] = bvh [n] obj

let bvh_hit [n] (bvh: bvh [n]) (r: ray) (t_min: f32) (t_max: f32) : hit =
  let contains aabb = aabb_hit aabb r t_min t_max
  let closest_hit (hit, t_max) obj =
    match obj_hit obj r t_min t_max
    case #no_hit -> (hit, t_max)
    case #hit h -> (#hit h, h.t)
  in bvh_fold contains closest_hit (#no_hit, t_max) bvh
     |> (.1)

type scatter = #scatter {attenuation: vec3, scattered: ray}
             | #no_scatter

let scattering (texture_value: texture_value)
               (r: ray) (h: hit_info) (rng: rng) : (rng, scatter) =
  match h.material

  case #lambertian {albedo} ->
    let (rng, bounce) = random_in_unit_sphere rng
    let target = vec3.(h.p + h.normal + bounce)
    in (rng, #scatter {attenuation=texture_value albedo h.u h.v h.p,
                       scattered={origin = h.p,
                                  direction = target vec3.- h.p,
                                  time = r.time}})

  case #metal {albedo, fuzz} ->
    let reflected = reflect (vec3.normalise r.direction) h.normal
    let (rng, bounce) = random_in_unit_sphere rng
    let scattered = {origin = h.p,
                     direction = reflected vec3.+ (fuzz `vec3.scale` bounce),
                     time = r.time}
    in if vec3.dot scattered.direction h.normal > 0
       then (rng, #scatter {attenuation=albedo,
                            scattered})
       else (rng, #no_scatter)

  case #dielectric {ref_idx} ->
    let reflected = reflect r.direction h.normal
    let attenuation = vec(1, 1, 1)
    let (outward_normal, ni_over_nt, cosine) =
      if vec3.dot r.direction h.normal > 0
      then (vec3.map f32.negate h.normal,
            ref_idx,
            ref_idx * vec3.dot r.direction h.normal / vec3.norm r.direction)
      else (h.normal,
            1/ref_idx,
            -vec3.dot r.direction h.normal / vec3.norm r.direction)
    in (match refract r.direction outward_normal ni_over_nt
        case #refract refracted ->
          let reflect_prob = schlick cosine ref_idx
          let (rng, x) = rand rng
          let direction = if x < reflect_prob then reflected else refracted
          in (rng, #scatter {attenuation, scattered={origin=h.p,
                                                     direction,
                                                     time=r.time}})
        case #no_refract ->
          (rng, #scatter {attenuation, scattered={origin=h.p,
                                                  direction=reflected,
                                                  time=r.time}}))

  case #diffuse_light _ ->
    (rng, #no_scatter)

let emitted (texture_value: texture_value)
            (m: material) (u: f32) (v: f32) (p: vec3) : vec3 =
  match m
  case #diffuse_light {emit} ->
    texture_value emit u v p
  case _ ->
    vec(0, 0, 0)

type scene [n] = {textures: texture_value,
                  bvh: bvh [n]}

let color (max_depth: i32) ({textures, bvh}: scene [])
          (r: ray) (rng: rng) : (rng, vec3) =
  let ((rng, _), (_, _, color)) =
    loop
      ((rng, r), (depth, light, color)) =
      ((rng, r), (0, vec(1,1,1), vec(0,0,0))) while depth < max_depth do
      match bvh_hit bvh r 0.00001 f32.highest
      case #hit h ->
        (let emitted = emitted textures h.material h.u h.v h.p
         in match scattering textures r h rng
            case (rng, #scatter {attenuation, scattered}) ->
              ((rng, scattered),
               (depth+1,
                light vec3.* attenuation,
                light vec3.* emitted vec3.+ color))
            case (rng, #no_scatter) ->
              ((rng, r),
               (max_depth,
                light,
                light vec3.* emitted vec3.+ color)))
      case #no_hit ->
        ((rng, r),
         (max_depth,
          light,
          color))
  in (rng, color)

let random_object_at (a: f32) (b: f32) (rng: rng) : (rng, obj) =
  let (rng, center0) = let (rng, xd) = rand rng
                      let (rng, yd) = rand rng
                      in (rng, vec(a+0.9*xd, 0.2, b+0.9*yd))
  let (rng, center1) = let (rng, yd) = rand rng
                       in (rng, center0 with y = center0.y + 0.5 * yd)
  let randp rng = let (rng, x) = rand rng
                  let (rng, y) = rand rng
                  in (rng, x * y)
  let (rng, choose_mat) = rand rng
  let (rng, material) =
    if choose_mat > 0.95 then
      (rng, #dielectric {ref_idx=1.5})
    else
    let (rng, x) = randp rng
    let (rng, y) = randp rng
    let (rng, z) = randp rng
    let albedo = vec(x,y,z)
    let (rng, fuzz) = rand rng
    in if choose_mat > 0.8
       then (rng, #metal {albedo, fuzz})
       else (rng, #lambertian {albedo=#constant {color=albedo}})
  in (rng,
      #sphere {center0, center1,
               time0 = 0, time1 = 1,
               radius=0.2, material})

let random_world (seed: i32) (n: i32) =
  let mk_obj a b = let rng = rnge.rng_from_seed [seed, a ^ b]
                   in random_object_at (r32 a) (r32 b) rng
  let (rngs, objs) = tabulate_2d (n*2+1) (n*2+1) (\a b -> mk_obj (a-n) (b-n))
                     |> map unzip |> unzip
  let rng = rnge.join_rng (flatten rngs)

  let sphere {center, radius, material} : obj =
    #sphere { center0 = center, center1 = center,
              time0 = 0, time1 = 0,
              radius, material }

  let fixed_objs =
    [ sphere {center=vec(0,-1000,0),
              radius=1000,
              material=#lambertian {albedo=#noise {scale=5}}}
    , sphere {center=vec(0,1,0),
              radius=1,
              material=#dielectric {ref_idx=1.5}}
    , sphere {center=vec(-4,1,0),
              radius=1,
              material=#lambertian {albedo=#image}}
    , sphere {center=vec(4,1,0),
              radius=1,
              material=#metal {albedo=vec(0.6,0.6,0.5), fuzz=0}}
    ]

  let world = flatten objs ++ fixed_objs

  in (rng, world)

let light_world : []obj =
  let sphere {center, radius, material} : obj =
    #sphere { center0 = center, center1 = center,
              time0 = 0, time1 = 0,
              radius, material }
  in [ sphere {center=vec(0,-1000,0),
               radius=1000,
               material=#lambertian {albedo=#noise {scale=5}}},
       sphere {center=vec(0,2,0),
               radius=2,
               material=#lambertian {albedo=#noise {scale=15}}},
       sphere {center=vec(-2,2,0),
               radius=2,
               material=#metal {albedo=vec(0.6,0.6,0.5), fuzz=0}},
       sphere {center=vec(0,7,0),
               radius=3,
               material=#diffuse_light {emit=#constant {color=vec(4,4,4)}}},
       #xy_rect { x0=3, x1=5, y0=2, y1=4, k = -2,
                  material=#diffuse_light {emit=#constant {color=vec(4,4,4)}}}
     ]

import "lib/github.com/athas/matte/colour"

let render (max_depth: i32) (nx: i32) (ny: i32) (ns: i32) (scene: scene []) (cam: camera) (rngs: [ny][nx]rng) =
  let start rng =
    (rng, vec(0,0,0))
  let end (_, acc) =
    let col = ((1/r32 ns) `vec3.scale` acc) |> vec3.map f32.sqrt
    in argb.from_rgba col.x col.y col.z 0
  let update (j, i) (rng, acc) =
    let (rng, ud) = rand rng
    let (rng, vd) = rand rng
    let u = (r32(i) + ud) / r32(nx)
    let v = (r32(j) + vd) / r32(ny)
    let (rng, r) = get_ray cam u v rng
    let (rng, col) = color max_depth scene r rng
    in (rng, acc vec3.+ col)
  let space = tabulate_2d ny nx (\j i -> (j,i))
  in rngs
     |> map (map start)
     |> iterate ns (map2 (map2 update) space)
     |> map (map end)
     |> reverse

-- ==
-- compiled input { 800 400 200 }

import "perlin"
module perlin = mk_perlin rnge

let main (nx: i32) (ny: i32) (ns: i32) (nobj: i32) (img: [][][3]u8): [ny][nx]argb.colour =
  let lookfrom = 2 `vec3.scale` vec(13,2,3)
  let lookat = vec(0,2,0)
  let dist_to_focus = 10
  let aperture = 0
  let cam = camera lookfrom lookat (vec(0,1,0)) 20 (r32 nx / r32 ny)
                   aperture dist_to_focus 0 1
  let (rng, world) = random_world (nx ^ ny ^ ns) nobj
  let world = light_world
  let bvh = bvh_mk (obj_aabb  cam.time0 cam.time1) world
  let (rng, p) = perlin.mk_perlin rng 256
  let textures = mk_texture_value (perlin.turb p 7) img
  let scene = {bvh, textures}
  let rngs = rnge.split_rng (nx*ny) rng |> unflatten ny nx
  let max_depth = 50
  in render max_depth nx ny ns scene cam rngs
