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

module rnge = pcg32
module dist = uniform_real_distribution f32 rnge
type rng = rnge.rng

let rand : rng -> (rng, f32) = dist.rand (0,1)

let random_in_unit_sphere rng =
  let new rng = let (rng, x) = dist.rand (-1, 1) rng
                let (rng, y) = dist.rand (-1, 1) rng
                let (rng, z) = dist.rand (-1, 1) rng
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

type transform = {flip: #flip | #noflip,
                  translate: vec3}

let transform_id : transform =
  {flip = #noflip, translate = vec(0, 0, 0)}

let transform_hit (t: transform) (h: hit) : hit =
  match h
  case #hit h ->
    #hit (h with normal = (match t.flip
                           case #flip -> (-1) `vec3.scale` h.normal
                           case #noflip -> h.normal)
            with p = h.p vec3.+ t.translate)
  case _ ->
    h

let transform_ray (t: transform) (r: ray) : ray =
  r with origin = r.origin vec3.- t.translate

type sphere = {center1: vec3,
               time0: f32, time1: f32,
               radius: f32, material: material}

let sphere_center (s: sphere) (time: f32): vec3 =
  if s.time0 == s.time1
  then vec(0,0,0)
  else ((time - s.time0) / (s.time1 - s.time0)) `vec3.scale` s.center1

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
     then sphere_box (vec(0,0,0)) s.radius
     else let box0 = sphere_box (sphere_center s t0) s.radius
          let box1 = sphere_box (sphere_center s t1) s.radius
          in surrounding_box box0 box1

type rect_type = #xy | #xz | #yz

type rect = {rtype: rect_type,
             a1: f32,
             b1: f32,
             k: f32,
             material: material}

let rect_abc (rtype: rect_type) (v: vec3) : {a: f32, b: f32, c: f32} =
  match rtype case #xy -> {a = v.x, b=v.y, c=v.z}
              case #xz -> {a = v.x, b=v.z, c=v.y}
              case #yz -> {a = v.y, b=v.z, c=v.x}

let rect_cba (rtype: rect_type) {a: f32, b: f32, c: f32} : vec3 =
  match rtype case #xy -> {x=a, y=b, z=c}
              case #xz -> {x=a, y=c, z=b}
              case #yz -> {x=c, y=a, z=b}

let rect_aabb (rect: rect) : aabb =
  let {rtype, a1, b1, k, material=_} = rect
  in { min = rect_cba rtype {a=0, b=0, c=k-0.0001},
       max = rect_cba rtype {a=a1, b=b1, c=k+0.0001} }

let rect_hit (rect: rect) (r: ray) (t0: f32) (t1: f32) : hit =
  let {rtype, a1, b1, k, material} = rect
  let origin = rect_abc rtype r.origin
  let direction = rect_abc rtype r.direction
  let t = (k - origin.c) / direction.c
  in if t < t0 || t > t1 then #no_hit
     else let a = origin.a + t*direction.a
          let b = origin.b + t*direction.b
          in if a < 0 || a > a1 || b < 0 || b > b1 then #no_hit
             else #hit { u = a/a1,
                         v = b/b1,
                         p = point_at_parameter r t,
                         normal = rect_cba rtype {a=0, b=0, c=1},
                         t, material}

type obj = {transform: transform,
            obj: #sphere sphere | #rect rect}

let obj_mk x : obj = {obj = x, transform = transform_id }

let obj_flip (obj: obj) = obj with transform.flip = #flip

let obj_translate (v: vec3) (obj: obj) : obj =
  obj with transform.translate = obj.transform.translate vec3.+ v

let obj_hit (obj: obj) (r: ray) (t0: f32) (t1: f32) : hit =
  let r = transform_ray obj.transform r
  let h = match obj.obj
          case #sphere s -> sphere_hit s r t0 t1
          case #rect rect -> rect_hit rect r t0 t1
  in transform_hit obj.transform h

type bounded = #unbounded | #bounded aabb
let obj_aabb (t0: f32) (t1: f32) (obj: obj) : aabb =
  let {min, max} = match obj.obj
                   case #sphere s -> sphere_aabb s t0 t1
                   case #rect rect -> rect_aabb rect
  in {min = min vec3.+ obj.transform.translate,
      max = max vec3.+ obj.transform.translate}

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
      match bvh_hit bvh r 0.001 f32.highest
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

let xy_rect {x, y, k, material} =
  obj_mk (#rect {rtype=#xy, a1=x, b1=y, k, material})

let xz_rect {x, z, k, material} =
  obj_mk (#rect {rtype=#xz, a1=x, b1=z, k, material})

let yz_rect {y, z, k, material} =
  obj_mk (#rect {rtype=#yz, a1=y, b1=z, k, material})

let box {p={x, y, z}, material} =
  let a k = xy_rect {x, y, k, material}
  let b k = xz_rect {x, z, k, material}
  let c k = yz_rect {y, z, k, material}
  in [a z,
      obj_flip (a 0),
      b y,
      obj_flip (b 0),
      c x,
      obj_flip (c 0)]

let cornell_box : []obj =
  let red = #lambertian {albedo=#constant {color=vec(0.65, 0.05, 0.05)}}
  let white = #lambertian {albedo=#constant {color=vec(0.73, 0.73, 0.73)}}
  let green = #lambertian {albedo=#constant {color=vec(0.12, 0.45, 0.15)}}
  let light = #diffuse_light {emit=#constant {color=vec(15, 15, 15)}}
  in [ obj_flip (yz_rect {y=555, z=555, k=555, material=green})
     , yz_rect {y=555, z=555, k=0, material=red}
     , obj_translate (vec(213, 0, 227))
                     (xz_rect {x=130, z=105, k=554, material=light})
     , obj_flip (xz_rect {x=555, z=555, k=555, material=white})
     , xz_rect {x=555, z=555, k=0, material=white}
     , obj_flip (xy_rect {x=555, y=555, k=555, material=white})
     ]
     ++ map (obj_translate (vec(130, 0, 65)))
            (box {p=vec(165, 165, 165), material=white})
     ++ map (obj_translate (vec(265, 0, 295)))
            (box {p=vec(165, 330, 165), material=white})

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

let main (nx: i32) (ny: i32) (ns: i32) (img: [][][3]u8): [ny][nx]argb.colour =
  let lookfrom = vec(278,278,-800)
  let lookat = vec(278,278,0)
  let dist_to_focus = 10
  let aperture = 0
  let vfov = 40
  let cam = camera lookfrom lookat (vec(0,1,0)) vfov (r32 nx / r32 ny)
                   aperture dist_to_focus 0 1
  let rng = rnge.rng_from_seed [nx, ny, ns]
  let world = cornell_box
  let bvh = bvh_mk (obj_aabb  cam.time0 cam.time1) world
  let (rng, p) = perlin.mk_perlin rng 256
  let textures = mk_texture_value (perlin.turb p 7) img
  let scene = {bvh, textures}
  let rngs = rnge.split_rng (nx*ny) rng |> unflatten ny nx
  let max_depth = 50
  in render max_depth nx ny ns scene cam rngs
