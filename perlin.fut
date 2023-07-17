-- Perlin noise generator encapsulated as a module.

import "prim"

import "lib/github.com/diku-dk/cpprandom/random"

module type perlin = {
  type rng
  type perlin [n]
  val mk_perlin : (rng: rng) -> (n: i64) -> (rng, perlin [n])
  val noise [n] : perlin [n] -> vec3 -> f32
  val turb [n] : perlin [n] -> (depth: i32) -> vec3 -> f32
}

import "lib/github.com/diku-dk/cpprandom/shuffle"

module mk_perlin (E: rng_engine) : perlin with rng = E.rng = {
  type perlin [n] = { ranvec: [n]vec3
                    , perm_x: [n]i32
                    , perm_y: [n]i32
                    , perm_z: [n]i32
                    }

  type rng = E.rng
  module shuffle = mk_shuffle E
  module dist = uniform_real_distribution f32 E

  def generate (rng: E.rng) (n: i64) =
    rng
    |> E.split_rng n
    |> map (\rng -> let (rng, x) = dist.rand (-1,1) rng
                    let (rng, y) = dist.rand (-1,1) rng
                    let (rng, z) = dist.rand (-1,1) rng
                    in (rng, vec3.normalise {x,y,z}))
    |> unzip
    |> (\(rngs, xs) -> (E.join_rng rngs, xs))

  def generate_perm (rng: E.rng) (n: i64) =
    iota n
    |> map i32.i64
    |> shuffle.shuffle rng

  def mk_perlin (rng: E.rng) (n: i64) =
    let (rng, ranvec) = generate rng n
    let (rng, perm_x) = generate_perm rng n
    let (rng, perm_y) = generate_perm rng n
    let (rng, perm_z) = generate_perm rng n
    in (rng, { ranvec, perm_x, perm_y, perm_z })

  def perlin_interp (c: i32 -> i32 -> i32 -> vec3) (u: f32) (v: f32) (w: f32) =
    let uu = u * u * (3-2*u)
    let vv = v * v * (3-2*v)
    let ww = w * w * (3-2*w)

    in loop accum = 0 for i < 2 do
         loop accum for j < 2 do
           loop accum for k < 2 do
           let weight_v = vec(u-r32 i, v-r32 j, w-r32 k)
           in accum + ((r32 i*uu + r32 (1-i)*(1-uu)) *
                       (r32 j*vv + r32 (1-j)*(1-vv)) *
                       (r32 k*ww + r32 (1-k)*(1-ww)) *
                       vec3.dot (c i j k) weight_v)

  def noise [n] ({ranvec, perm_x, perm_y, perm_z}: perlin [n]) {x, y, z} : f32 =
    let n = i32.i64 n

    let u = x - f32.floor x
    let v = y - f32.floor y
    let w = z - f32.floor z

    let i = t32 (f32.floor x)
    let j = t32 (f32.floor y)
    let k = t32 (f32.floor z)

    let c di dj dk =
      #[unsafe] ranvec[perm_x[(i+di) % n] ^
                       perm_y[(j+dj) % n] ^
                       perm_z[(k+dk) % n]]

    in perlin_interp c u v w

  def turb perlin (depth: i32) p =
    f32.abs <| (.0) <|
    loop (accum, temp_p, weight) = (0, p, 1) for _i < depth do
      (accum + weight * noise perlin temp_p,
       vec3.scale 2 temp_p,
       weight / 2)
}
