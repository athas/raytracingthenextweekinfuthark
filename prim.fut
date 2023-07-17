import "lib/github.com/athas/vector/vspace"

module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector

-- A convenient alias so we don't have to indicate the fields all the
-- time.
def vec (x, y, z) : vec3 = {x,y,z}

type ray = {origin: vec3,
            direction: vec3,
            time: f32}

def point_at_parameter (r: ray) (t: f32) =
  vec3.(r.origin + scale t r.direction)

def reflect (v: vec3) (n: vec3) : vec3 =
  v vec3.- (2 * vec3.dot v n `vec3.scale` n)

-- | Axis-aligned bounding box.
type aabb = { min: vec3, max: vec3 }

def surrounding_box (box0: aabb) (box1: aabb) : aabb =
  let small = vec(f32.min box0.min.x box1.min.x,
                  f32.min box0.min.y box1.min.y,
                  f32.min box0.min.z box1.min.z)
  let big = vec(f32.max box0.max.x box1.max.x,
                f32.max box0.max.y box1.max.y,
                f32.max box0.max.z box1.max.z)
  in {min = small, max = big}

def aabb_center ({min, max}: aabb) =
  {x=min.x + (max.x - min.x),
   y=min.y + (max.y - min.y),
   z=min.z + (max.z - min.z)}

def aabb_rot_y (radians: f32) (aabb: aabb) : aabb =
  let sin_theta = f32.sin radians
  let cos_theta = f32.cos radians
  let min = vec(f32.highest, f32.highest, f32.highest)
  let max = vec(f32.lowest, f32.lowest, f32.lowest)
  let (min, max) =
    loop (min, max) for i < 2 do
      loop (min, max) for j < 2 do
        loop (min, max) for k < 2 do
          let x = r32 i * aabb.max.x + r32 (1-i) * aabb.min.x
          let y = r32 j * aabb.max.y + r32 (1-j) * aabb.min.y
          let z = r32 k * aabb.max.z + r32 (1-k) * aabb.min.z
          let newx = cos_theta*x + sin_theta*z
          let newz = (-sin_theta)*x + cos_theta*z
          let tester = vec(newx, y, newz)
          in ({x = f32.min tester.x min.x,
               y = f32.min tester.y min.y,
               z = f32.min tester.z min.z},
              {x = f32.max tester.x max.x,
               y = f32.max tester.y max.y,
               z = f32.max tester.z max.z})
  in {min, max}
