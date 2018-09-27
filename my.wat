(module
  (type (func (param i32) (result i32)))
  (func (param i32) (result i32)
    (get_local 0)
    (i32.const 1)
    (i32.add)
  )
)