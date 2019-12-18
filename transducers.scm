(module
  transducers
  (reduced-value
   reduced
   reduced?
   extract-reduced
   ensure-reduced
   unreduced

   unit
   map
   concatenate
   append-map
   filter
   filter-map
   remove
   delete
   take
   take-while
   take-nth
   drop
   drop-while
   partition-every
   intersperse
   delete-duplicates
   random-sample

   build/list
   build/vector

   transduce/list
   transduce/string
   transduce/vector
   transduce/u8vector
   transduce/s8vector
   transduce/u16vector
   transduce/s16vector
   transduce/u32vector
   transduce/s32vector
   transduce/u64vector
   transduce/s64vector
   transduce/f32vector
   transduce/f64vector
   transduce/generator)
  (import
    (except scheme map)
    (except (chicken base) intersperse)
    (chicken random)
    srfi-4)
  (include "transducers.impl.scm"))
