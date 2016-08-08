(*
  the monoidal machine

  We describe an abstract machine that potentially can be implement in
  a performant way in hardware.  Begin by considering a certain
  restricted type of monoidal category. Assume we are given a
  certain number of primitive operations

  op = { o_1, ..., o_k }

  Each of which can have inputs and outputs of colors c_1 ... c_m. We
  consider subsets of operations s_1,...,s_n and a finite lattice of
  intersections and unions of those.

  A state of the monoidal machine is a tensor product

  r0 ⊗ r1 ⊗ ... ⊗ rn

 *)
