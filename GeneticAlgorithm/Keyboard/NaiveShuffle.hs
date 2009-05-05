module NaiveShuffle (shuffle) where
extract:: Integer -> [a] -> (a,[a])
-- given a list l, extract the n-th element and return the element and
-- the remaining list. We won't worry about the order of the list
-- of the remaining elements. n>=0. n==0 corresponds to the first element.
extract 0 (h:t) = (h,t)
extract n l = loop n l []
	where
	    loop 0 (h:t) accum = (h,accum ++ t)
	    loop n (h:t) accum = loop (n-1) t (h:accum)

-- given a sequence (e1,...en) to shuffle, and a sequence
-- (r1,...r[n-1]) of numbers such that r[i] is an independent sample
-- from a uniform random distribution [0..n-i], compute the
-- corresponding permutation of the input sequence.

shuffle:: [b] -> [Integer] -> [b]
shuffle [e] [] = [e]
shuffle elements (r:r_others) = let (b,rest) = extract r elements
				in b:(shuffle rest r_others)


