data D = S D |Z
data W = R D  |E D D

i x  = x

c:: D -> D -> Int
c x (S(S(S(y)))) = 1
c Z w = 0


m =   c Z (i(S(i(S(i(S(i(S Z))))))))
