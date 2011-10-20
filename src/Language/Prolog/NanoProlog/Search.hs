module Search where

data Tree a =  Node a [Tree a]
            |  Success
            deriving Show

bf_label t l = let (res, levels) = traverse t (l:levels)
                   traverse (Node a cs) ((l:ll):ls) = (Node l rcs, ll:rls)
                            where (rcs, rls) = traversel cs ls
                   traversel [] rls = ([], rls)
                   traversel (t:ts) ls = let (tr, ls')  = traverse  t  ls
                                             (trs,ls'') = traversel ts ls'
                                         in (tr:trs, ls'')
               in  res

bf_enum t = let (l:ls) = traverse t ls
                traverse (Node a cs) ~(l:ls) = (a:l):rls
                            where rls = traversel cs ls
                traversel []     ls =  ls
                traversel (t:ts) ls =  traverse t (traversel ts ls)
             in l

t = Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 []]


