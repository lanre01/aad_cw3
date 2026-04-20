module STree where 



{-
main algorithm 
create states root and \bottom
create a suffix link f'(root) = \bottom
s = root, k = 1, i = 0;
while t_i+1 /= #
    i = i + 1
    (s, k) = update(s, (k, i))
    (s, k) = canonize(s, (k, i))


update(s, (k , i)):
    (s, (k, i - 1)) is the canonical reference pair for the active point
    oldr = root; (end-point, r) = test_and_split(s, (k, i-1), t_i)
    while not (end-point) do :
        create new transition g'(r, (i, inf)) = r' where r' is a new state
        if oldr \= root then create a new suffix link f'(oldr) = r
        oldr = r 
        (s, k) = canonize(f'(s), (k, i-1))
        (end-point, r) = test_and_split(s, (k, i-1), t_i)
    
    if oldr \= root then create new suffix link f'(oldr) = s
    return (s, k)
    
canonize(s, (k,p))
    if p < k then return (s, k)

    else
            find the t_k transition g'(s, (k', p')) = s' from s
            while p' - k' <= p - k do 
                k = k + p' - k' + 1
                s = s'
                if k <= p then find the t_k transition g'(s, (k', p')) = s' from s 
            return (s, k)

test_and_split (s, (k,p), t)
    if k <= p then 
        let g'(s, k' p') = s' be the t_k transition from s
        if t = t_{k' + p - k + 1} then return (true, s)
        else 
            replace the t_k transition above by transitions 
                g'(s, (k', k' + p - k)) = r and g'(r, (k' + p - k + 1, p')) = s'
                where r is the new state
                return (false, r)
    else
        if there is no t transition from s then return (false, s)
        else return (true, s)
-}