
expr "42" = [([Num 42],"")]
expr "!42" = [([Op1 !,Num 42],"")]
expr "+42 17" = [([Op2 +,Num 42,Num 17],"")]
expr "+!42 17" [([Op2 +,Op1 !,Num 42,Num 17],"")]
expr "+42 !17" = [] ???

expr "+42 !17" =  
(altp 
    (token num)
    (altp 
        (seqp 
            (token binary)
            (seqp 
                expr
                expr
            ) 
        )
        (seqp
            (token unary)
            expr
        )
    )
) "+42 !17"

\s -> case ((token num) s) of
    [(as,s')] -> [(as,s']
    [] -> case (altp 
                    (seqp 
                        (token binary)
                        (seqp 
                            expr
                            expr
                        ) 
                    )
                    (seqp
                        (token unary)
                        expr
                    )
                ) s of 
                [(bs,s')] -> [(bs,s')]
                [] -> []

case ((token num) "+42 !17") of
    [(as,s')] -> [(as,s']
    [] -> case (altp 
                    (seqp 
                        (token binary)
                        (seqp 
                            expr
                            expr
                        ) 
                    )
                    (seqp
                        (token unary)
                        expr
                    )
                ) "+42 !17" of 
                [(bs,s')] -> [(bs,s')]
                [] -> []

    case (altp 
             (seqp 
                (token binary)
                (seqp 
                    expr
                    expr
                ) 
             )
            (seqp
                (token unary)
                expr
            )
        ) "+42 !17" of 
    [(bs,s')] -> [(bs,s')]
    [] -> []

    case (\s -> case (seqp 
                        (token binary)
                        (seqp 
                            expr
                            expr
                        ) 
                     ) "+42 !17" of
                        [(as,s')] -> [(as,s')]
                        [] -> case  (seqp
                                        (token unary)
                                        expr
                                ) "+42 !17" of 
                                        [(bs,s')] -> [(bs,s')]
                                        [] -> []
    case (\s -> case (seqp 
                        (token binary)
                        (seqp 
                            expr
                            expr
                        ) 
                     ) "+42 !17" of
                        [(as,s')] -> [(as,s')]
                        [] -> case  (seqp
                                        (token unary)
                                        expr
                                ) "+42 !17" of 
                                        [(bs,s')] -> [(bs,s')]
                                        [] -> []
