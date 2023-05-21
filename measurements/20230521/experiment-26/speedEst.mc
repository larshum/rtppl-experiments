mexpr
let compileOptions =
  { printSamples =
      true,
    seedIsSome =
      false,
    seed =
      0,
    resample =
      "manual",
    cps =
      "partial",
    earlyStop =
      true,
    mcmcLightweightGlobalProb =
      0.1,
    mcmcLightweightReuseLocal =
      true,
    printAcceptanceRate =
      false }
in
type EmpiricalExtra
in
type Dist a0
in
let not: Bool -> Bool =
  lam a123.
    match
      a123
    with
      true
    then
      false
    else
      true
in
let and: Bool -> Bool -> Bool =
  lam a122.
    lam b50.
      match
        a122
      with
        true
      then
        b50
      else
        false
in
let or: Bool -> Bool -> Bool =
  lam a121.
    lam b49.
      match
        a121
      with
        true
      then
        true
      else
        b49
in
let xor: Bool -> Bool -> Bool =
  lam a120.
    lam b48.
      match
        a120
      with
        true
      then
        not
          b48
      else
        b48
in
let xnor: Bool -> Bool -> Bool =
  lam a119.
    lam b47.
      not
        (xor
           a119
           b47)
in
let eqBool: Bool -> Bool -> Bool =
  lam b112: Bool.
    lam b211: Bool.
      match
        b112
      with
        true
      then
        b211
      else
        match
          b211
        with
          true
        then
          false
        else
          true
in
let cmpBool: Bool -> Bool -> Int =
  lam b111: Bool.
    lam b210: Bool.
      match
        b111
      with
        true
      then
        match
          b210
        with
          true
        then
          0
        else
          1
      else
        match
          b210
        with
          true
        then
          negi
            1
        else
          0
in
let bool2string: Bool -> [Char] =
  lam b46.
    match
      b46
    with
      true
    then
      "true"
    else
      "false"
in
type Option a
in
con Some: all a1. a1 -> Option a1 in
con None: all a2. () -> Option a2 in
let optionEq: all a118. all b45. (a118 -> b45 -> Bool) -> Option a118 -> Option b45 -> Bool =
  lam eq5.
    lam o110.
      lam o26.
        match
          (o110, o26)
        with
          (Some v13, Some v23)
        then
          eq5
            v13
            v23
        else
          match
            (o110, o26)
          with
            (None {}, None {})
          then
            true
          else
            false
in
let optionMap: all a117. all b44. (a117 -> b44) -> Option a117 -> Option b44 =
  lam f46.
    lam o19.
      match
        o19
      with
        Some t240
      then
        Some
          (f46
             t240)
      else
        None
          {}
in
let optionMapAccum: all a115. all b42. all acc45. (acc45 -> a115 -> (acc45, b42)) -> acc45 -> Option a115 -> (acc45, Option b42) =
  lam f45.
    lam acc46.
      lam o18.
        match
          o18
        with
          Some a116
        then
          match
            f45
              acc46
              a116
          with
            (acc47, b43)
          in
          (acc47, Some
              b43)
        else
          (acc46, None
            {})
in
let optionJoin: all a114. Option (Option a114) -> Option a114 =
  lam o17.
    match
      o17
    with
      Some t239
    then
      t239
    else
      None
        {}
in
let optionBind: all a113. all b41. Option a113 -> (a113 -> Option b41) -> Option b41 =
  lam o16.
    lam f44.
      optionJoin
        (optionMap
           f44
           o16)
in
let optionCompose: all a112. all b40. all c40. (b40 -> Option c40) -> (a112 -> Option b40) -> a112 -> Option c40 =
  lam f43.
    lam g3.
      lam x67.
        optionBind
          (g3
             x67)
          f43
in
let optionZipWith: all a111. all b39. all c39. (a111 -> b39 -> c39) -> Option a111 -> Option b39 -> Option c39 =
  lam f42.
    lam o15.
      lam o25.
        match
          (o15, o25)
        with
          (Some v12, Some v22)
        then
          Some
            (f42
               v12
               v22)
        else
          None
            {}
in
let optionZipWithOrElse: all a109. all b38. all c38. (() -> c38) -> (a109 -> b38 -> c38) -> Option a109 -> Option b38 -> c38 =
  lam d10.
    lam f41.
      lam o14.
        lam o24.
          match
            (o14, o24)
          with
            (Some v11, Some v21)
          then
            f41
              v11
              v21
          else
            d10
              {}
in
let optionZipWithOr: all a108. all b37. all c37. c37 -> (a108 -> b37 -> c37) -> Option a108 -> Option b37 -> c37 =
  lam v4.
    optionZipWithOrElse
      (lam #var"85".
         v4)
in
let optionGetOrElse: all a107. (() -> a107) -> Option a107 -> a107 =
  lam d9.
    lam o10.
      match
        o10
      with
        Some t238
      then
        t238
      else
        d9
          {}
in
let optionGetOr: all a106. a106 -> Option a106 -> a106 =
  lam d8.
    optionGetOrElse
      (lam #var"84".
         d8)
in
let optionMapOrElse: all a105. all b36. (() -> b36) -> (a105 -> b36) -> Option a105 -> b36 =
  lam d7.
    lam f40.
      lam o9.
        optionGetOrElse
          d7
          (optionMap
             f40
             o9)
in
let optionMapOr: all a104. all b35. b35 -> (a104 -> b35) -> Option a104 -> b35 =
  lam d6.
    lam f39.
      lam o8.
        optionGetOr
          d6
          (optionMap
             f39
             o8)
in
let optionMapM: all a103. all b34. (a103 -> Option b34) -> [a103] -> Option [b34] =
  lam f38.
    lam l17.
      recursive
        let g2 =
          lam l18.
            lam acc44.
              match
                l18
              with
                [ hd ] ++ rest1 ++ ""
              then
                match
                  f38
                    hd
                with
                  Some x66
                then
                  g2
                    rest1
                    (snoc
                       acc44
                       x66)
                else
                  None
                    {}
              else
                Some
                  acc44
      in
      g2
        l17
        ""
in
let optionFoldlM: all a100. all b32. (a100 -> b32 -> Option a100) -> a100 -> [b32] -> Option a100 =
  lam f37.
    recursive
      let recur =
        lam a101.
          lam bs1.
            match
              bs1
            with
              [ b33 ] ++ bs2 ++ ""
            then
              let res7 =
                f37
                  a101
                  b33
              in
              match
                res7
              with
                Some a102
              then
                recur
                  a102
                  bs2
              else
                match
                  res7
                with
                  None {}
                in
                None
                    {}
            else
              match
                bs1
              with
                ""
              in
              Some
                  a101
    in
    recur
in
let optionContains: all a99. Option a99 -> (a99 -> Bool) -> Bool =
  lam o7.
    lam p19.
      optionMapOr
        false
        p19
        o7
in
let optionIsSome: all a98. Option a98 -> Bool =
  lam o6.
    optionContains
      o6
      (lam #var"83".
         true)
in
let optionIsNone: all a97. Option a97 -> Bool =
  lam o5.
    not
      (optionIsSome
         o5)
in
let optionAnd: all a96. Option a96 -> Option a96 -> Option a96 =
  lam o13.
    lam o23.
      match
        (o13, o23)
      with
        (Some _, Some _)
      then
        o13
      else
        None
          {}
in
let optionFilter: all a95. (a95 -> Bool) -> Option a95 -> Option a95 =
  lam p18.
    lam o4.
      match
        optionContains
          o4
          p18
      with
        true
      then
        o4
      else
        None
          {}
in
let optionOrElse: all a94. (() -> Option a94) -> Option a94 -> Option a94 =
  lam f36.
    lam o3.
      optionGetOrElse
        f36
        (optionMap
           (lam x65.
              Some
                x65)
           o3)
in
let optionOr: all a93. Option a93 -> Option a93 -> Option a93 =
  lam o12.
    lam o22.
      optionOrElse
        (lam #var"82".
           o22)
        o12
in
let optionXor: all a92. Option a92 -> Option a92 -> Option a92 =
  lam o11.
    lam o21.
      match
        (o11, o21)
      with
        (Some _, None {})
      then
        o11
      else
        match
          (o11, o21)
        with
          (None {}, Some _)
        then
          o21
        else
          None
            {}
in
let make =
  lam n19.
    lam v3.
      create
        n19
        (lam #var"81".
           v3)
in
let last =
  lam seq35.
    get
      seq35
      (subi
         (length
            seq35)
         1)
in
let init =
  lam seq34.
    subsequence
      seq34
      0
      (subi
         (length
            seq34)
         1)
in
let eqSeq: all a91. all b31. (a91 -> b31 -> Bool) -> [a91] -> [b31] -> Bool =
  lam eq4.
    lam s119.
      lam s219.
        recursive
          let work14 =
            lam s120.
              lam s220.
                match
                  (s120, s220)
                with
                  ([ h12 ] ++ t1101 ++ "", [ h22 ] ++ t237 ++ "")
                then
                  match
                    eq4
                      h12
                      h22
                  with
                    true
                  then
                    work14
                      t1101
                      t237
                  else
                    false
                else
                  true
        in
        let n18 =
          length
            s119
        in
        let n23 =
          length
            s219
        in
        let ndiff1 =
          subi
            n18
            n23
        in
        match
          eqi
            ndiff1
            0
        with
          true
        then
          work14
            s119
            s219
        else
          false
in
let toRope =
  lam seq33.
    createRope
      (length
         seq33)
      (lam i17.
         get
           seq33
           i17)
in
let toList =
  lam seq32.
    createList
      (length
         seq32)
      (lam i16.
         get
           seq32
           i16)
in
let mapOption: all a89. all b29. (a89 -> Option b29) -> [a89] -> [b29] =
  lam f35.
    recursive
      let work13 =
        lam as3.
          match
            as3
          with
            [ a90 ] ++ as4 ++ ""
          then
            match
              f35
                a90
            with
              Some b30
            then
              cons
                b30
                (work13
                   as4)
            else
              work13
                as4
          else
            ""
    in
    work13
in
let for_: all a88. [a88] -> (a88 -> ()) -> () =
  lam xs9.
    lam f34.
      iter
        f34
        xs9
in
let mapReverse =
  lam f33.
    lam lst1.
      foldl
        (lam acc43.
           lam x64.
             cons
               (f33
                  x64)
               acc43)
        (toList
           "")
        lst1
in
let mapK: all a87. all b28. all c36. (a87 -> (b28 -> c36) -> c36) -> [a87] -> ([b28] -> c36) -> c36 =
  lam f32.
    lam seq31.
      lam k2.
        foldl
          (lam k3.
             lam x62.
               lam xs8.
                 f32
                   x62
                   (lam x63.
                      k3
                        (cons
                           x63
                           xs8)))
          k2
          seq31
          ""
in
let foldl1 =
  lam f31.
    lam l16.
      foldl
        f31
        (head
           l16)
        (tail
           l16)
in
let foldr1 =
  lam f30.
    lam seq30.
      foldr
        f30
        (last
           seq30)
        (init
           seq30)
in
recursive
  let unfoldr: all a86. all c35. (a86 -> Option (c35, a86)) -> a86 -> [c35] =
    lam f29.
      lam b0.
        let fb =
          f29
            b0
        in
        match
          fb
        with
          None _
        then
          ""
        else
          match
            fb
          with
            Some (x61, b110)
          in
          cons
              x61
              (unfoldr
                 f29
                 b110)
in
let range =
  lam s46.
    lam e6.
      lam by.
        unfoldr
          (lam b27.
             match
               leqi
                 e6
                 b27
             with
               true
             then
               None
                 {}
             else
               Some
                 (b27, addi
                   b27
                   by))
          s46
in
recursive
  let foldl2: all a85. all b26. all c34. (a85 -> b26 -> c34 -> a85) -> a85 -> [b26] -> [c34] -> a85 =
    lam f28.
      lam acc38.
        lam seq112.
          lam seq29.
            let g1 =
              lam acc41: (a85, [b26]).
                lam x213.
                  match
                    acc41
                  with
                    (acc42, [ x113 ] ++ xs11 ++ "")
                  in
                  (f28
                      acc42
                      x113
                      x213, xs11)
            in
            match
              geqi
                (length
                   seq112)
                (length
                   seq29)
            with
              true
            then
              match
                foldl
                  g1
                  (acc38, seq112)
                  seq29
              with
                (acc39, _)
              in
              acc39
            else
              foldl2
                (lam acc40.
                   lam x112.
                     lam x212.
                       f28
                         acc40
                         x212
                         x112)
                acc38
                seq29
                seq112
in
let foldli: all a84. all b25. (a84 -> Int -> b25 -> a84) -> a84 -> [b25] -> a84 =
  lam fn.
    lam initAcc.
      lam seq28.
        recursive
          let work12 =
            lam acc37.
              lam i15.
                lam s45.
                  match
                    s45
                  with
                    [ e5 ] ++ rest ++ ""
                  then
                    work12
                      (fn
                         acc37
                         i15
                         e5)
                      (addi
                         i15
                         1)
                      rest
                  else
                    acc37
        in
        work12
          initAcc
          0
          seq28
in
let zipWith: all a83. all b24. all c33. (a83 -> b24 -> c33) -> [a83] -> [b24] -> [c33] =
  lam f27.
    foldl2
      (lam acc36.
         lam x111.
           lam x211.
             snoc
               acc36
               (f27
                  x111
                  x211))
      ""
in
let zipWithIndex: all a82. all b23. all c32. (Int -> a82 -> b23 -> c32) -> [a82] -> [b23] -> [c32] =
  lam f26.
    lam a110.
      lam a210.
        recursive
          let work11 =
            lam acc35.
              lam i14.
                lam seq111.
                  lam seq27.
                    match
                      seq111
                    with
                      [ e11 ] ++ seq1tail ++ ""
                    then
                      match
                        seq27
                      with
                        [ e21 ] ++ seq2tail ++ ""
                      then
                        work11
                          (cons
                             (f26
                                i14
                                e11
                                e21)
                             acc35)
                          (addi
                             i14
                             1)
                          seq1tail
                          seq2tail
                      else
                        reverse
                          acc35
                    else
                      reverse
                        acc35
        in
        work11
          (toList
             "")
          0
          a110
          a210
in
let zip: all a81. all b22. [a81] -> [b22] -> [(a81, b22)] =
  zipWith
    (lam x60.
       lam y10.
         (x60, y10))
in
let mapAccumL: all a80. all b21. all c31. (a80 -> b21 -> (a80, c31)) -> a80 -> [b21] -> (a80, [c31]) =
  lam f25: a80 -> b21 -> (a80, c31).
    lam acc33.
      lam seq26.
        foldl
          (lam tacc1: (a80, [c31]).
             lam x59.
               match
                 f25
                   (tacc1.0)
                   x59
               with
                 (acc34, y9)
               in
               (acc34, snoc
                   (tacc1.1)
                   y9))
          (acc33, "")
          seq26
in
let mapAccumR: all a79. all b20. all c30. (a79 -> b20 -> (a79, c30)) -> a79 -> [b20] -> (a79, [c30]) =
  lam f24: a79 -> b20 -> (a79, c30).
    lam acc31.
      lam seq25.
        foldr
          (lam x58.
             lam tacc: (a79, [c30]).
               match
                 f24
                   (tacc.0)
                   x58
               with
                 (acc32, y8)
               in
               (acc32, cons
                   y8
                   (tacc.1)))
          (acc31, "")
          seq25
in
let unzip: all a78. all b19. [(a78, b19)] -> ([a78], [b19]) =
  mapAccumL
    (lam l15.
       lam p17: (a78, b19).
         (snoc
           l15
           (p17.0), p17.1))
    ""
in
let iter2: all a77. all b18. (a77 -> b18 -> ()) -> [a77] -> [b18] -> () =
  lam f22.
    lam seq110.
      lam seq24.
        let f23 =
          lam x57: (a77, b18).
            match
              x57
            with
              (x110, x210)
            in
            f22
                x110
                x210
        in
        iter
          f23
          (zip
             seq110
             seq24)
in
recursive
  let any =
    lam p16.
      lam seq23.
        match
          null
            seq23
        with
          true
        then
          false
        else
          match
            p16
              (head
                 seq23)
          with
            true
          then
            true
          else
            any
              p16
              (tail
                 seq23)
in
recursive
  let forAll =
    lam p15.
      lam seq22.
        match
          null
            seq22
        with
          true
        then
          true
        else
          match
            p15
              (head
                 seq22)
          with
            true
          then
            forAll
              p15
              (tail
                 seq22)
          else
            false
in
let join =
  lam seqs.
    foldl
      concat
      ""
      seqs
in
let seqLiftA2: all a75. all b17. all c29. (a75 -> b17 -> c29) -> [a75] -> [b17] -> [c29] =
  lam f21.
    lam as2.
      lam bs.
        join
          (map
             (lam a76.
                map
                  (f21
                     a76)
                  bs)
             as2)
in
let seqMapM: all a73. all b16. (a73 -> [b16]) -> [a73] -> [[b16]] =
  lam f20.
    foldr
      (lam a74.
         lam acc30.
           seqLiftA2
             cons
             (f20
                a74)
             acc30)
      [ "" ]
in
recursive
  let filter =
    lam p14.
      lam seq20.
        match
          null
            seq20
        with
          true
        then
          ""
        else
          match
            p14
              (head
                 seq20)
          with
            true
          then
            cons
              (head
                 seq20)
              (filter
                 p14
                 (tail
                    seq20))
          else
            filter
              p14
              (tail
                 seq20)
in
recursive
  let filterOption: all a72. [Option a72] -> [a72] =
    lam optSeq.
      match
        optSeq
      with
        [ Some x56 ] ++ optSeq1 ++ ""
      then
        cons
          x56
          (filterOption
             optSeq1)
      else
        match
          optSeq
        with
          [ None _ ] ++ optSeq2 ++ ""
        then
          filterOption
            optSeq2
        else
          ""
in
recursive
  let find =
    lam p13.
      lam seq19.
        match
          null
            seq19
        with
          true
        then
          None
            {}
        else
          match
            p13
              (head
                 seq19)
          with
            true
          then
            Some
              (head
                 seq19)
          else
            find
              p13
              (tail
                 seq19)
in
recursive
  let findMap: all a71. all b15. (a71 -> Option b15) -> [a71] -> Option b15 =
    lam f19.
      lam seq18.
        match
          seq18
        with
          [ h4 ] ++ t236 ++ ""
        then
          match
            f19
              h4
          with
            Some x55
          then
            Some
              x55
          else
            findMap
              f19
              t236
        else
          None
            {}
in
let lowerBoundBinarySearch: all a70. (a70 -> Int) -> [a70] -> Option Int =
  lam f18.
    lam s44.
      recursive
        let work10 =
          lam first.
            lam count.
              match
                gti
                  count
                  0
              with
                true
              then
                let step1 =
                  divi
                    count
                    2
                in
                let idx3 =
                  addi
                    first
                    step1
                in
                match
                  lti
                    (f18
                       (get
                          s44
                          idx3))
                    0
                with
                  true
                then
                  work10
                    (addi
                       first
                       (addi
                          step1
                          1))
                    (subi
                       count
                       (addi
                          step1
                          1))
                else
                  work10
                    first
                    step1
              else
                first
      in
      let idx2 =
        work10
          0
          (length
             s44)
      in
      match
        eqi
          idx2
          (length
             s44)
      with
        true
      then
        None
          {}
      else
        Some
          idx2
in
let s =
  [ 0,
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9 ]
in
let partition =
  lam p12.
    lam seq15.
      recursive
        let work9 =
          lam l14.
            lam r13.
              lam seq16.
                match
                  seq16
                with
                  ""
                then
                  (l14, r13)
                else
                  match
                    seq16
                  with
                    [ s43 ] ++ seq17 ++ ""
                  in
                  match
                      p12
                        s43
                    with
                      true
                    then
                      work9
                        (cons
                           s43
                           l14)
                        r13
                        seq17
                    else
                      work9
                        l14
                        (cons
                           s43
                           r13)
                        seq17
      in
      work9
        ""
        ""
        (reverse
           seq15)
in
let distinct =
  lam eq3.
    lam seq13.
      recursive
        let work8 =
          lam seq14.
            lam seq21.
              match
                seq14
              with
                [ h3 ] ++ t235 ++ ""
              then
                match
                  find
                    (eq3
                       h3)
                    seq21
                with
                  Some _
                then
                  work8
                    t235
                    seq21
                else
                  cons
                    h3
                    (work8
                       t235
                       (cons
                          h3
                          seq21))
              else
                ""
      in
      work8
        seq13
        ""
in
let distinctSorted =
  lam eq2.
    lam s41.
      recursive
        let work7 =
          lam acc29.
            lam s42.
              match
                s42
              with
                [ h11 ] ++ t234 ++ ""
              then
                match
                  acc29
                with
                  [ h21 ] ++ _ ++ ""
                then
                  match
                    eq2
                      h11
                      h21
                  with
                    true
                  then
                    work7
                      acc29
                      t234
                  else
                    work7
                      (cons
                         h11
                         acc29)
                      t234
                else
                  work7
                    [ h11 ]
                    t234
              else
                acc29
      in
      reverse
        (work7
           ""
           s41)
in
recursive
  let quickSort: all a69. (a69 -> a69 -> Int) -> [a69] -> [a69] =
    lam cmp10.
      lam seq12.
        match
          null
            seq12
        with
          true
        then
          seq12
        else
          let h =
            head
              seq12
          in
          let t233 =
            tail
              seq12
          in
          let lr1 =
            partition
              (lam x54.
                 lti
                   (cmp10
                      x54
                      h)
                   0)
              t233
          in
          concat
            (quickSort
               cmp10
               (lr1.0))
            (cons
               h
               (quickSort
                  cmp10
                  (lr1.1)))
in
recursive
  let merge =
    lam cmp9.
      lam l13.
        lam r12.
          match
            l13
          with
            ""
          then
            r12
          else
            match
              r12
            with
              ""
            then
              l13
            else
              match
                (l13, r12)
              with
                ([ x53 ] ++ xs7 ++ "", [ y7 ] ++ ys1 ++ "")
              in
              match
                  leqi
                    (cmp9
                       x53
                       y7)
                    0
                with
                  true
                then
                  cons
                    x53
                    (merge
                       cmp9
                       xs7
                       r12)
                else
                  cons
                    y7
                    (merge
                       cmp9
                       l13
                       ys1)
in
recursive
  let mergeSort =
    lam cmp8.
      lam seq11.
        match
          seq11
        with
          ""
        then
          ""
        else
          match
            seq11
          with
            [ x52 ]
          then
            [ x52 ]
          else
            let lr =
              splitAt
                seq11
                (divi
                   (length
                      seq11)
                   2)
            in
            merge
              cmp8
              (mergeSort
                 cmp8
                 (lr.0))
              (mergeSort
                 cmp8
                 (lr.1))
in
let sort =
  quickSort
in
let minIdx: all a68. (a68 -> a68 -> Int) -> [a68] -> Option (Int, a68) =
  lam cmp7: a68 -> a68 -> Int.
    lam seq10: [a68].
      match
        null
          seq10
      with
        true
      then
        None
          {}
      else
        match
          foldl
            (lam acc28: (Int, Int, a68).
               lam e4: a68.
                 match
                   acc28
                 with
                   (curi, mini1, m4)
                 in
                 match
                     lti
                       (cmp7
                          m4
                          e4)
                       0
                   with
                     true
                   then
                     (addi
                       curi
                       1, mini1, m4)
                   else
                     (addi
                       curi
                       1, curi, e4))
            (1, 0, head
              seq10)
            (tail
               seq10)
        with
          (_, i13, m5)
        in
        Some
            (i13, m5)
in
let min: all a67. (a67 -> a67 -> Int) -> [a67] -> Option a67 =
  lam cmp6.
    lam seq9.
      optionMap
        (lam r11.
           match
             r11
           with
             (_, m3)
           in
           m3)
        (minIdx
           cmp6
           seq9)
in
let max =
  lam cmp5.
    min
      (lam l12.
         lam r10.
           cmp5
             r10
             l12)
in
let minOrElse =
  lam d5.
    lam cmp4.
      lam seq8.
        optionGetOrElse
          d5
          (min
             cmp4
             seq8)
in
let maxOrElse =
  lam d4.
    lam cmp3.
      minOrElse
        d4
        (lam l11.
           lam r9.
             cmp3
               r9
               l11)
in
let index =
  lam pred3.
    lam seq6.
      recursive
        let index_rechelper =
          lam i12.
            lam pred4.
              lam seq7.
                match
                  null
                    seq7
                with
                  true
                then
                  None
                    {}
                else
                  match
                    pred4
                      (head
                         seq7)
                  with
                    true
                  then
                    Some
                      i12
                  else
                    index_rechelper
                      (addi
                         i12
                         1)
                      pred4
                      (tail
                         seq7)
      in
      index_rechelper
        0
        pred3
        seq6
in
let lastIndex =
  lam pred1.
    lam seq4.
      recursive
        let lastIndex_rechelper =
          lam i11.
            lam acc27.
              lam pred2.
                lam seq5.
                  match
                    null
                      seq5
                  with
                    true
                  then
                    acc27
                  else
                    match
                      pred2
                        (head
                           seq5)
                    with
                      true
                    then
                      lastIndex_rechelper
                        (addi
                           i11
                           1)
                        (Some
                           i11)
                        pred2
                        (tail
                           seq5)
                    else
                      lastIndex_rechelper
                        (addi
                           i11
                           1)
                        acc27
                        pred2
                        (tail
                           seq5)
      in
      lastIndex_rechelper
        0
        (None
           {})
        pred1
        seq4
in
recursive
  let isPrefix =
    lam eq1.
      lam s118.
        lam s218.
          match
            null
              s118
          with
            true
          then
            true
          else
            match
              null
                s218
            with
              true
            then
              false
            else
              and
                (eq1
                   (head
                      s118)
                   (head
                      s218))
                (isPrefix
                   eq1
                   (tail
                      s118)
                   (tail
                      s218))
in
let isSuffix =
  lam eq.
    lam s117.
      lam s217.
        isPrefix
          eq
          (reverse
             s117)
          (reverse
             s217)
in
let seqCmp: all a66. (a66 -> a66 -> Int) -> [a66] -> [a66] -> Int =
  lam cmp2.
    lam s115.
      lam s215.
        recursive
          let work6 =
            lam s116.
              lam s216.
                match
                  (s116, s216)
                with
                  ([ h1 ] ++ t1100 ++ "", [ h2 ] ++ t232 ++ "")
                then
                  let c28 =
                    cmp2
                      h1
                      h2
                  in
                  match
                    eqi
                      c28
                      0
                  with
                    true
                  then
                    work6
                      t1100
                      t232
                  else
                    c28
                else
                  0
        in
        let n17 =
          length
            s115
        in
        let n22 =
          length
            s215
        in
        let ndiff =
          subi
            n17
            n22
        in
        match
          eqi
            ndiff
            0
        with
          true
        then
          work6
            s115
            s215
        else
          ndiff
in
let randIndex: all a65. [a65] -> Option Int =
  lam seq3.
    match
      seq3
    with
      ""
    then
      None
        {}
    else
      Some
        (randIntU
           0
           (length
              seq3))
in
let randElem: all a64. [a64] -> Option a64 =
  lam seq2.
    optionMap
      (get
         seq2)
      (randIndex
         seq2)
in
let permute: all a63. [a63] -> [Int] -> [a63] =
  lam elems.
    lam permutation.
      match
        eqi
          (length
             elems)
          (length
             permutation)
      with
        true
      then
        let ordered =
          sort
            (lam x51: (a63, Int).
               lam y6: (a63, Int).
                 subi
                   (x51.1)
                   (y6.1))
            (zip
               elems
               permutation)
        in
        match
          unzip
            ordered
        with
          (orderedElems, _)
        in
        orderedElems
      else
        error
          "Expected sequences of equal length"
in
let identity =
  lam x50.
    x50
in
let const =
  lam x49.
    lam #var"80".
      x49
in
let apply =
  lam f17.
    lam x48.
      f17
        x48
in
let compose =
  lam f16.
    lam g.
      lam x47.
        f16
          (g
             x47)
in
let curry =
  lam f15.
    lam x46.
      lam y5.
        f15
          (x46, y5)
in
let uncurry: all a62. all b14. all c20. (a62 -> b14 -> c20) -> (a62, b14) -> c20 =
  lam f14.
    lam t231: (a62, b14).
      f14
        (t231.0)
        (t231.1)
in
let flip =
  lam f13.
    lam x45.
      lam y4.
        f13
          y4
          x45
in
let printLn =
  lam s40.
    let #var"79" =
      print
        (concat
           s40
           "\n")
    in
    flushStdout
      {}
in
let printSeq =
  lam s39.
    print
      (join
         s39)
in
let printSeqLn =
  lam s38.
    let #var"77" =
      printSeq
        s38
    in
    let #var"78" =
      print
        "\n"
    in
    flushStdout
      {}
in
let dprintLn =
  lam x44.
    let #var"76" =
      dprint
        x44
    in
    printLn
      ""
in
recursive
  let fix: all a61. all b13. ((a61 -> b13) -> a61 -> b13) -> a61 -> b13 =
    lam f12.
      lam e3.
        f12
          (fix
             f12)
          e3
in
let repeat: (() -> ()) -> Int -> () =
  lam f11.
    lam n15.
      recursive
        let rec9 =
          lam n16.
            match
              leqi
                n16
                0
            with
              true
            then
              {}
            else
              let #var"75" =
                f11
                  {}
              in
              rec9
                (subi
                   n16
                   1)
      in
      rec9
        n15
in
let repeati: (Int -> ()) -> Int -> () =
  lam f10.
    lam n14.
      recursive
        let rec8 =
          lam i10.
            match
              geqi
                i10
                n14
            with
              true
            then
              {}
            else
              let #var"74" =
                f10
                  i10
              in
              rec8
                (addi
                   i10
                   1)
      in
      rec8
        0
in
let fixMutual: all a60. all b12. [[a60 -> b12] -> a60 -> b12] -> [a60 -> b12] =
  lam l8.
    let l9 =
      map
        (lam li1.
           (li1,))
        l8
    in
    fix
      (lam self.
         lam l10.
           map
             (lam li: ([a60 -> b12] -> a60 -> b12,).
                lam x43.
                  (li.0)
                    (self
                       l10)
                    x43)
             l10)
      l9
in
let maxf: Float -> Float -> Float =
  lam r8.
    lam l7.
      match
        gtf
          r8
          l7
      with
        true
      then
        r8
      else
        l7
in
let absf: Float -> Float =
  lam f9.
    maxf
      f9
      (negf
         f9)
in
let eqfApprox =
  lam epsilon1.
    lam r7.
      lam l6.
        match
          leqf
            (absf
               (subf
                  r7
                  l6))
            epsilon1
        with
          true
        then
          true
        else
          false
in
let _eqf =
  eqfApprox
    1e-15
in
external externalExp : Float -> Float
in
let exp =
  lam x42: Float.
    externalExp
      x42
in
external externalLog : Float -> Float
in
let log =
  lam x41: Float.
    externalLog
      x41
in
external externalAtan : Float -> Float
in
let atan =
  lam x40: Float.
    externalAtan
      x40
in
let pi =
  mulf
    4.
    (atan
       1.)
in
external externalSin : Float -> Float
in
let sin =
  lam x39: Float.
    externalSin
      x39
in
external externalCos : Float -> Float
in
let cos =
  lam x38: Float.
    externalCos
      x38
in
external externalAtan2 : Float -> Float -> Float
in
let atan2 =
  lam x37: Float.
    lam y3: Float.
      externalAtan2
        x37
        y3
in
external externalPow : Float -> Float -> Float
in
let pow =
  lam x36: Float.
    lam y2: Float.
      externalPow
        x36
        y2
in
external externalSqrt : Float -> Float
in
let sqrt: Float -> Float =
  lam x35.
    externalSqrt
      x35
in
let inf =
  divf
    1.
    0.
in
let nan =
  mulf
    0.
    inf
in
let minf: Float -> Float -> Float =
  lam r6.
    lam l5.
      match
        ltf
          r6
          l5
      with
        true
      then
        r6
      else
        l5
in
let cmpfApprox: Float -> Float -> Float -> Int =
  lam epsilon.
    lam l4.
      lam r5.
        match
          eqfApprox
            epsilon
            l4
            r5
        with
          true
        then
          0
        else
          match
            ltf
              l4
              r5
          with
            true
          then
            subi
              0
              1
          else
            1
in
let logFactorial: Int -> Float =
  lam n12.
    recursive
      let work5 =
        lam acc26.
          lam n13.
            match
              gti
                n13
                0
            with
              true
            then
              work5
                (addf
                   (log
                      (int2float
                         n13))
                   acc26)
                (subi
                   n13
                   1)
            else
              acc26
    in
    work5
      0.
      n12
in
let maxi =
  lam r4.
    lam l3.
      match
        gti
          r4
          l3
      with
        true
      then
        r4
      else
        l3
in
let mini =
  lam r3.
    lam l2.
      match
        lti
          r3
          l2
      with
        true
      then
        r3
      else
        l2
in
let absi =
  lam i9.
    maxi
      i9
      (negi
         i9)
in
let succ =
  lam x34.
    addi
      x34
      1
in
let pred =
  lam x33.
    subi
      x33
      1
in
external externalGammaLogPdf : Float -> Float -> Float -> Float
in
external externalGammaSample! : Float -> Float -> Float
in
let gammaPdf =
  lam shape2: Float.
    lam scale2: Float.
      lam x32: Float.
        exp
          (externalGammaLogPdf
             x32
             shape2
             scale2)
in
let gammaLogPdf =
  lam shape1: Float.
    lam scale1: Float.
      lam x31: Float.
        externalGammaLogPdf
          x31
          shape1
          scale1
in
let gammaSample =
  lam shape: Float.
    lam scale: Float.
      externalGammaSample
        shape
        scale
in
external externalBinomialLogPmf : Int -> Float -> Int -> Float
in
external externalBinomialSample! : Float -> Int -> Int
in
let binomialPmf =
  lam p11: Float.
    lam n11: Int.
      lam x30: Int.
        exp
          (externalBinomialLogPmf
             x30
             p11
             n11)
in
let binomialLogPmf =
  lam p10: Float.
    lam n10: Int.
      lam x29: Int.
        externalBinomialLogPmf
          x29
          p10
          n10
in
let binomialSample =
  lam p9: Float.
    lam n9: Int.
      externalBinomialSample
        p9
        n9
in
let bernoulliPmf =
  lam p8: Float.
    lam x28: Bool.
      match
        x28
      with
        true
      then
        p8
      else
        subf
          1.
          p8
in
let bernoulliLogPmf =
  lam p7: Float.
    lam x27: Bool.
      log
        (bernoulliPmf
           p7
           x27)
in
let bernoulliSample =
  lam p6: Float.
    match
      eqi
        1
        (externalBinomialSample
           p6
           1)
    with
      true
    then
      true
    else
      false
in
external externalBetaLogPdf : Float -> Float -> Float -> Float
in
external externalBetaSample! : Float -> Float -> Float
in
let betaPdf =
  lam a59: Float.
    lam b11: Float.
      lam x26: Float.
        exp
          (externalBetaLogPdf
             x26
             a59
             b11)
in
let betaLogPdf =
  lam a58: Float.
    lam b10: Float.
      lam x25: Float.
        externalBetaLogPdf
          x25
          a58
          b10
in
let betaSample =
  lam a57: Float.
    lam b9: Float.
      externalBetaSample
        a57
        b9
in
external externalGaussianLogPdf : Float -> Float -> Float -> Float
in
external externalGaussianSample! : Float -> Float -> Float
in
let gaussianPdf =
  lam mu2: Float.
    lam sigma4: Float.
      lam x24: Float.
        exp
          (externalGaussianLogPdf
             x24
             mu2
             sigma4)
in
let gaussianLogPdf =
  lam mu1: Float.
    lam sigma3: Float.
      lam x23: Float.
        externalGaussianLogPdf
          x23
          mu1
          sigma3
in
let gaussianSample =
  lam mu: Float.
    lam sigma2: Float.
      externalGaussianSample
        mu
        sigma2
in
external externalMultinomialLogPmf : [Int] -> [Float] -> Float
in
external externalMultinomialSample! : Int -> [Float] -> [Int]
in
external externalCategoricalSample! : [Float] -> Int
in
let multinomialLogPmf: [Float] -> [Int] -> Float =
  lam ps5.
    lam ns6.
      externalMultinomialLogPmf
        ns6
        ps5
in
let multinomialPmf: [Float] -> [Int] -> Float =
  lam ps4.
    lam ns5.
      exp
        (externalMultinomialLogPmf
           ns5
           ps4)
in
let categoricalLogPmf: [Float] -> Int -> Float =
  lam ps3.
    lam x22.
      log
        (get
           ps3
           x22)
in
let categoricalPmf: [Float] -> Int -> Float =
  lam ps2.
    lam x21.
      get
        ps2
        x21
in
let multinomialSample: [Float] -> Int -> [Int] =
  lam ps1.
    lam n8.
      externalMultinomialSample
        n8
        ps1
in
let categoricalSample: [Float] -> Int =
  lam ps.
    externalCategoricalSample
      ps
in
external externalDirichletLogPdf : [Float] -> [Float] -> Float
in
external externalDirichletSample : [Float] -> [Float]
in
let dirichletLogPdf: [Float] -> [Float] -> Float =
  lam alpha2.
    lam xs6.
      match
        eqfApprox
          1e-15
          (foldl
             addf
             0.
             xs6)
          1.
      with
        true
      then
        externalDirichletLogPdf
          xs6
          alpha2
      else
        negf
          inf
in
let dirichletPdf: [Float] -> [Float] -> Float =
  lam alpha1.
    lam xs5.
      exp
        (externalDirichletLogPdf
           xs5
           alpha1)
in
let dirichletSample: [Float] -> [Float] =
  lam alpha.
    externalDirichletSample
      alpha
in
external externalUniformContinuousSample! : Float -> Float -> Float
in
let uniformContinuousSample =
  lam a56.
    lam b8.
      externalUniformContinuousSample
        a56
        b8
in
let uniformContinuousLogPdf =
  lam a55.
    lam b7.
      lam x20.
        match
          geqf
            x20
            a55
        with
          true
        then
          match
            leqf
              x20
              b7
          with
            true
          then
            subf
              (log
                 1.)
              (log
                 (subf
                    b7
                    a55))
          else
            0.
        else
          0.
in
let uniformContinuousPdf =
  lam a54.
    lam b6.
      lam x19.
        match
          geqf
            x19
            a54
        with
          true
        then
          match
            leqf
              x19
              b6
          with
            true
          then
            divf
              1.
              (subf
                 b6
                 a54)
          else
            0.
        else
          0.
in
let uniformSample: () -> Float =
  lam #var"73".
    uniformContinuousSample
      0.
      1.
in
external externalUniformDiscreteSample! : Int -> Int -> Int
in
let uniformDiscreteSample =
  lam a53: Int.
    lam b5: Int.
      externalUniformDiscreteSample
        a53
        b5
in
let uniformDiscreteLogPdf: Int -> Int -> Int -> Float =
  lam a52.
    lam b4.
      lam x18.
        match
          geqi
            x18
            a52
        with
          true
        then
          match
            leqi
              x18
              b4
          with
            true
          then
            subf
              (log
                 1.)
              (log
                 (int2float
                    (addi
                       1
                       (subi
                          b4
                          a52))))
          else
            0.
        else
          0.
in
let uniformDiscretePdf: Int -> Int -> Int -> Float =
  lam a51.
    lam b3.
      lam x17.
        match
          geqi
            x17
            a51
        with
          true
        then
          match
            leqi
              x17
              b3
          with
            true
          then
            divf
              1.
              (int2float
                 (addi
                    1
                    (subi
                       b3
                       a51)))
          else
            0.
        else
          0.
in
let poissonLogPmf =
  lam lambda5: Float.
    lam x16: Int.
      subf
        (subf
           (mulf
              (int2float
                 x16)
              (log
                 lambda5))
           lambda5)
        (logFactorial
           x16)
in
let poissonPmf =
  lam lambda4: Float.
    lam x15: Int.
      exp
        (poissonLogPmf
           lambda4
           x15)
in
let poissonSample =
  lam lambda3: Float.
    let enlam =
      exp
        (negf
           lambda3)
    in
    let x13 =
      0
    in
    let prod =
      1.
    in
    recursive
      let rec7 =
        lam x14.
          lam prod1.
            let u1 =
              uniformSample
                {}
            in
            let prod2 =
              mulf
                prod1
                u1
            in
            match
              gtf
                prod2
                enlam
            with
              true
            then
              rec7
                (addi
                   x14
                   1)
                prod2
            else
              x14
    in
    rec7
      x13
      prod
in
external externalExponentialSample! : Float -> Float
in
let exponentialSample =
  lam lambda2: Float.
    externalExponentialSample
      lambda2
in
let exponentialLogPdf: Float -> Float -> Float =
  lam lambda1.
    lam x12.
      subf
        (log
           lambda1)
        (mulf
           lambda1
           x12)
in
let exponentialPdf: Float -> Float -> Float =
  lam lambda.
    lam x11.
      exp
        (exponentialLogPdf
           lambda
           x11)
in
external externalSetSeed! : Int -> ()
in
let setSeed: Int -> () =
  lam seed.
    externalSetSeed
      seed
in
let eqChar =
  lam c114.
    lam c27.
      eqc
        c114
        c27
in
let neqChar =
  lam c113.
    lam c26.
      not
        (eqc
           c113
           c26)
in
let ltChar =
  lam c112.
    lam c25.
      lti
        (char2int
           c112)
        (char2int
           c25)
in
let gtChar =
  lam c111.
    lam c24.
      gti
        (char2int
           c111)
        (char2int
           c24)
in
let leqChar =
  lam c110.
    lam c23.
      leqi
        (char2int
           c110)
        (char2int
           c23)
in
let geqChar =
  lam c19.
    lam c22.
      geqi
        (char2int
           c19)
        (char2int
           c22)
in
let cmpChar =
  lam c18.
    lam c21.
      subi
        (char2int
           c18)
        (char2int
           c21)
in
let _escapes =
  [ ('\n', "\\n"),
    ('\t', "\\t"),
    ('\r', "\\r"),
    ('\\', "\\\\"),
    ('\"', "\\\""),
    ('\'', "\\\'") ]
in
let escapeChar =
  lam c17.
    match
      find
        (lam e2: (Char, [Char]).
           eqChar
             c17
             (e2.0))
        _escapes
    with
      Some n6
    then
      let n7: (Char, [Char]) =
        n6
      in
      n7.1
    else
      [ c17 ]
in
let showChar =
  lam c16.
    join
      [ "\'",
        escapeChar
          c16,
        "\'" ]
in
let char2upper =
  lam c15.
    match
      and
        (geqChar
           c15
           'a')
        (leqChar
           c15
           'z')
    with
      true
    then
      int2char
        (subi
           (char2int
              c15)
           32)
    else
      c15
in
let char2lower =
  lam c14.
    match
      and
        (geqChar
           c14
           'A')
        (leqChar
           c14
           'Z')
    with
      true
    then
      int2char
        (addi
           (char2int
              c14)
           32)
    else
      c14
in
let isWhitespace =
  lam c13.
    any
      (eqChar
         c13)
      " \n\t\r"
in
let isLowerAlpha =
  lam c12.
    let i8 =
      char2int
        c12
    in
    match
      leqi
        (char2int
           'a')
        i8
    with
      true
    then
      leqi
        i8
        (char2int
           'z')
    else
      false
in
let isUpperAlpha =
  lam c11.
    let i7 =
      char2int
        c11
    in
    match
      leqi
        (char2int
           'A')
        i7
    with
      true
    then
      leqi
        i7
        (char2int
           'Z')
    else
      false
in
let isAlpha =
  lam c10.
    match
      isLowerAlpha
        c10
    with
      true
    then
      true
    else
      isUpperAlpha
        c10
in
let isLowerAlphaOrUnderscore =
  lam c9.
    match
      isLowerAlpha
        c9
    with
      true
    then
      true
    else
      eqChar
        c9
        '_'
in
let isAlphaOrUnderscore =
  lam c8.
    match
      isAlpha
        c8
    with
      true
    then
      true
    else
      eqChar
        c8
        '_'
in
let isDigit =
  lam c7.
    let i6 =
      char2int
        c7
    in
    match
      leqi
        (char2int
           '0')
        i6
    with
      true
    then
      leqi
        i6
        (char2int
           '9')
    else
      false
in
let isAlphanum =
  lam c6.
    match
      isAlpha
        c6
    with
      true
    then
      true
    else
      isDigit
        c6
in
let randAlphanum: () -> Char =
  lam #var"72".
    let r2 =
      randIntU
        0
        62
    in
    match
      lti
        r2
        10
    with
      true
    then
      int2char
        (addi
           r2
           48)
    else
      match
        lti
          r2
          36
      with
        true
      then
        int2char
          (addi
             r2
             55)
      else
        int2char
          (addi
             r2
             61)
in
let emptyStr: [Char] =
  ""
in
let escapeString =
  lam s37.
    join
      (map
         escapeChar
         s37)
in
let eqString =
  lam s114.
    lam s214.
      eqSeq
        eqc
        s114
        s214
in
let neqString =
  lam s113.
    lam s213.
      not
        (eqString
           s113
           s213)
in
let eqStringSlice =
  lam s112.
    lam s212.
      lam o2.
        lam n21.
          recursive
            let work4 =
              lam i5.
                match
                  eqi
                    i5
                    n21
                with
                  true
                then
                  true
                else
                  match
                    eqc
                      (get
                         s112
                         i5)
                      (get
                         s212
                         (addi
                            o2
                            i5))
                  with
                    true
                  then
                    work4
                      (addi
                         i5
                         1)
                  else
                    false
          in
          match
            eqi
              (length
                 s112)
              n21
          with
            true
          then
            work4
              0
          else
            false
in
recursive
  let ltString: [Char] -> [Char] -> Bool =
    lam s111.
      lam s211.
        match
          null
            s211
        with
          true
        then
          false
        else
          match
            null
              s111
          with
            true
          then
            true
          else
            match
              eqChar
                (head
                   s111)
                (head
                   s211)
            with
              true
            then
              ltString
                (tail
                   s111)
                (tail
                   s211)
            else
              ltChar
                (head
                   s111)
                (head
                   s211)
in
let gtString: [Char] -> [Char] -> Bool =
  lam s110.
    lam s210.
      ltString
        s210
        s110
in
let cmpString: [Char] -> [Char] -> Int =
  seqCmp
    cmpChar
in
let str2upper =
  lam s36.
    map
      char2upper
      s36
in
let str2lower =
  lam s35.
    map
      char2lower
      s35
in
let string2int =
  lam s33.
    recursive
      let string2int_rechelper =
        lam s34.
          lam acc25.
            match
              null
                s34
            with
              true
            then
              acc25
            else
              let fsd =
                subi
                  (char2int
                     (head
                        s34))
                  (char2int
                     '0')
              in
              string2int_rechelper
                (tail
                   s34)
                (addi
                   (muli
                      10
                      acc25)
                   fsd)
    in
    match
      s33
    with
      ""
    then
      0
    else
      match
        eqChar
          '-'
          (head
             s33)
      with
        true
      then
        negi
          (string2int_rechelper
             (tail
                s33)
             0)
      else
        string2int_rechelper
          s33
          0
in
let digit2char =
  lam d3.
    int2char
      (addi
         d3
         (char2int
            '0'))
in
let int2string =
  lam n4.
    recursive
      let int2string_rechelper =
        lam n5.
          lam acc24.
            match
              lti
                n5
                10
            with
              true
            then
              cons
                (digit2char
                   n5)
                acc24
            else
              int2string_rechelper
                (divi
                   n5
                   10)
                (cons
                   (digit2char
                      (modi
                         n5
                         10))
                   acc24)
    in
    match
      lti
        n4
        0
    with
      true
    then
      cons
        '-'
        (int2string_rechelper
           (negi
              n4)
           "")
    else
      int2string_rechelper
        n4
        ""
in
let stringIsInt: [Char] -> Bool =
  lam s32.
    eqString
      s32
      (int2string
         (string2int
            s32))
in
let strIndex =
  lam c4.
    lam s30.
      recursive
        let strIndex_rechelper =
          lam i4.
            lam c5.
              lam s31.
                match
                  null
                    s31
                with
                  true
                then
                  None
                    {}
                else
                  match
                    eqChar
                      c5
                      (head
                         s31)
                  with
                    true
                  then
                    Some
                      i4
                  else
                    strIndex_rechelper
                      (addi
                         i4
                         1)
                      c5
                      (tail
                         s31)
      in
      strIndex_rechelper
        0
        c4
        s30
in
let strLastIndex =
  lam c2.
    lam s28.
      recursive
        let strLastIndex_rechelper =
          lam i3.
            lam acc23.
              lam c3.
                lam s29.
                  match
                    null
                      s29
                  with
                    true
                  then
                    match
                      eqi
                        acc23
                        (negi
                           1)
                    with
                      true
                    then
                      None
                        {}
                    else
                      Some
                        acc23
                  else
                    match
                      eqChar
                        c3
                        (head
                           s29)
                    with
                      true
                    then
                      strLastIndex_rechelper
                        (addi
                           i3
                           1)
                        i3
                        c3
                        (tail
                           s29)
                    else
                      strLastIndex_rechelper
                        (addi
                           i3
                           1)
                        acc23
                        c3
                        (tail
                           s29)
      in
      strLastIndex_rechelper
        0
        (negi
           1)
        c2
        s28
in
let strSplit =
  lam delim1.
    lam s27.
      let n3 =
        length
          s27
      in
      let m2 =
        length
          delim1
      in
      recursive
        let work3 =
          lam acc22.
            lam lastMatch.
              lam i2.
                match
                  lti
                    (subi
                       n3
                       m2)
                    i2
                with
                  true
                then
                  snoc
                    acc22
                    (subsequence
                       s27
                       lastMatch
                       n3)
                else
                  match
                    eqStringSlice
                      delim1
                      s27
                      i2
                      m2
                  with
                    true
                  then
                    let nexti =
                      addi
                        i2
                        m2
                    in
                    work3
                      (snoc
                         acc22
                         (subsequence
                            s27
                            lastMatch
                            (subi
                               i2
                               lastMatch)))
                      nexti
                      nexti
                  else
                    work3
                      acc22
                      lastMatch
                      (addi
                         i2
                         1)
      in
      match
        null
          delim1
      with
        true
      then
        [ s27 ]
      else
        work3
          ""
          0
          0
in
let strTrim =
  lam s25.
    recursive
      let strTrim_init =
        lam s26.
          match
            eqString
              s26
              ""
          with
            true
          then
            s26
          else
            match
              isWhitespace
                (head
                   s26)
            with
              true
            then
              strTrim_init
                (tail
                   s26)
            else
              s26
    in
    reverse
      (strTrim_init
         (reverse
            (strTrim_init
               s25)))
in
let stringIsInt1 =
  lam s23.
    match
      null
        s23
    with
      true
    then
      false
    else
      let s24 =
        match
          eqChar
            (get
               s23
               0)
            '-'
        with
          true
        then
          tail
            s23
        else
          s23
      in
      forAll
        isDigit
        s24
in
recursive
  let strJoin =
    lam delim.
      lam strs.
        match
          null
            strs
        with
          true
        then
          ""
        else
          match
            eqi
              (length
                 strs)
              1
          with
            true
          then
            head
              strs
          else
            concat
              (concat
                 (head
                    strs)
                 delim)
              (strJoin
                 delim
                 (tail
                    strs))
in
type WriteChannel
in
type ReadChannel
in
external fileExists! : [Char] -> Bool
in
external deleteFile! : [Char] -> ()
in
let deleteFile1 =
  lam s22.
    match
      fileExists
        s22
    with
      true
    then
      deleteFile
        s22
    else
      {}
in
external fileSize! : [Char] -> Int
in
external writeOpen! : [Char] -> (WriteChannel, Bool)
in
let writeOpen1: [Char] -> Option WriteChannel =
  lam name1.
    match
      writeOpen
        name1
    with
      (wc, true)
    then
      Some
        wc
    else
      None
        {}
in
external writeString! : WriteChannel -> [Char] -> ()
in
let writeString1: WriteChannel -> [Char] -> () =
  lam c1.
    lam s21.
      writeString
        c1
        s21
in
external writeFlush! : WriteChannel -> ()
in
external writeClose! : WriteChannel -> ()
in
external readOpen! : [Char] -> (ReadChannel, Bool)
in
let readOpen1: [Char] -> Option ReadChannel =
  lam name.
    match
      readOpen
        name
    with
      (rc1, true)
    then
      Some
        rc1
    else
      None
        {}
in
external readLine! : ReadChannel -> ([Char], Bool)
in
let readLine1: ReadChannel -> Option [Char] =
  lam rc.
    match
      readLine
        rc
    with
      (s20, false)
    then
      Some
        s20
    else
      None
        {}
in
external readString! : ReadChannel -> [Char]
in
external readClose! : ReadChannel -> ()
in
external stdin! : ReadChannel
in
external stdout! : WriteChannel
in
external stderr! : WriteChannel
in
type Res a3 =
  ([Float], [a3])
in
type ResOption a4 =
  ([Float], [Option a4])
in
(match
     compileOptions.seedIsSome
   with
     true
   then
     setSeed
       (compileOptions.seed)
   else
     {})
; let negInf =
  divf
    (negf
       1.)
    0.
in
let numarg =
  lam #var"70".
    match
      neqi
        (length
           argv)
        2
    with
      true
    then
      let #var"71" =
        writeString1
          stderr
          "The number of particles/points need to be given as a program argument.\n"
      in
      exit
        1
    else
      string2int
        (get
           argv
           1)
in
let saveCSV =
  lam res6.
    lam names4.
      lam filename.
        lam expOnLogWeights1.
          match
            writeOpen1
              filename
          with
            Some ch
          then
            let #var"66" =
              writeString1
                ch
                (strJoin
                   ","
                   names4)
            in
            let #var"67" =
              writeString1
                ch
                "\n"
            in
            let #var"68" =
              iter
                (lam lst.
                   let #var"69" =
                     writeString1
                       ch
                       (strJoin
                          ","
                          (map
                             float2string
                             lst))
                   in
                   writeString1
                     ch
                     "\n")
                (expOnLogWeights1
                   res6)
            in
            writeClose
              ch
          else
            writeString1
              stderr
              (join
                 [ "Cannot write to file ",
                   filename,
                   "\n" ])
in
let printStatistics =
  lam res5.
    lam names2.
      lam normConst3.
        lam expVals2.
          lam varianceVals1.
            let pad =
              18
            in
            let padPrint =
              lam s19.
                lam n2.
                  match
                    geqi
                      n2
                      (length
                         s19)
                  with
                    true
                  then
                    let #var"64" =
                      print
                        s19
                    in
                    print
                      (create
                         (subi
                            n2
                            (length
                               s19))
                         (lam #var"65".
                            ' '))
                  else
                    print
                      s19
            in
            let #var"52" =
              padPrint
                "Variable"
                14
            in
            let #var"53" =
              padPrint
                "Expected Value"
                pad
            in
            let #var"54" =
              padPrint
                "Variance"
                pad
            in
            let #var"55" =
              padPrint
                "Standard Deviation"
                pad
            in
            let #var"56" =
              print
                "\n"
            in
            recursive
              let work2 =
                lam names3.
                  lam ev.
                    lam vv.
                      match
                        (names3, ev, vv)
                      with
                        ([ n1 ] ++ ns4 ++ "", [ e1 ] ++ es1 ++ "", [ v2 ] ++ vs ++ "")
                      then
                        match
                          isPrefix
                            eqChar
                            "#"
                            n1
                        with
                          true
                        then
                          work2
                            ns4
                            ev
                            vv
                        else
                          let #var"59" =
                            padPrint
                              n1
                              14
                          in
                          let #var"60" =
                            padPrint
                              (float2string
                                 e1)
                              pad
                          in
                          let #var"61" =
                            padPrint
                              (float2string
                                 v2)
                              pad
                          in
                          let #var"62" =
                            padPrint
                              (float2string
                                 (sqrt
                                    v2))
                              pad
                          in
                          let #var"63" =
                            print
                              "\n"
                          in
                          work2
                            ns4
                            es1
                            vs
                      else
                        {}
            in
            let #var"57" =
              work2
                names2
                expVals2
                varianceVals1
            in
            let #var"58" =
              print
                "\n"
            in
            print
              (join
                 [ "Normalization constant: ",
                   float2string
                     normConst3,
                   "\n" ])
in
let systematicSample: all a50. [a50] -> [Float] -> Float -> Int -> [a50] =
  lam seq.
    lam weights7.
      lam weightSum.
        lam sampleCount.
          let step =
            divf
              weightSum
              (int2float
                 sampleCount)
          in
          recursive
            let systematicSampleRec =
              lam seq1.
                lam weights8.
                  lam u.
                    lam out1.
                      match
                        null
                          weights8
                      with
                        true
                      then
                        out1
                      else
                        match
                          ltf
                            u
                            (head
                               weights8)
                        with
                          true
                        then
                          systematicSampleRec
                            seq1
                            weights8
                            (addf
                               u
                               step)
                            (cons
                               (head
                                  seq1)
                               out1)
                        else
                          systematicSampleRec
                            (tail
                               seq1)
                            (tail
                               weights8)
                            (subf
                               u
                               (head
                                  weights8))
                            out1
          in
          systematicSampleRec
            seq
            weights7
            (uniformContinuousSample
               0.
               step)
            (toList
               "")
in
let normConstant: [Float] -> Float =
  lam res4.
    let max1 =
      foldl
        (lam acc21.
           lam x10.
             match
               geqf
                 x10
                 acc21
             with
               true
             then
               x10
             else
               acc21)
        negInf
        res4
    in
    match
      eqf
        max1
        negInf
    with
      true
    then
      negInf
    else
      let sum1 =
        foldl
          (lam acc20.
             lam x9.
               addf
                 (exp
                    (subf
                       x9
                       max1))
                 acc20)
          0.
          res4
      in
      subf
        (addf
           max1
           (log
              sum1))
        (log
           (int2float
              (length
                 res4)))
in
let expectedValues =
  lam res3: [[Float]].
    lam normConst2.
      foldl
        (lam acc18.
           lam t230.
             let w3 =
               exp
                 (subf
                    (head
                       t230)
                    normConst2)
             in
             let ys =
               tail
                 t230
             in
             recursive
               let work1 =
                 lam acc19.
                   lam xs3.
                     match
                       (acc19, xs3)
                     with
                       ([ a49 ] ++ as1 ++ "", [ x8 ] ++ xs4 ++ "")
                     then
                       cons
                         (addf
                            (mulf
                               x8
                               w3)
                            a49)
                         (work1
                            as1
                            xs4)
                     else
                       ""
             in
             work1
               acc18
               ys)
        (create
           (subi
              (length
                 (head
                    res3))
              1)
           (lam #var"51".
              0.))
        res3
in
let variance =
  lam res2.
    lam expVals1.
      let sum =
        foldl
          (lam acc16.
             lam t229.
               recursive
                 let work =
                   lam acc17.
                     lam xs1.
                       lam expv.
                         match
                           (acc17, xs1, expv)
                         with
                           ([ a48 ] ++ as ++ "", [ x7 ] ++ xs2 ++ "", [ e ] ++ es ++ "")
                         then
                           let v1 =
                             subf
                               x7
                               e
                           in
                           cons
                             (addf
                                a48
                                (mulf
                                   v1
                                   v1))
                             (work
                                as
                                xs2
                                es)
                         else
                           ""
               in
               work
                 acc16
                 (tail
                    t229)
                 expVals1)
          (create
             (subi
                (length
                   (head
                      res2))
                1)
             (lam #var"50".
                0.))
          res2
      in
      let dval =
        int2float
          (length
             res2)
      in
      map
        (lam x6.
           divf
             x6
             dval)
        sum
in
let expOnLogWeights =
  lam res1.
    mapReverse
      (lam t228.
         match
           t228
         with
           [ x5 ] ++ xs ++ ""
         in
         cons
             (exp
                x5)
             xs)
      res1
in
let output =
  lam res: [[Float]].
    lam names: [[Char]].
      let names1 =
        cons
          "#"
          names
      in
      let nc =
        normConstant
          (map
             head
             res)
      in
      let expVals =
        expectedValues
          res
          nc
      in
      let varianceVals =
        variance
          res
          expVals
      in
      let #var"49" =
        printStatistics
          res
          names1
          nc
          expVals
          varianceVals
      in
      saveCSV
        res
        names1
        "data.csv"
        expOnLogWeights
in
let printSamples: all a47. (a47 -> [Char]) -> [Float] -> [a47] -> () =
  lam printFun1.
    lam weights4.
      lam samples10.
        recursive
          let rec6: [Float] -> [a47] -> () =
            lam weights5.
              lam samples11.
                match
                  null
                    weights5
                with
                  true
                then
                  {}
                else
                  let w2 =
                    head
                      weights5
                  in
                  let weights6 =
                    tail
                      weights5
                  in
                  let s18 =
                    head
                      samples11
                  in
                  let samples12 =
                    tail
                      samples11
                  in
                  let #var"45" =
                    print
                      (printFun1
                         s18)
                  in
                  let #var"46" =
                    print
                      " "
                  in
                  let #var"47" =
                    print
                      (float2string
                         w2)
                  in
                  let #var"48" =
                    print
                      "\n"
                  in
                  rec6
                    weights6
                    samples12
        in
        match
          compileOptions.printSamples
        with
          true
        then
          rec6
            weights4
            samples10
        else
          {}
in
let printSamplesOption: all a46. (a46 -> [Char]) -> [Float] -> [Option a46] -> () =
  lam printFun.
    lam weights1.
      lam samples7.
        recursive
          let rec5: [Float] -> [Option a46] -> () =
            lam weights2.
              lam samples8.
                match
                  null
                    weights2
                with
                  true
                then
                  {}
                else
                  let w1 =
                    head
                      weights2
                  in
                  let weights3 =
                    tail
                      weights2
                  in
                  let s16 =
                    head
                      samples8
                  in
                  let samples9 =
                    tail
                      samples8
                  in
                  let #var"41" =
                    match
                      s16
                    with
                      Some s17
                    then
                      print
                        (printFun
                           s17)
                    else
                      print
                        "."
                  in
                  let #var"42" =
                    print
                      " "
                  in
                  let #var"43" =
                    print
                      (float2string
                         w1)
                  in
                  let #var"44" =
                    print
                      "\n"
                  in
                  rec5
                    weights3
                    samples9
        in
        match
          compileOptions.printSamples
        with
          true
        then
          rec5
            weights1
            samples7
        else
          {}
in
let _mcmcAccepts =
  ref
    0
in
let _mcmcSamples =
  ref
    (negi
       1)
in
let mcmcAcceptInit =
  lam n.
    let #var"40" =
      modref
        _mcmcSamples
        n
    in
    modref
      _mcmcAccepts
      0
in
let mcmcAccept =
  lam #var"39".
    modref
      _mcmcAccepts
      (addi
         (deref
            _mcmcAccepts)
         1)
in
let mcmcAcceptRate =
  lam #var"38".
    divf
      (int2float
         (deref
            _mcmcAccepts))
      (int2float
         (deref
            _mcmcSamples))
in
recursive
  let #var"RuntimeDistBase_sample": all a44. Dist a44 -> a44 =
    lam __sem_target20.
      let _20 =
        dprint
          __sem_target20
      in
      error
        "No matching case for function sample"
  let #var"RuntimeDistBase_logObserve": all a45. Dist a45 -> a45 -> Float =
    lam __sem_target21.
      let _21 =
        dprint
          __sem_target21
      in
      error
        "No matching case for function logObserve"
in
con RuntimeDistElementary_DistGamma: all a5. {scale: Float, shape: Float} -> Dist a5 in
con RuntimeDistElementary_DistExponential: all a6. {rate: Float} -> Dist a6 in
con RuntimeDistElementary_DistPoisson: all a7. {lambda: Float} -> Dist a7 in
con RuntimeDistElementary_DistBinomial: all a8. {n: Int, p: Float} -> Dist a8 in
con RuntimeDistElementary_DistBernoulli: all a9. {p: Float} -> Dist a9 in
con RuntimeDistElementary_DistBeta: all a10. {a: Float, b: Float} -> Dist a10 in
con RuntimeDistElementary_DistGaussian: all a11. {mu: Float, sigma: Float} -> Dist a11 in
con RuntimeDistElementary_DistMultinomial: all a12. {n: Int, p: [Float]} -> Dist a12 in
con RuntimeDistElementary_DistCategorical: all a13. {p: [Float]} -> Dist a13 in
con RuntimeDistElementary_DistDirichlet: all a14. {a: [Float]} -> Dist a14 in
con RuntimeDistElementary_DistUniform: all a15. {a: Float, b: Float} -> Dist a15 in
recursive
  let #var"RuntimeDistElementary_sample": all a42. Dist a42 -> a42 =
    lam __sem_target18.
      match
        __sem_target18
      with
        RuntimeDistElementary_DistGamma t206
      then
        unsafeCoerce
          (gammaSample
             (t206.shape)
             (t206.scale))
      else
        match
          __sem_target18
        with
          RuntimeDistElementary_DistExponential t207
        then
          unsafeCoerce
            (exponentialSample
               (t207.rate))
        else
          match
            __sem_target18
          with
            RuntimeDistElementary_DistPoisson t208
          then
            unsafeCoerce
              (poissonSample
                 (t208.lambda))
          else
            match
              __sem_target18
            with
              RuntimeDistElementary_DistBinomial t209
            then
              unsafeCoerce
                (binomialSample
                   (t209.p)
                   (t209.n))
            else
              match
                __sem_target18
              with
                RuntimeDistElementary_DistBernoulli t210
              then
                unsafeCoerce
                  (bernoulliSample
                     (t210.p))
              else
                match
                  __sem_target18
                with
                  RuntimeDistElementary_DistBeta t211
                then
                  unsafeCoerce
                    (betaSample
                       (t211.a)
                       (t211.b))
                else
                  match
                    __sem_target18
                  with
                    RuntimeDistElementary_DistGaussian t212
                  then
                    unsafeCoerce
                      (gaussianSample
                         (t212.mu)
                         (t212.sigma))
                  else
                    match
                      __sem_target18
                    with
                      RuntimeDistElementary_DistMultinomial t213
                    then
                      unsafeCoerce
                        (multinomialSample
                           (t213.p)
                           (t213.n))
                    else
                      match
                        __sem_target18
                      with
                        RuntimeDistElementary_DistCategorical t214
                      then
                        unsafeCoerce
                          (categoricalSample
                             (t214.p))
                      else
                        match
                          __sem_target18
                        with
                          RuntimeDistElementary_DistDirichlet t215
                        then
                          unsafeCoerce
                            (dirichletSample
                               (t215.a))
                        else
                          match
                            __sem_target18
                          with
                            RuntimeDistElementary_DistUniform t216
                          then
                            unsafeCoerce
                              (uniformContinuousSample
                                 (t216.a)
                                 (t216.b))
                          else
                            let _18 =
                              dprint
                                __sem_target18
                            in
                            error
                              "No matching case for function sample"
  let #var"RuntimeDistElementary_logObserve": all a43. Dist a43 -> a43 -> Float =
    lam __sem_target19.
      match
        __sem_target19
      with
        RuntimeDistElementary_DistGamma t217
      then
        unsafeCoerce
          (gammaLogPdf
             (t217.shape)
             (t217.scale))
      else
        match
          __sem_target19
        with
          RuntimeDistElementary_DistExponential t218
        then
          unsafeCoerce
            (exponentialLogPdf
               (t218.rate))
        else
          match
            __sem_target19
          with
            RuntimeDistElementary_DistPoisson t219
          then
            unsafeCoerce
              (poissonLogPmf
                 (t219.lambda))
          else
            match
              __sem_target19
            with
              RuntimeDistElementary_DistBinomial t220
            then
              unsafeCoerce
                (binomialLogPmf
                   (t220.p)
                   (t220.n))
            else
              match
                __sem_target19
              with
                RuntimeDistElementary_DistBernoulli t221
              then
                unsafeCoerce
                  (bernoulliLogPmf
                     (t221.p))
              else
                match
                  __sem_target19
                with
                  RuntimeDistElementary_DistBeta t222
                then
                  unsafeCoerce
                    (betaLogPdf
                       (t222.a)
                       (t222.b))
                else
                  match
                    __sem_target19
                  with
                    RuntimeDistElementary_DistGaussian t223
                  then
                    unsafeCoerce
                      (gaussianLogPdf
                         (t223.mu)
                         (t223.sigma))
                  else
                    match
                      __sem_target19
                    with
                      RuntimeDistElementary_DistMultinomial t224
                    then
                      unsafeCoerce
                        (lam o1.
                           match
                             eqi
                               (t224.n)
                               (foldl1
                                  addi
                                  o1)
                           with
                             true
                           then
                             multinomialLogPmf
                               (t224.p)
                               o1
                           else
                             negf
                               inf)
                    else
                      match
                        __sem_target19
                      with
                        RuntimeDistElementary_DistCategorical t225
                      then
                        unsafeCoerce
                          (categoricalLogPmf
                             (t225.p))
                      else
                        match
                          __sem_target19
                        with
                          RuntimeDistElementary_DistDirichlet t226
                        then
                          unsafeCoerce
                            (dirichletLogPdf
                               (t226.a))
                        else
                          match
                            __sem_target19
                          with
                            RuntimeDistElementary_DistUniform t227
                          then
                            unsafeCoerce
                              (uniformContinuousLogPdf
                                 (t227.a)
                                 (t227.b))
                          else
                            let _19 =
                              dprint
                                __sem_target19
                            in
                            error
                              "No matching case for function logObserve"
in
con RuntimeDistEmpirical_EmpNorm: {normConst: Float} -> EmpiricalExtra in
con RuntimeDistEmpirical_EmpMCMC: {acceptRate: Float} -> EmpiricalExtra in
con RuntimeDistEmpirical_DistEmpirical: all a16. {extra: EmpiricalExtra, samples: [a16], degenerate: Bool, logWeights: [Float], cumulativeWeights: [Float]} -> Dist a16 in
recursive
  let #var"RuntimeDistEmpirical_sample": all a40. Dist a40 -> a40 =
    lam __sem_target10.
      match
        __sem_target10
      with
        RuntimeDistEmpirical_DistEmpirical t200
      then
        let x3 =
          uniformContinuousSample
            0.
            (last
               (t200.cumulativeWeights))
        in
        let cmp1 =
          lam y1.
            match
              ltf
                (subf
                   y1
                   x3)
                0.
            with
              true
            then
              negi
                1
            else
              0
        in
        match
          lowerBoundBinarySearch
            cmp1
            (t200.cumulativeWeights)
        with
          Some idx1
        then
          unsafeCoerce
            (get
               (t200.samples)
               idx1)
        else
          error
            "Sampling from empirical distribution failed"
      else
        let _10 =
          dprint
            __sem_target10
        in
        error
          "No matching case for function sample"
  let #var"RuntimeDistEmpirical_logObserve": all a41. Dist a41 -> a41 -> Float =
    lam __sem_target11.
      match
        __sem_target11
      with
        RuntimeDistEmpirical_DistEmpirical t201
      then
        error
          "Log observe not supported for empirical distribution"
      else
        let _11 =
          dprint
            __sem_target11
        in
        error
          "No matching case for function logObserve"
  let #var"RuntimeDistEmpirical_empiricalSamples" =
    lam __sem_target12.
      match
        __sem_target12
      with
        RuntimeDistEmpirical_DistEmpirical t202
      then
        (t202.samples, t202.logWeights)
      else
        match
          __sem_target12
        with
          _
        then
          ("", "")
        else
          let _12 =
            dprint
              __sem_target12
          in
          error
            "No matching case for function empiricalSamples"
  let #var"RuntimeDistEmpirical_empiricalNormConst" =
    lam __sem_target13.
      match
        __sem_target13
      with
        RuntimeDistEmpirical_DistEmpirical t203
      then
        match
          t203.extra
        with
          RuntimeDistEmpirical_EmpNorm {normConst = normConst1}
        then
          normConst1
        else
          nan
      else
        match
          __sem_target13
        with
          _
        then
          nan
        else
          let _13 =
            dprint
              __sem_target13
          in
          error
            "No matching case for function empiricalNormConst"
  let #var"RuntimeDistEmpirical_empiricalAcceptRate" =
    lam __sem_target14.
      match
        __sem_target14
      with
        RuntimeDistEmpirical_DistEmpirical t204
      then
        match
          t204.extra
        with
          RuntimeDistEmpirical_EmpMCMC {acceptRate = acceptRate1}
        then
          acceptRate1
        else
          nan
      else
        match
          __sem_target14
        with
          _
        then
          nan
        else
          let _14 =
            dprint
              __sem_target14
          in
          error
            "No matching case for function empiricalAcceptRate"
  let #var"RuntimeDistEmpirical_empiricalDegenerate" =
    lam __sem_target15.
      match
        __sem_target15
      with
        RuntimeDistEmpirical_DistEmpirical t205
      then
        t205.degenerate
      else
        match
          __sem_target15
        with
          _
        then
          false
        else
          let _15 =
            dprint
              __sem_target15
          in
          error
            "No matching case for function empiricalDegenerate"
  let #var"RuntimeDistEmpirical_constructDistEmpirical" =
    lam samples4.
      lam logWeights3.
        lam __sem_target16.
          match
            __sem_target16
          with
            extra2
          then
            let maxLogWeight1 =
              foldl
                (lam acc15.
                   lam lw6.
                     match
                       geqf
                         lw6
                         acc15
                     with
                       true
                     then
                       lw6
                     else
                       acc15)
                (negf
                   inf)
                logWeights3
            in
            let degenerate1 =
              eqf
                maxLogWeight1
                (negf
                   inf)
            in
            let lse1 =
              addf
                maxLogWeight1
                (log
                   (foldl
                      (lam acc14.
                         lam lw5.
                           addf
                             acc14
                             (exp
                                (subf
                                   lw5
                                   maxLogWeight1)))
                      0.
                      logWeights3))
            in
            let logWeights4 =
              map
                (lam lw4.
                   subf
                     lw4
                     lse1)
                logWeights3
            in
            let f8 =
              lam acc12.
                lam x4.
                  let acc13 =
                    addf
                      acc12
                      (exp
                         x4)
                  in
                  (acc13, acc13)
            in
            match
              mapAccumL
                f8
                0.
                logWeights4
            with
              (_, cumulativeWeights1)
            in
            RuntimeDistEmpirical_DistEmpirical
                { extra =
                    extra2,
                  samples =
                    samples4,
                  degenerate =
                    degenerate1,
                  logWeights =
                    logWeights4,
                  cumulativeWeights =
                    cumulativeWeights1 }
          else
            let _16 =
              dprint
                __sem_target16
            in
            error
              "No matching case for function constructDistEmpirical"
  let #var"RuntimeDistEmpirical_constructDistEmpiricalHelper" =
    lam __sem_target17.
      match
        __sem_target17
      with
        samples5
      then
        match
          unzip
            samples5
        with
          (logWeights5, samples6)
        in
        let extra3 =
            RuntimeDistEmpirical_EmpNorm
              { normConst =
                  0. }
          in
          #var"RuntimeDistEmpirical_constructDistEmpirical"
            samples6
            logWeights5
            extra3
      else
        let _17 =
          dprint
            __sem_target17
        in
        error
          "No matching case for function constructDistEmpiricalHelper"
in
con RuntimeDistCombined_DistCombinedIndependent: all a17. {combined: [Dist a17]} -> Dist a17 in
recursive
  let #var"RuntimeDistCombined_sample" =
    lam __sem_target8.
      match
        __sem_target8
      with
        RuntimeDistCombined_DistCombinedIndependent t198
      then
        unsafeCoerce
          (map
             #var"RuntimeDistCombined_sample"
             (t198.combined))
      else
        let _8 =
          dprint
            __sem_target8
        in
        error
          "No matching case for function sample"
  let #var"RuntimeDistCombined_logObserve" =
    lam __sem_target9.
      match
        __sem_target9
      with
        RuntimeDistCombined_DistCombinedIndependent t199
      then
        unsafeCoerce
          (map
             #var"RuntimeDistCombined_logObserve"
             (t199.combined))
      else
        let _9 =
          dprint
            __sem_target9
        in
        error
          "No matching case for function logObserve"
in
recursive
  let #var"RuntimeDist_sample": all a38. Dist a38 -> a38 =
    lam __sem_target.
      match
        __sem_target
      with
        RuntimeDistCombined_DistCombinedIndependent t168
      then
        unsafeCoerce
          (map
             #var"RuntimeDist_sample"
             (t168.combined))
      else
        match
          __sem_target
        with
          RuntimeDistEmpirical_DistEmpirical t169
        then
          let x1 =
            uniformContinuousSample
              0.
              (last
                 (t169.cumulativeWeights))
          in
          let cmp =
            lam y.
              match
                ltf
                  (subf
                     y
                     x1)
                  0.
              with
                true
              then
                negi
                  1
              else
                0
          in
          match
            lowerBoundBinarySearch
              cmp
              (t169.cumulativeWeights)
          with
            Some idx
          then
            unsafeCoerce
              (get
                 (t169.samples)
                 idx)
          else
            error
              "Sampling from empirical distribution failed"
        else
          match
            __sem_target
          with
            RuntimeDistElementary_DistGamma t170
          then
            unsafeCoerce
              (gammaSample
                 (t170.shape)
                 (t170.scale))
          else
            match
              __sem_target
            with
              RuntimeDistElementary_DistExponential t171
            then
              unsafeCoerce
                (exponentialSample
                   (t171.rate))
            else
              match
                __sem_target
              with
                RuntimeDistElementary_DistPoisson t172
              then
                unsafeCoerce
                  (poissonSample
                     (t172.lambda))
              else
                match
                  __sem_target
                with
                  RuntimeDistElementary_DistBinomial t173
                then
                  unsafeCoerce
                    (binomialSample
                       (t173.p)
                       (t173.n))
                else
                  match
                    __sem_target
                  with
                    RuntimeDistElementary_DistBernoulli t174
                  then
                    unsafeCoerce
                      (bernoulliSample
                         (t174.p))
                  else
                    match
                      __sem_target
                    with
                      RuntimeDistElementary_DistBeta t175
                    then
                      unsafeCoerce
                        (betaSample
                           (t175.a)
                           (t175.b))
                    else
                      match
                        __sem_target
                      with
                        RuntimeDistElementary_DistGaussian t176
                      then
                        unsafeCoerce
                          (gaussianSample
                             (t176.mu)
                             (t176.sigma))
                      else
                        match
                          __sem_target
                        with
                          RuntimeDistElementary_DistMultinomial t177
                        then
                          unsafeCoerce
                            (multinomialSample
                               (t177.p)
                               (t177.n))
                        else
                          match
                            __sem_target
                          with
                            RuntimeDistElementary_DistCategorical t178
                          then
                            unsafeCoerce
                              (categoricalSample
                                 (t178.p))
                          else
                            match
                              __sem_target
                            with
                              RuntimeDistElementary_DistDirichlet t179
                            then
                              unsafeCoerce
                                (dirichletSample
                                   (t179.a))
                            else
                              match
                                __sem_target
                              with
                                RuntimeDistElementary_DistUniform t180
                              then
                                unsafeCoerce
                                  (uniformContinuousSample
                                     (t180.a)
                                     (t180.b))
                              else
                                let #var"_" =
                                  dprint
                                    __sem_target
                                in
                                error
                                  "No matching case for function sample"
  let #var"RuntimeDist_logObserve": all a39. Dist a39 -> a39 -> Float =
    lam __sem_target1.
      match
        __sem_target1
      with
        RuntimeDistCombined_DistCombinedIndependent t181
      then
        unsafeCoerce
          (map
             #var"RuntimeDist_logObserve"
             (t181.combined))
      else
        match
          __sem_target1
        with
          RuntimeDistEmpirical_DistEmpirical t182
        then
          error
            "Log observe not supported for empirical distribution"
        else
          match
            __sem_target1
          with
            RuntimeDistElementary_DistGamma t183
          then
            unsafeCoerce
              (gammaLogPdf
                 (t183.shape)
                 (t183.scale))
          else
            match
              __sem_target1
            with
              RuntimeDistElementary_DistExponential t184
            then
              unsafeCoerce
                (exponentialLogPdf
                   (t184.rate))
            else
              match
                __sem_target1
              with
                RuntimeDistElementary_DistPoisson t185
              then
                unsafeCoerce
                  (poissonLogPmf
                     (t185.lambda))
              else
                match
                  __sem_target1
                with
                  RuntimeDistElementary_DistBinomial t186
                then
                  unsafeCoerce
                    (binomialLogPmf
                       (t186.p)
                       (t186.n))
                else
                  match
                    __sem_target1
                  with
                    RuntimeDistElementary_DistBernoulli t187
                  then
                    unsafeCoerce
                      (bernoulliLogPmf
                         (t187.p))
                  else
                    match
                      __sem_target1
                    with
                      RuntimeDistElementary_DistBeta t188
                    then
                      unsafeCoerce
                        (betaLogPdf
                           (t188.a)
                           (t188.b))
                    else
                      match
                        __sem_target1
                      with
                        RuntimeDistElementary_DistGaussian t189
                      then
                        unsafeCoerce
                          (gaussianLogPdf
                             (t189.mu)
                             (t189.sigma))
                      else
                        match
                          __sem_target1
                        with
                          RuntimeDistElementary_DistMultinomial t190
                        then
                          unsafeCoerce
                            (lam o.
                               match
                                 eqi
                                   (t190.n)
                                   (foldl1
                                      addi
                                      o)
                               with
                                 true
                               then
                                 multinomialLogPmf
                                   (t190.p)
                                   o
                               else
                                 negf
                                   inf)
                        else
                          match
                            __sem_target1
                          with
                            RuntimeDistElementary_DistCategorical t191
                          then
                            unsafeCoerce
                              (categoricalLogPmf
                                 (t191.p))
                          else
                            match
                              __sem_target1
                            with
                              RuntimeDistElementary_DistDirichlet t192
                            then
                              unsafeCoerce
                                (dirichletLogPdf
                                   (t192.a))
                            else
                              match
                                __sem_target1
                              with
                                RuntimeDistElementary_DistUniform t193
                              then
                                unsafeCoerce
                                  (uniformContinuousLogPdf
                                     (t193.a)
                                     (t193.b))
                              else
                                let _1 =
                                  dprint
                                    __sem_target1
                                in
                                error
                                  "No matching case for function logObserve"
  let #var"RuntimeDist_empiricalSamples" =
    lam __sem_target2.
      match
        __sem_target2
      with
        RuntimeDistEmpirical_DistEmpirical t194
      then
        (t194.samples, t194.logWeights)
      else
        match
          __sem_target2
        with
          _
        then
          ("", "")
        else
          let _2 =
            dprint
              __sem_target2
          in
          error
            "No matching case for function empiricalSamples"
  let #var"RuntimeDist_empiricalNormConst" =
    lam __sem_target3.
      match
        __sem_target3
      with
        RuntimeDistEmpirical_DistEmpirical t195
      then
        match
          t195.extra
        with
          RuntimeDistEmpirical_EmpNorm {normConst = normConst}
        then
          normConst
        else
          nan
      else
        match
          __sem_target3
        with
          _
        then
          nan
        else
          let _3 =
            dprint
              __sem_target3
          in
          error
            "No matching case for function empiricalNormConst"
  let #var"RuntimeDist_empiricalAcceptRate" =
    lam __sem_target4.
      match
        __sem_target4
      with
        RuntimeDistEmpirical_DistEmpirical t196
      then
        match
          t196.extra
        with
          RuntimeDistEmpirical_EmpMCMC {acceptRate = acceptRate}
        then
          acceptRate
        else
          nan
      else
        match
          __sem_target4
        with
          _
        then
          nan
        else
          let _4 =
            dprint
              __sem_target4
          in
          error
            "No matching case for function empiricalAcceptRate"
  let #var"RuntimeDist_empiricalDegenerate" =
    lam __sem_target5.
      match
        __sem_target5
      with
        RuntimeDistEmpirical_DistEmpirical t197
      then
        t197.degenerate
      else
        match
          __sem_target5
        with
          _
        then
          false
        else
          let _5 =
            dprint
              __sem_target5
          in
          error
            "No matching case for function empiricalDegenerate"
  let #var"RuntimeDist_constructDistEmpirical" =
    lam samples1.
      lam logWeights.
        lam __sem_target6.
          match
            __sem_target6
          with
            extra
          then
            let maxLogWeight =
              foldl
                (lam acc11.
                   lam lw3.
                     match
                       geqf
                         lw3
                         acc11
                     with
                       true
                     then
                       lw3
                     else
                       acc11)
                (negf
                   inf)
                logWeights
            in
            let degenerate =
              eqf
                maxLogWeight
                (negf
                   inf)
            in
            let lse =
              addf
                maxLogWeight
                (log
                   (foldl
                      (lam acc10.
                         lam lw2.
                           addf
                             acc10
                             (exp
                                (subf
                                   lw2
                                   maxLogWeight)))
                      0.
                      logWeights))
            in
            let logWeights1 =
              map
                (lam lw1.
                   subf
                     lw1
                     lse)
                logWeights
            in
            let f7 =
              lam acc8.
                lam x2.
                  let acc9 =
                    addf
                      acc8
                      (exp
                         x2)
                  in
                  (acc9, acc9)
            in
            match
              mapAccumL
                f7
                0.
                logWeights1
            with
              (_, cumulativeWeights)
            in
            RuntimeDistEmpirical_DistEmpirical
                { extra =
                    extra,
                  samples =
                    samples1,
                  degenerate =
                    degenerate,
                  logWeights =
                    logWeights1,
                  cumulativeWeights =
                    cumulativeWeights }
          else
            let _6 =
              dprint
                __sem_target6
            in
            error
              "No matching case for function constructDistEmpirical"
  let #var"RuntimeDist_constructDistEmpiricalHelper" =
    lam __sem_target7.
      match
        __sem_target7
      with
        samples2
      then
        match
          unzip
            samples2
        with
          (logWeights2, samples3)
        in
        let extra1 =
            RuntimeDistEmpirical_EmpNorm
              { normConst =
                  0. }
          in
          #var"RuntimeDist_constructDistEmpirical"
            samples3
            logWeights2
            extra1
      else
        let _7 =
          dprint
            __sem_target7
        in
        error
          "No matching case for function constructDistEmpiricalHelper"
in
let distEmpiricalSamples: all a37. Dist a37 -> ([a37], [Float]) =
  #var"RuntimeDist_empiricalSamples"
in
let distEmpiricalDegenerate: all a36. Dist a36 -> Bool =
  #var"RuntimeDist_empiricalDegenerate"
in
let distEmpiricalNormConst: all a35. Dist a35 -> Float =
  #var"RuntimeDist_empiricalNormConst"
in
let distEmpiricalAcceptRate: all a34. Dist a34 -> Float =
  #var"RuntimeDist_empiricalAcceptRate"
in
let distCombineIndependent: all a33. [Dist a33] -> Dist a33 =
  lam dists.
    RuntimeDistCombined_DistCombinedIndependent
      { combined =
          dists }
in
let sample: all a32. Dist a32 -> a32 =
  #var"RuntimeDist_sample"
in
let logObserve: all a31. Dist a31 -> a31 -> Float =
  #var"RuntimeDist_logObserve"
in
type Checkpoint a18
in
con Resample: all a19. {k: () -> Checkpoint a19} -> Checkpoint a19 in
con End: all a20. a20 -> Checkpoint a20 in
type State =
  Ref Float
in
let resample =
  lam k1.
    Resample
      { k =
          k1 }
in
let updateWeight =
  lam weight1.
    lam state2.
      modref
        state2
        (addf
           (deref
              state2)
           weight1)
in
let stopFirstAssume =
  lam dist.
    lam cont4.
      (Some
        dist, cont4)
in
let stopInit =
  lam cont3.
    (None
      {}, cont3)
in
let run: all a28. all b2. Unknown -> (State -> (Option (Dist b2), b2 -> Checkpoint a28)) -> Dist a28 =
  lam config.
    lam model.
      let particleCount =
        config.particles
      in
      let logParticleCount =
        log
          (int2float
             particleCount)
      in
      let state1 =
        ref
          0.
      in
      type Stop a29 =
        {weight: Float, checkpoint: Checkpoint a29}
      in
      let start: (b2 -> Checkpoint a28) -> Float -> (() -> b2) -> Int -> Stop a28 =
        lam cont2.
          lam weight.
            lam sampleFun.
              lam #var"36".
                let #var"37" =
                  modref
                    state1
                    weight
                in
                let checkpoint1: Checkpoint a28 =
                  cont2
                    (sampleFun
                       {})
                in
                { weight =
                    deref
                      state1,
                  checkpoint =
                    checkpoint1 }
      in
      let propagate =
        lam particle.
          lam contWeight1.
            let #var"35" =
              modref
                state1
                contWeight1
            in
            match
              particle.checkpoint
            with
              Resample {k = k}
            then
              let checkpoint =
                k
                  {}
              in
              { weight =
                  deref
                    state1,
                checkpoint =
                  checkpoint }
            else
              match
                particle.checkpoint
              with
                End _
              in
              particle
      in
      recursive
        let runRec =
          lam particles1.
            match
              forAll
                (lam p.
                   match
                     p.checkpoint
                   with
                     End _
                   then
                     true
                   else
                     false)
                particles1
            with
              true
            then
              unzip
                (mapReverse
                   (lam p1.
                      (p1.weight, match
                        p1.checkpoint
                      with
                        End a30
                      in
                      a30))
                   particles1)
            else
              let maxWeight =
                foldl
                  (lam acc7.
                     lam p5.
                       match
                         geqf
                           (p5.weight)
                           acc7
                       with
                         true
                       then
                         p5.weight
                       else
                         acc7)
                  (negf
                     inf)
                  particles1
              in
              let expWeights =
                reverse
                  (mapReverse
                     (lam p4.
                        exp
                          (subf
                             (p4.weight)
                             maxWeight))
                     particles1)
              in
              let sums =
                foldl
                  (lam acc6.
                     lam w.
                       (addf
                         (acc6.0)
                         w, addf
                         (acc6.1)
                         (mulf
                            w
                            w)))
                  (0., 0.)
                  expWeights
              in
              let ess =
                divf
                  (mulf
                     (sums.0)
                     (sums.0))
                  (sums.1)
              in
              match
                ltf
                  (mulf
                     0.7
                     (int2float
                        particleCount))
                  ess
              with
                true
              then
                let particles2 =
                  mapReverse
                    (lam p2.
                       propagate
                         p2
                         (p2.weight))
                    particles1
                in
                runRec
                  particles2
              else
                let contWeight =
                  subf
                    (addf
                       maxWeight
                       (log
                          (sums.0)))
                    logParticleCount
                in
                let resampled =
                  systematicSample
                    particles1
                    expWeights
                    (sums.0)
                    particleCount
                in
                let particles3 =
                  mapReverse
                    (lam p3.
                       propagate
                         p3
                         contWeight)
                    resampled
                in
                runRec
                  particles3
      in
      match
        model
          state1
      with
        (d1, cont1)
      in
      let particles: [Stop a28] =
          match
            d1
          with
            Some d2
          then
            match
              d2
            with
              RuntimeDistEmpirical_DistEmpirical r1
            then
              match
                eqi
                  particleCount
                  (length
                     (r1.samples))
              with
                true
              then
                foldl2
                  (lam acc5.
                     lam s15.
                       lam lw.
                         cons
                           (start
                              cont1
                              lw
                              (lam #var"31".
                                 s15)
                              0)
                           acc5)
                  (toList
                     "")
                  (r1.samples)
                  (r1.logWeights)
              else
                createList
                  particleCount
                  (start
                     cont1
                     0.
                     (lam #var"32".
                        #var"RuntimeDist_sample"
                          d2))
            else
              createList
                particleCount
                (start
                   cont1
                   0.
                   (lam #var"33".
                      #var"RuntimeDist_sample"
                        d2))
          else
            createList
              particleCount
              (start
                 cont1
                 0.
                 (lam #var"34".
                    unsafeCoerce
                      {}))
        in
        match
          runRec
            particles
        with
          (weights, samples)
        in
        #var"RuntimeDist_constructDistEmpirical"
            samples
            weights
            (RuntimeDistEmpirical_EmpNorm
               { normConst =
                   normConstant
                     weights })
in
type Signal =
  Int
in
external setSigintHandler : (Signal -> ()) -> ()
in
type Timespec =
  (Int, Int)
in
external getMonotonicTime : () -> Timespec
in
external getWallClockTime : () -> Timespec
in
external clockNanosleep : Timespec -> ()
in
external setMaxPriority : () -> Int
in
external setPriority : Int -> Int
in
external externalOpenFileNonblocking : [Char] -> Int
in
external externalCloseFileDescriptor : Int -> ()
in
type Opaque
in
external externalReadFloatPipe : Int -> [(Timespec, Float)]
in
external externalWriteDistFloatRecordPipe : Int -> Int -> (Timespec, ([Opaque], [Float])) -> ()
in
let nanosPerSec =
  1000000000
in
let nanosToTimespec =
  lam nanosPerSec17.
    lam nanos.
      let s14 =
        divi
          nanos
          nanosPerSec17
      in
      let ns3 =
        modi
          nanos
          nanosPerSec17
      in
      (s14, ns3)
in
let timespecToNanos =
  lam nanosPerSec11.
    lam ts.
      match
        ts
      with
        (s1, ns)
      in
      addi
          (muli
             s1
             nanosPerSec11)
          ns
in
let addTimespec =
  lam nanosPerSec16.
    lam lhs2.
      lam rhs2.
        match
          (lhs2, rhs2)
        with
          ((ls2, lns2), (rs2, rns2))
        in
        let s13 =
            addi
              ls2
              rs2
          in
          let ns2 =
            addi
              lns2
              rns2
          in
          match
            geqi
              ns2
              nanosPerSec16
          with
            true
          then
            (addi
              s13
              1, subi
              ns2
              nanosPerSec16)
          else
            (s13, ns2)
in
let diffTimespec =
  lam nanosPerSec12.
    lam lhs.
      lam rhs.
        match
          (lhs, rhs)
        with
          ((ls, lns), (rs, rns))
        in
        let s2 =
            subi
              ls
              rs
          in
          let ns1 =
            subi
              lns
              rns
          in
          match
            lti
              ns1
              0
          with
            true
          then
            (subi
              s2
              1, addi
              ns1
              nanosPerSec12)
          else
            (s2, ns1)
in
let cmpTimespec: Timespec -> Timespec -> Int =
  lam lhs1.
    lam rhs1.
      match
        (lhs1, rhs1)
      with
        ((ls1, lns1), (rs1, rns1))
      in
      match
          gti
            ls1
            rs1
        with
          true
        then
          1
        else
          match
            lti
              ls1
              rs1
          with
            true
          then
            negi
              1
          else
            match
              gti
                lns1
                rns1
            with
              true
            then
              1
            else
              match
                lti
                  lns1
                  rns1
              with
                true
              then
                negi
                  1
              else
                0
in
let monoLogicalTime: Ref (Timespec) =
  ref
    (0, 0)
in
let wallLogicalTime: Ref (Timespec) =
  ref
    (0, 0)
in
let delayBy =
  lam nanosPerSec15.
    lam monoLogicalTime6.
      lam wallLogicalTime14.
        lam delay1.
          let oldPriority =
            setMaxPriority
              {}
          in
          let intervalTime =
            nanosToTimespec
              nanosPerSec15
              delay1
          in
          let endTime =
            getMonotonicTime
              {}
          in
          let elapsedTime =
            diffTimespec
              nanosPerSec15
              endTime
              (deref
                 monoLogicalTime6)
          in
          let waitTime =
            addTimespec
              nanosPerSec15
              (deref
                 monoLogicalTime6)
              intervalTime
          in
          let overrun1 =
            let c =
              cmpTimespec
                intervalTime
                elapsedTime
            in
            match
              gti
                c
                0
            with
              true
            then
              let #var"30" =
                clockNanosleep
                  waitTime
              in
              0
            else
              match
                lti
                  c
                  0
              with
                true
              then
                let elapsedTime1 =
                  diffTimespec
                    nanosPerSec15
                    endTime
                    waitTime
                in
                timespecToNanos
                  nanosPerSec15
                  elapsedTime1
              else
                0
          in
          let #var"27" =
            modref
              monoLogicalTime6
              waitTime
          in
          let #var"28" =
            modref
              wallLogicalTime14
              (addTimespec
                 nanosPerSec15
                 (deref
                    wallLogicalTime14)
                 intervalTime)
          in
          let #var"29" =
            setPriority
              oldPriority
          in
          overrun1
in
type TSV a21 =
  (Timespec, a21)
in
let timestamp =
  lam nanosPerSec10.
    lam wallLogicalTime10.
      lam tsv2.
        let lt =
          deref
            wallLogicalTime10
        in
        timespecToNanos
          nanosPerSec10
          (diffTimespec
             nanosPerSec10
             (tsv2.0)
             lt)
in
let value: TSV Unknown -> Unknown =
  lam tsv3.
    tsv3.1
in
let tsv =
  lam nanosPerSec14.
    lam wallLogicalTime13.
      lam offset.
        lam value1.
          let lt1 =
            deref
              wallLogicalTime13
          in
          (addTimespec
            nanosPerSec14
            lt1
            (nanosToTimespec
               nanosPerSec14
               offset), value1)
in
let sdelay =
  lam nanosPerSec13.
    lam monoLogicalTime5.
      lam wallLogicalTime12.
        lam flushOutputs1.
          lam updateInputs1.
            lam delay.
              let #var"25" =
                flushOutputs1
                  {}
              in
              let overrun =
                delayBy
                  nanosPerSec13
                  monoLogicalTime5
                  wallLogicalTime12
                  delay
              in
              let #var"26" =
                updateInputs1
                  {}
              in
              overrun
in
let openFileDescriptor: [Char] -> Int =
  lam file.
    externalOpenFileNonblocking
      file
in
let closeFileDescriptor: Int -> () =
  lam fd3.
    externalCloseFileDescriptor
      fd3
in
let rtpplReadFloatPort =
  lam fd2.
    externalReadFloatPipe
      fd2
in
let t =
  lam fd1.
    lam nfields1.
      lam msg.
        externalWriteDistFloatRecordPipe
          fd1
          nfields1
          msg
in
let rtpplWriteDistFloatRecordPort =
  lam fd.
    lam nfields.
      lam msgs.
        iter
          (t
             fd
             nfields)
          msgs
in
let t1 =
  lam closeFileDescriptors2.
    lam #var"23".
      let #var"24" =
        closeFileDescriptors2
          {}
      in
      exit
        0
in
let rtpplRuntimeInit =
  lam monoLogicalTime4.
    lam wallLogicalTime11.
      lam updateInputSequences.
        lam closeFileDescriptors1.
          lam cont.
            let #var"18" =
              setSigintHandler
                (t1
                   closeFileDescriptors1)
            in
            let #var"19" =
              modref
                monoLogicalTime4
                (getMonotonicTime
                   {})
            in
            let #var"20" =
              modref
                wallLogicalTime11
                (getWallClockTime
                   {})
            in
            let #var"21" =
              updateInputSequences
                {}
            in
            let #var"22" =
              cont
                {}
            in
            {}
in
let fileDescriptors =
  { speed =
      openFileDescriptor
        "speedEst-speed",
    lspeed =
      openFileDescriptor
        "speedEst-lspeed",
    rspeed =
      openFileDescriptor
        "speedEst-rspeed" }
in
let closeFileDescriptors =
  lam fileDescriptors6.
    lam #var"17".
      let close_lspeed =
        closeFileDescriptor
          (fileDescriptors6.lspeed)
      in
      let close_rspeed =
        closeFileDescriptor
          (fileDescriptors6.rspeed)
      in
      let close_speed =
        closeFileDescriptor
          (fileDescriptors6.speed)
      in
      {}
in
let inputSeqs =
  ref
    { lspeed =
        "",
      rspeed =
        "" }
in
let outputSeqs =
  ref
    { speed =
        "" }
in
let updateInputs =
  lam fileDescriptors5.
    lam inputSeqs4.
      lam #var"16".
        modref
          inputSeqs4
          { lspeed =
              rtpplReadFloatPort
                (fileDescriptors5.lspeed),
            rspeed =
              rtpplReadFloatPort
                (fileDescriptors5.rspeed) }
in
let t2 =
  lam tsv4.
    match
      tsv4
    with
      (ts3, v)
    in
    (ts3, unsafeCoerce
        (distEmpiricalSamples
           v))
in
let flushOutputs =
  lam fileDescriptors4.
    lam outputSeqs4.
      lam #var"14".
        let w_speed =
          rtpplWriteDistFloatRecordPort
            (fileDescriptors4.speed)
            2
            (map
               t2
               ((deref
                   outputSeqs4).speed))
        in
        let #var"15" =
          modref
            outputSeqs4
            { speed =
                "" }
        in
        {}
in
let gtInt =
  gti
in
let intToFloat =
  int2float
in
let concat: [Unknown] -> [Unknown] -> [Unknown] =
  lam l1.
    lam r.
      concat
        l1
        r
in
type Line =
  {slope: Float, intercept: Float}
in
let maxSpeed =
  0.5
in
let wheelCircumference =
  0.35
in
let timestampToSeconds =
  lam intToFloat8.
    lam ts1.
      divf
        (intToFloat8
           ts1)
        (intToFloat8
           1000000000)
in
let t3 =
  lam nanosPerSec9.
    lam wallLogicalTime9.
      lam intToFloat7.
        lam wheelCircumference7.
          lam m1.
            lam b1.
              lam sigma1.
                lam #var"10".
                  lam tsv1.
                    let ts2 =
                      timestampToSeconds
                        intToFloat7
                        (timestamp
                           nanosPerSec9
                           wallLogicalTime9
                           tsv1)
                    in
                    let rpm =
                      value
                        tsv1
                    in
                    let mps =
                      divf
                        (mulf
                           rpm
                           wheelCircumference7)
                        60.
                    in
                    let #var"11": () =
                      error
                        "Cannot use observe outside of inferred model"
                    in
                    {}
in
let speedModel =
  lam nanosPerSec8.
    lam wallLogicalTime8.
      lam intToFloat6.
        lam maxSpeed6.
          lam wheelCircumference6.
            lam speedObs.
              let m =
                error
                  "Cannot use assume outside of inferred model"
              in
              let b =
                error
                  "Cannot use assume outside of inferred model"
              in
              let sigma =
                error
                  "Cannot use assume outside of inferred model"
              in
              let #var"9" =
                foldl
                  (t3
                     nanosPerSec8
                     wallLogicalTime8
                     intToFloat6
                     wheelCircumference6
                     m
                     b
                     sigma)
                  {}
                  speedObs
              in
              { slope =
                  m,
                intercept =
                  b }
in
let t4 =
  lam obs2: [((Int, Int), Float)].
    lam nanosPerSec7: Int.
      lam wallLogicalTime7: Ref (Timespec).
        lam intToFloat5: Int -> Float.
          lam maxSpeed5: Float.
            lam wheelCircumference5: Float.
              lam state: State.
                stopInit
                  (lam #var"8".
                     let map =
                       lam f6.
                         let t154 =
                           lam s11.
                             recursive
                               let rec4 =
                                 lam s12.
                                   let t156 =
                                     match
                                       s12
                                     with
                                       ""
                                     then
                                       let t157 =
                                         ""
                                       in
                                       t157
                                     else
                                       let t158 =
                                         match
                                           s12
                                         with
                                           [ a26 ]
                                         then
                                           let t159 =
                                             f6
                                               a26
                                           in
                                           let t160 =
                                             [ t159 ]
                                           in
                                           t160
                                         else
                                           let t161 =
                                             match
                                               s12
                                             with
                                               [ a27 ] ++ ss3 ++ ""
                                             then
                                               let t162 =
                                                 cons
                                               in
                                               let t163 =
                                                 f6
                                                   a27
                                               in
                                               let t164 =
                                                 t162
                                                   t163
                                               in
                                               let t165 =
                                                 rec4
                                                   ss3
                                               in
                                               let t166 =
                                                 t164
                                                   t165
                                               in
                                               t166
                                             else
                                               let t167 =
                                                 never
                                               in
                                               t167
                                           in
                                           t161
                                       in
                                       t158
                                   in
                                   t156
                             in
                             let t155 =
                               rec4
                                 s11
                             in
                             t155
                         in
                         t154
                     in
                     let iter =
                       lam f5.
                         let t151 =
                           lam s10.
                             let t152 =
                               map
                                 f5
                             in
                             let #var"13" =
                               t152
                                 s10
                             in
                             let t153 =
                               {}
                             in
                             t153
                         in
                         t151
                     in
                     let mapi =
                       lam f4.
                         let t127 =
                           lam s8.
                             recursive
                               let rec3 =
                                 lam i1.
                                   let t131 =
                                     lam s9.
                                       let t132 =
                                         match
                                           s9
                                         with
                                           ""
                                         then
                                           let t133 =
                                             ""
                                           in
                                           t133
                                         else
                                           let t134 =
                                             match
                                               s9
                                             with
                                               [ a24 ]
                                             then
                                               let t135 =
                                                 f4
                                                   i1
                                               in
                                               let t136 =
                                                 t135
                                                   a24
                                               in
                                               let t137 =
                                                 [ t136 ]
                                               in
                                               t137
                                             else
                                               let t138 =
                                                 match
                                                   s9
                                                 with
                                                   [ a25 ] ++ ss2 ++ ""
                                                 then
                                                   let t139 =
                                                     cons
                                                   in
                                                   let t140 =
                                                     f4
                                                       i1
                                                   in
                                                   let t141 =
                                                     t140
                                                       a25
                                                   in
                                                   let t142 =
                                                     t139
                                                       t141
                                                   in
                                                   let t143 =
                                                     addi
                                                   in
                                                   let t144 =
                                                     t143
                                                       i1
                                                   in
                                                   let t145 =
                                                     1
                                                   in
                                                   let t146 =
                                                     t144
                                                       t145
                                                   in
                                                   let t147 =
                                                     rec3
                                                       t146
                                                   in
                                                   let t148 =
                                                     t147
                                                       ss2
                                                   in
                                                   let t149 =
                                                     t142
                                                       t148
                                                   in
                                                   t149
                                                 else
                                                   let t150 =
                                                     never
                                                   in
                                                   t150
                                               in
                                               t138
                                           in
                                           t134
                                       in
                                       t132
                                   in
                                   t131
                             in
                             let t128 =
                               0
                             in
                             let t129 =
                               rec3
                                 t128
                             in
                             let t130 =
                               t129
                                 s8
                             in
                             t130
                         in
                         t127
                     in
                     let iteri =
                       lam f3.
                         let t124 =
                           lam s7.
                             let t125 =
                               mapi
                                 f3
                             in
                             let #var"12" =
                               t125
                                 s7
                             in
                             let t126 =
                               {}
                             in
                             t126
                         in
                         t124
                     in
                     let foldl =
                       lam f2.
                         let t112 =
                           lam acc3.
                             let t113 =
                               lam s5.
                                 recursive
                                   let rec2 =
                                     lam acc4.
                                       let t116 =
                                         lam s6.
                                           let t117 =
                                             match
                                               s6
                                             with
                                               ""
                                             then
                                               acc4
                                             else
                                               let t118 =
                                                 match
                                                   s6
                                                 with
                                                   [ a23 ] ++ ss1 ++ ""
                                                 then
                                                   let t119 =
                                                     f2
                                                       acc4
                                                   in
                                                   let t120 =
                                                     t119
                                                       a23
                                                   in
                                                   let t121 =
                                                     rec2
                                                       t120
                                                   in
                                                   let t122 =
                                                     t121
                                                       ss1
                                                   in
                                                   t122
                                                 else
                                                   let t123 =
                                                     never
                                                   in
                                                   t123
                                               in
                                               t118
                                           in
                                           t117
                                       in
                                       t116
                                 in
                                 let t114 =
                                   rec2
                                     acc3
                                 in
                                 let t115 =
                                   t114
                                     s5
                                 in
                                 t115
                             in
                             t113
                         in
                         t112
                     in
                     let foldr =
                       lam f1.
                         let t100 =
                           lam acc1.
                             let t101 =
                               lam s3.
                                 recursive
                                   let rec1 =
                                     lam acc2.
                                       let t104 =
                                         lam s4.
                                           let t105 =
                                             match
                                               s4
                                             with
                                               ""
                                             then
                                               acc2
                                             else
                                               let t106 =
                                                 match
                                                   s4
                                                 with
                                                   [ a22 ] ++ ss ++ ""
                                                 then
                                                   let t107 =
                                                     f1
                                                       a22
                                                   in
                                                   let t108 =
                                                     rec1
                                                       acc2
                                                   in
                                                   let t109 =
                                                     t108
                                                       ss
                                                   in
                                                   let t110 =
                                                     t107
                                                       t109
                                                   in
                                                   t110
                                                 else
                                                   let t111 =
                                                     never
                                                   in
                                                   t111
                                               in
                                               t106
                                           in
                                           t105
                                       in
                                       t104
                                 in
                                 let t102 =
                                   rec1
                                     acc1
                                 in
                                 let t103 =
                                   t102
                                     s3
                                 in
                                 t103
                             in
                             t101
                         in
                         t100
                     in
                     let create =
                       lam l.
                         let t76 =
                           lam f.
                             recursive
                               let rec =
                                 lam i.
                                   let t84 =
                                     lam acc.
                                       let t85 =
                                         geqi
                                       in
                                       let t86 =
                                         t85
                                           i
                                       in
                                       let t87 =
                                         0
                                       in
                                       let t88 =
                                         t86
                                           t87
                                       in
                                       let t89 =
                                         match
                                           t88
                                         with
                                           true
                                         then
                                           let t90 =
                                             subi
                                           in
                                           let t91 =
                                             t90
                                               i
                                           in
                                           let t92 =
                                             1
                                           in
                                           let t93 =
                                             t91
                                               t92
                                           in
                                           let t94 =
                                             rec
                                               t93
                                           in
                                           let t95 =
                                             cons
                                           in
                                           let t96 =
                                             f
                                               i
                                           in
                                           let t97 =
                                             t95
                                               t96
                                           in
                                           let t98 =
                                             t97
                                               acc
                                           in
                                           let t99 =
                                             t94
                                               t98
                                           in
                                           t99
                                         else
                                           acc
                                       in
                                       t89
                                   in
                                   t84
                             in
                             let t77 =
                               subi
                             in
                             let t78 =
                               t77
                                 l
                             in
                             let t79 =
                               1
                             in
                             let t80 =
                               t78
                                 t79
                             in
                             let t81 =
                               rec
                                 t80
                             in
                             let t82 =
                               ""
                             in
                             let t83 =
                               t81
                                 t82
                             in
                             t83
                         in
                         t76
                     in
                     let t7 =
                       {}
                     in
                     let nanosPerSec4 =
                       nanosPerSec7
                     in
                     let wallLogicalTime4 =
                       wallLogicalTime7
                     in
                     let intToFloat3 =
                       intToFloat5
                     in
                     let maxSpeed3 =
                       maxSpeed5
                     in
                     let wheelCircumference3 =
                       wheelCircumference5
                     in
                     let obs =
                       obs2
                     in
                     let #var"3" =
                       {}
                     in
                     let nanosPerSec8 =
                       nanosPerSec4
                     in
                     let wallLogicalTime8 =
                       wallLogicalTime4
                     in
                     let intToFloat6 =
                       intToFloat3
                     in
                     let maxSpeed6 =
                       maxSpeed3
                     in
                     let wheelCircumference6 =
                       wheelCircumference3
                     in
                     let speedObs =
                       obs
                     in
                     let t8 =
                       0.
                     in
                     let t9 =
                       0.1
                     in
                     let t10 =
                       RuntimeDistElementary_DistGaussian
                         { mu =
                             t8,
                           sigma =
                             t9 }
                     in
                     let m =
                       sample
                         t10
                     in
                     let t11 =
                       0.
                     in
                     let t12 =
                       RuntimeDistElementary_DistUniform
                         { a =
                             t11,
                           b =
                             maxSpeed6 }
                     in
                     let b =
                       sample
                         t12
                     in
                     let t13 =
                       1.
                     in
                     let t14 =
                       1.
                     in
                     let t15 =
                       RuntimeDistElementary_DistGamma
                         { scale =
                             t14,
                           shape =
                             t13 }
                     in
                     let sigma =
                       sample
                         t15
                     in
                     let t16 =
                       foldl
                     in
                     let nanosPerSec9 =
                       nanosPerSec8
                     in
                     let wallLogicalTime9 =
                       wallLogicalTime8
                     in
                     let intToFloat7 =
                       intToFloat6
                     in
                     let wheelCircumference7 =
                       wheelCircumference6
                     in
                     let m1 =
                       m
                     in
                     let b1 =
                       b
                     in
                     let sigma1 =
                       sigma
                     in
                     let t17 =
                       lam #var"10".
                         let t22 =
                           lam tsv1.
                             let intToFloat8 =
                               intToFloat7
                             in
                             let nanosPerSec10 =
                               nanosPerSec9
                             in
                             let wallLogicalTime10 =
                               wallLogicalTime9
                             in
                             let tsv2 =
                               tsv1
                             in
                             let t23 =
                               deref
                             in
                             let lt =
                               t23
                                 wallLogicalTime10
                             in
                             let nanosPerSec11 =
                               nanosPerSec10
                             in
                             let nanosPerSec12 =
                               nanosPerSec10
                             in
                             let lhs =
                               match
                                 tsv2
                               with
                                 (#var"X1",)
                               then
                                 #var"X1"
                               else
                                 let t75 =
                                   never
                                 in
                                 t75
                             in
                             let rhs =
                               lt
                             in
                             let t24 =
                               (lhs, rhs)
                             in
                             let t25 =
                               match
                                 t24
                               with
                                 ((ls, lns), (rs, rns))
                               then
                                 let t56 =
                                   subi
                                 in
                                 let t57 =
                                   t56
                                     ls
                                 in
                                 let s2 =
                                   t57
                                     rs
                                 in
                                 let t58 =
                                   subi
                                 in
                                 let t59 =
                                   t58
                                     lns
                                 in
                                 let ns1 =
                                   t59
                                     rns
                                 in
                                 let t60 =
                                   lti
                                 in
                                 let t61 =
                                   t60
                                     ns1
                                 in
                                 let t62 =
                                   0
                                 in
                                 let t63 =
                                   t61
                                     t62
                                 in
                                 let t64 =
                                   match
                                     t63
                                   with
                                     true
                                   then
                                     let t65 =
                                       addi
                                     in
                                     let t66 =
                                       t65
                                         ns1
                                     in
                                     let t67 =
                                       t66
                                         nanosPerSec12
                                     in
                                     let t68 =
                                       subi
                                     in
                                     let t69 =
                                       t68
                                         s2
                                     in
                                     let t70 =
                                       1
                                     in
                                     let t71 =
                                       t69
                                         t70
                                     in
                                     let t72 =
                                       (t71, t67)
                                     in
                                     t72
                                   else
                                     let t73 =
                                       (s2, ns1)
                                     in
                                     t73
                                 in
                                 t64
                               else
                                 let t74 =
                                   never
                                 in
                                 t74
                             in
                             let ts =
                               t25
                             in
                             let t26 =
                               match
                                 ts
                               with
                                 (s1, ns)
                               then
                                 let t49 =
                                   addi
                                 in
                                 let t50 =
                                   muli
                                 in
                                 let t51 =
                                   t50
                                     s1
                                 in
                                 let t52 =
                                   t51
                                     nanosPerSec11
                                 in
                                 let t53 =
                                   t49
                                     t52
                                 in
                                 let t54 =
                                   t53
                                     ns
                                 in
                                 t54
                               else
                                 let t55 =
                                   never
                                 in
                                 t55
                             in
                             let ts1 =
                               t26
                             in
                             let t27 =
                               divf
                             in
                             let t28 =
                               intToFloat8
                                 ts1
                             in
                             let t29 =
                               t27
                                 t28
                             in
                             let t30 =
                               1000000000
                             in
                             let t31 =
                               intToFloat8
                                 t30
                             in
                             let t32 =
                               t29
                                 t31
                             in
                             let ts2 =
                               t32
                             in
                             let tsv3 =
                               tsv1
                             in
                             let t33 =
                               match
                                 tsv3
                               with
                                 {#label"1" = #var"X"}
                               then
                                 #var"X"
                               else
                                 let t48 =
                                   never
                                 in
                                 t48
                             in
                             let rpm =
                               t33
                             in
                             let t34 =
                               divf
                             in
                             let t35 =
                               mulf
                             in
                             let t36 =
                               t35
                                 rpm
                             in
                             let t37 =
                               t36
                                 wheelCircumference7
                             in
                             let t38 =
                               t34
                                 t37
                             in
                             let t39 =
                               60.
                             in
                             let mps =
                               t38
                                 t39
                             in
                             let t40 =
                               addf
                             in
                             let t41 =
                               mulf
                             in
                             let t42 =
                               t41
                                 m1
                             in
                             let t43 =
                               t42
                                 ts2
                             in
                             let t44 =
                               t40
                                 t43
                             in
                             let t45 =
                               t44
                                 b1
                             in
                             let t46 =
                               RuntimeDistElementary_DistGaussian
                                 { mu =
                                     t45,
                                   sigma =
                                     sigma1 }
                             in
                             let #var"11": () =
                               updateWeight
                                 (logObserve
                                    t46
                                    mps)
                                 state
                             in
                             let t47 =
                               {}
                             in
                             t47
                         in
                         t22
                     in
                     let t18 =
                       t16
                         t17
                     in
                     let t19 =
                       {}
                     in
                     let t20 =
                       t18
                         t19
                     in
                     let #var"9" =
                       t20
                         speedObs
                     in
                     let t21 =
                       { slope =
                           m,
                         intercept =
                           b }
                     in
                     (lam x.
                        End
                          x)
                       t21)
in
recursive
  let t6 =
    lam nanosPerSec4.
      lam wallLogicalTime4.
        lam intToFloat3.
          lam maxSpeed3.
            lam wheelCircumference3.
              lam obs.
                lam #var"3".
                  speedModel
                    nanosPerSec4
                    wallLogicalTime4
                    intToFloat3
                    maxSpeed3
                    wheelCircumference3
                    obs
  let loopFn =
    lam fileDescriptors3.
      lam inputSeqs3.
        lam outputSeqs3.
          lam nanosPerSec5.
            lam monoLogicalTime3.
              lam wallLogicalTime5.
                lam nanosPerSec6.
                  lam wallLogicalTime6.
                    lam gtInt3.
                      lam intToFloat4.
                        lam maxSpeed4.
                          lam wheelCircumference4.
                            lam period1.
                              lam #var"4".
                                match
                                  true
                                with
                                  true
                                then
                                  let #var"5" =
                                    sdelay
                                      nanosPerSec5
                                      monoLogicalTime3
                                      wallLogicalTime5
                                      (flushOutputs
                                         fileDescriptors3
                                         outputSeqs3)
                                      (updateInputs
                                         fileDescriptors3
                                         inputSeqs3)
                                      period1
                                  in
                                  let lObs =
                                    unsafeCoerce
                                      ((deref
                                          inputSeqs3).lspeed)
                                  in
                                  let rObs =
                                    unsafeCoerce
                                      ((deref
                                          inputSeqs3).rspeed)
                                  in
                                  let obs1 =
                                    concat
                                      lObs
                                      rObs
                                  in
                                  let #var"6" =
                                    match
                                      gtInt3
                                        (length
                                           obs1)
                                        0
                                    with
                                      true
                                    then
                                      let d =
                                        run
                                          { particles =
                                              1000 }
                                          (t4
                                             obs1
                                             nanosPerSec6
                                             wallLogicalTime6
                                             intToFloat4
                                             maxSpeed4
                                             wheelCircumference4)
                                      in
printLn (join ["speed estimation at t=", int2string (timespecToNanos nanosPerSec (deref wallLogicalTime6))]);
                                      let #var"7" =
                                        let out =
                                          deref
                                            outputSeqs3
                                        in
                                        modref
                                          outputSeqs3
                                          { out
                                            with
                                            speed =
                                              cons
                                                (tsv
                                                   nanosPerSec5
                                                   wallLogicalTime5
                                                   0
                                                   d)
                                                (out.speed) }
                                      in
                                      {}
                                    else
                                      {}
                                  in
                                  loopFn
                                    fileDescriptors3
                                    inputSeqs3
                                    outputSeqs3
                                    nanosPerSec5
                                    monoLogicalTime3
                                    wallLogicalTime5
                                    nanosPerSec6
                                    wallLogicalTime6
                                    gtInt3
                                    intToFloat4
                                    maxSpeed4
                                    wheelCircumference4
                                    period1
                                    {}
                                else
                                  {}
in
let #var"RTPPL_speedEst" =
  lam fileDescriptors2.
    lam inputSeqs2.
      lam outputSeqs2.
        lam nanosPerSec2.
          lam monoLogicalTime2.
            lam wallLogicalTime2.
              lam nanosPerSec3.
                lam wallLogicalTime3.
                  lam gtInt2.
                    lam intToFloat2.
                      lam maxSpeed2.
                        lam wheelCircumference2.
                          lam period.
                            let #var"2" =
                              loopFn
                                fileDescriptors2
                                inputSeqs2
                                outputSeqs2
                                nanosPerSec2
                                monoLogicalTime2
                                wallLogicalTime2
                                nanosPerSec3
                                wallLogicalTime3
                                gtInt2
                                intToFloat2
                                maxSpeed2
                                wheelCircumference2
                                period
                                {}
                            in
                            {}
in
let t5 =
  lam fileDescriptors1.
    lam inputSeqs1.
      lam outputSeqs1.
        lam nanosPerSec1.
          lam monoLogicalTime1.
            lam wallLogicalTime1.
              lam gtInt1.
                lam intToFloat1.
                  lam maxSpeed1.
                    lam wheelCircumference1.
                      lam #var"1".
                        #var"RTPPL_speedEst"
                          fileDescriptors1
                          inputSeqs1
                          outputSeqs1
                          nanosPerSec1
                          monoLogicalTime1
                          wallLogicalTime1
                          nanosPerSec1
                          wallLogicalTime1
                          gtInt1
                          intToFloat1
                          maxSpeed1
                          wheelCircumference1
                          500000000
in
rtpplRuntimeInit
  monoLogicalTime
  wallLogicalTime
  (updateInputs
     fileDescriptors
     inputSeqs)
  (closeFileDescriptors
     fileDescriptors)
  (t5
     fileDescriptors
     inputSeqs
     outputSeqs
     nanosPerSec
     monoLogicalTime
     wallLogicalTime
     gtInt
     intToFloat
     maxSpeed
     wheelCircumference)
