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
  lam a139.
    match
      a139
    with
      true
    then
      false
    else
      true
in
let and: Bool -> Bool -> Bool =
  lam a138.
    lam b51.
      match
        a138
      with
        true
      then
        b51
      else
        false
in
let or: Bool -> Bool -> Bool =
  lam a137.
    lam b50.
      match
        a137
      with
        true
      then
        true
      else
        b50
in
let xor: Bool -> Bool -> Bool =
  lam a136.
    lam b49.
      match
        a136
      with
        true
      then
        not
          b49
      else
        b49
in
let xnor: Bool -> Bool -> Bool =
  lam a135.
    lam b48.
      not
        (xor
           a135
           b48)
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
  lam b47.
    match
      b47
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
let optionEq: all a134. all b46. (a134 -> b46 -> Bool) -> Option a134 -> Option b46 -> Bool =
  lam eq6.
    lam o110.
      lam o27.
        match
          (o110, o27)
        with
          (Some v13, Some v23)
        then
          eq6
            v13
            v23
        else
          match
            (o110, o27)
          with
            (None {}, None {})
          then
            true
          else
            false
in
let optionMap: all a133. all b45. (a133 -> b45) -> Option a133 -> Option b45 =
  lam f62.
    lam o19.
      match
        o19
      with
        Some t741
      then
        Some
          (f62
             t741)
      else
        None
          {}
in
let optionMapAccum: all a131. all b43. all acc89. (acc89 -> a131 -> (acc89, b43)) -> acc89 -> Option a131 -> (acc89, Option b43) =
  lam f61.
    lam acc90.
      lam o18.
        match
          o18
        with
          Some a132
        then
          match
            f61
              acc90
              a132
          with
            (acc91, b44)
          in
          (acc91, Some
              b44)
        else
          (acc90, None
            {})
in
let optionJoin: all a130. Option (Option a130) -> Option a130 =
  lam o17.
    match
      o17
    with
      Some t740
    then
      t740
    else
      None
        {}
in
let optionBind: all a129. all b42. Option a129 -> (a129 -> Option b42) -> Option b42 =
  lam o16.
    lam f60.
      optionJoin
        (optionMap
           f60
           o16)
in
let optionCompose: all a128. all b41. all c40. (b41 -> Option c40) -> (a128 -> Option b41) -> a128 -> Option c40 =
  lam f59.
    lam g3.
      lam x116.
        optionBind
          (g3
             x116)
          f59
in
let optionZipWith: all a127. all b40. all c39. (a127 -> b40 -> c39) -> Option a127 -> Option b40 -> Option c39 =
  lam f58.
    lam o15.
      lam o26.
        match
          (o15, o26)
        with
          (Some v12, Some v22)
        then
          Some
            (f58
               v12
               v22)
        else
          None
            {}
in
let optionZipWithOrElse: all a126. all b39. all c38. (() -> c38) -> (a126 -> b39 -> c38) -> Option a126 -> Option b39 -> c38 =
  lam d12.
    lam f57.
      lam o14.
        lam o25.
          match
            (o14, o25)
          with
            (Some v11, Some v21)
          then
            f57
              v11
              v21
          else
            d12
              {}
in
let optionZipWithOr: all a125. all b38. all c37. c37 -> (a125 -> b38 -> c37) -> Option a125 -> Option b38 -> c37 =
  lam v6.
    optionZipWithOrElse
      (lam #var"94".
         v6)
in
let optionGetOrElse: all a124. (() -> a124) -> Option a124 -> a124 =
  lam d11.
    lam o10.
      match
        o10
      with
        Some t739
      then
        t739
      else
        d11
          {}
in
let optionGetOr: all a123. a123 -> Option a123 -> a123 =
  lam d10.
    optionGetOrElse
      (lam #var"93".
         d10)
in
let optionMapOrElse: all a122. all b37. (() -> b37) -> (a122 -> b37) -> Option a122 -> b37 =
  lam d9.
    lam f56.
      lam o9.
        optionGetOrElse
          d9
          (optionMap
             f56
             o9)
in
let optionMapOr: all a121. all b36. b36 -> (a121 -> b36) -> Option a121 -> b36 =
  lam d8.
    lam f55.
      lam o8.
        optionGetOr
          d8
          (optionMap
             f55
             o8)
in
let optionMapM: all a120. all b35. (a120 -> Option b35) -> [a120] -> Option [b35] =
  lam f54.
    lam l21.
      recursive
        let g2 =
          lam l22.
            lam acc88.
              match
                l22
              with
                [ hd ] ++ rest1 ++ ""
              then
                match
                  f54
                    hd
                with
                  Some x115
                then
                  g2
                    rest1
                    (snoc
                       acc88
                       x115)
                else
                  None
                    {}
              else
                Some
                  acc88
      in
      g2
        l21
        ""
in
let optionFoldlM: all a117. all b33. (a117 -> b33 -> Option a117) -> a117 -> [b33] -> Option a117 =
  lam f53.
    recursive
      let recur =
        lam a118.
          lam bs1.
            match
              bs1
            with
              [ b34 ] ++ bs2 ++ ""
            then
              let res11 =
                f53
                  a118
                  b34
              in
              match
                res11
              with
                Some a119
              then
                recur
                  a119
                  bs2
              else
                match
                  res11
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
                  a118
    in
    recur
in
let optionContains: all a116. Option a116 -> (a116 -> Bool) -> Bool =
  lam o7.
    lam p33.
      optionMapOr
        false
        p33
        o7
in
let optionIsSome: all a115. Option a115 -> Bool =
  lam o6.
    optionContains
      o6
      (lam #var"92".
         true)
in
let optionIsNone: all a114. Option a114 -> Bool =
  lam o5.
    not
      (optionIsSome
         o5)
in
let optionAnd: all a113. Option a113 -> Option a113 -> Option a113 =
  lam o13.
    lam o24.
      match
        (o13, o24)
      with
        (Some _, Some _)
      then
        o13
      else
        None
          {}
in
let optionFilter: all a109. (a109 -> Bool) -> Option a109 -> Option a109 =
  lam p32.
    lam o4.
      match
        optionContains
          o4
          p32
      with
        true
      then
        o4
      else
        None
          {}
in
let optionOrElse: all a108. (() -> Option a108) -> Option a108 -> Option a108 =
  lam f52.
    lam o3.
      optionGetOrElse
        f52
        (optionMap
           (lam x114.
              Some
                x114)
           o3)
in
let optionOr: all a107. Option a107 -> Option a107 -> Option a107 =
  lam o12.
    lam o23.
      optionOrElse
        (lam #var"91".
           o23)
        o12
in
let optionXor: all a106. Option a106 -> Option a106 -> Option a106 =
  lam o11.
    lam o22.
      match
        (o11, o22)
      with
        (Some _, None {})
      then
        o11
      else
        match
          (o11, o22)
        with
          (None {}, Some _)
        then
          o22
        else
          None
            {}
in
let make =
  lam n20.
    lam v5.
      create
        n20
        (lam #var"90".
           v5)
in
let last =
  lam seq36.
    get
      seq36
      (subi
         (length
            seq36)
         1)
in
let init =
  lam seq35.
    subsequence
      seq35
      0
      (subi
         (length
            seq35)
         1)
in
let eqSeq: all a105. all b32. (a105 -> b32 -> Bool) -> [a105] -> [b32] -> Bool =
  lam eq5.
    lam s121.
      lam s221.
        recursive
          let work18 =
            lam s122.
              lam s222.
                match
                  (s122, s222)
                with
                  ([ h13 ] ++ t1102 ++ "", [ h23 ] ++ t2102 ++ "")
                then
                  match
                    eq5
                      h13
                      h23
                  with
                    true
                  then
                    work18
                      t1102
                      t2102
                  else
                    false
                else
                  true
        in
        let n110 =
          length
            s121
        in
        let n23 =
          length
            s221
        in
        let ndiff1 =
          subi
            n110
            n23
        in
        match
          eqi
            ndiff1
            0
        with
          true
        then
          work18
            s121
            s221
        else
          false
in
let toRope =
  lam seq34.
    createRope
      (length
         seq34)
      (lam i24.
         get
           seq34
           i24)
in
let toList =
  lam seq33.
    createList
      (length
         seq33)
      (lam i23.
         get
           seq33
           i23)
in
let mapOption: all a103. all b30. (a103 -> Option b30) -> [a103] -> [b30] =
  lam f51.
    recursive
      let work17 =
        lam as3.
          match
            as3
          with
            [ a104 ] ++ as4 ++ ""
          then
            match
              f51
                a104
            with
              Some b31
            then
              cons
                b31
                (work17
                   as4)
            else
              work17
                as4
          else
            ""
    in
    work17
in
let for_: all a102. [a102] -> (a102 -> ()) -> () =
  lam xs12.
    lam f50.
      iter
        f50
        xs12
in
let mapReverse =
  lam f49.
    lam lst1.
      foldl
        (lam acc87.
           lam x109.
             cons
               (f49
                  x109)
               acc87)
        (toList
           "")
        lst1
in
let mapK: all a101. all b29. all c36. (a101 -> (b29 -> c36) -> c36) -> [a101] -> ([b29] -> c36) -> c36 =
  lam f48.
    lam seq32.
      lam k2.
        foldl
          (lam k3.
             lam x107.
               lam xs10.
                 f48
                   x107
                   (lam x108.
                      k3
                        (cons
                           x108
                           xs10)))
          k2
          seq32
          ""
in
let foldl1 =
  lam f47.
    lam l20.
      foldl
        f47
        (head
           l20)
        (tail
           l20)
in
let foldr1 =
  lam f46.
    lam seq31.
      foldr
        f46
        (last
           seq31)
        (init
           seq31)
in
recursive
  let unfoldr: all a100. all c35. (a100 -> Option (c35, a100)) -> a100 -> [c35] =
    lam f45.
      lam b0.
        let fb =
          f45
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
            Some (x106, b110)
          in
          cons
              x106
              (unfoldr
                 f45
                 b110)
in
let range =
  lam s79.
    lam e6.
      lam by.
        unfoldr
          (lam b28.
             match
               leqi
                 e6
                 b28
             with
               true
             then
               None
                 {}
             else
               Some
                 (b28, addi
                   b28
                   by))
          s79
in
recursive
  let foldl21: all a99. all b27. all c34. (a99 -> b27 -> c34 -> a99) -> a99 -> [b27] -> [c34] -> a99 =
    lam f44.
      lam acc82.
        lam seq112.
          lam seq210.
            let g1 =
              lam acc85: (a99, [b27]).
                lam x213.
                  match
                    acc85
                  with
                    (acc86, [ x113 ] ++ xs11 ++ "")
                  in
                  (f44
                      acc86
                      x113
                      x213, xs11)
            in
            match
              geqi
                (length
                   seq112)
                (length
                   seq210)
            with
              true
            then
              match
                foldl
                  g1
                  (acc82, seq112)
                  seq210
              with
                (acc83, _)
              in
              acc83
            else
              foldl21
                (lam acc84.
                   lam x112.
                     lam x212.
                       f44
                         acc84
                         x212
                         x112)
                acc82
                seq210
                seq112
in
let foldli: all a98. all b26. (a98 -> Int -> b26 -> a98) -> a98 -> [b26] -> a98 =
  lam fn.
    lam initAcc.
      lam seq30.
        recursive
          let work16 =
            lam acc81.
              lam i22.
                lam s78.
                  match
                    s78
                  with
                    [ e5 ] ++ rest ++ ""
                  then
                    work16
                      (fn
                         acc81
                         i22
                         e5)
                      (addi
                         i22
                         1)
                      rest
                  else
                    acc81
        in
        work16
          initAcc
          0
          seq30
in
let zipWith: all a97. all b25. all c33. (a97 -> b25 -> c33) -> [a97] -> [b25] -> [c33] =
  lam f43.
    foldl21
      (lam acc80.
         lam x111.
           lam x211.
             snoc
               acc80
               (f43
                  x111
                  x211))
      ""
in
let zipWithIndex: all a96. all b24. all c32. (Int -> a96 -> b24 -> c32) -> [a96] -> [b24] -> [c32] =
  lam f42.
    lam a112.
      lam a210.
        recursive
          let work15 =
            lam acc79.
              lam i21.
                lam seq111.
                  lam seq29.
                    match
                      seq111
                    with
                      [ e11 ] ++ seq1tail ++ ""
                    then
                      match
                        seq29
                      with
                        [ e21 ] ++ seq2tail ++ ""
                      then
                        work15
                          (cons
                             (f42
                                i21
                                e11
                                e21)
                             acc79)
                          (addi
                             i21
                             1)
                          seq1tail
                          seq2tail
                      else
                        reverse
                          acc79
                    else
                      reverse
                        acc79
        in
        work15
          (toList
             "")
          0
          a112
          a210
in
let zip: all a95. all b23. [a95] -> [b23] -> [(a95, b23)] =
  zipWith
    (lam x105.
       lam y11.
         (x105, y11))
in
let mapAccumL: all a94. all b22. all c31. (a94 -> b22 -> (a94, c31)) -> a94 -> [b22] -> (a94, [c31]) =
  lam f41: a94 -> b22 -> (a94, c31).
    lam acc77.
      lam seq28.
        foldl
          (lam tacc1: (a94, [c31]).
             lam x104.
               match
                 f41
                   (tacc1.0)
                   x104
               with
                 (acc78, y10)
               in
               (acc78, snoc
                   (tacc1.1)
                   y10))
          (acc77, "")
          seq28
in
let mapAccumR: all a93. all b21. all c30. (a93 -> b21 -> (a93, c30)) -> a93 -> [b21] -> (a93, [c30]) =
  lam f40: a93 -> b21 -> (a93, c30).
    lam acc75.
      lam seq27.
        foldr
          (lam x103.
             lam tacc: (a93, [c30]).
               match
                 f40
                   (tacc.0)
                   x103
               with
                 (acc76, y9)
               in
               (acc76, cons
                   y9
                   (tacc.1)))
          (acc75, "")
          seq27
in
let unzip: all a92. all b20. [(a92, b20)] -> ([a92], [b20]) =
  mapAccumL
    (lam l19.
       lam p31: (a92, b20).
         (snoc
           l19
           (p31.0), p31.1))
    ""
in
let iter2: all a91. all b19. (a91 -> b19 -> ()) -> [a91] -> [b19] -> () =
  lam f38.
    lam seq110.
      lam seq26.
        let f39 =
          lam x102: (a91, b19).
            match
              x102
            with
              (x110, x210)
            in
            f38
                x110
                x210
        in
        iter
          f39
          (zip
             seq110
             seq26)
in
recursive
  let any =
    lam p30.
      lam seq25.
        match
          null
            seq25
        with
          true
        then
          false
        else
          match
            p30
              (head
                 seq25)
          with
            true
          then
            true
          else
            any
              p30
              (tail
                 seq25)
in
recursive
  let forAll =
    lam p29.
      lam seq24.
        match
          null
            seq24
        with
          true
        then
          true
        else
          match
            p29
              (head
                 seq24)
          with
            true
          then
            forAll
              p29
              (tail
                 seq24)
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
let seqLiftA2: all a89. all b18. all c29. (a89 -> b18 -> c29) -> [a89] -> [b18] -> [c29] =
  lam f37.
    lam as2.
      lam bs.
        join
          (map
             (lam a90.
                map
                  (f37
                     a90)
                  bs)
             as2)
in
let seqMapM: all a87. all b17. (a87 -> [b17]) -> [a87] -> [[b17]] =
  lam f36.
    foldr
      (lam a88.
         lam acc74.
           seqLiftA2
             cons
             (f36
                a88)
             acc74)
      [ "" ]
in
recursive
  let filter =
    lam p28.
      lam seq23.
        match
          null
            seq23
        with
          true
        then
          ""
        else
          match
            p28
              (head
                 seq23)
          with
            true
          then
            cons
              (head
                 seq23)
              (filter
                 p28
                 (tail
                    seq23))
          else
            filter
              p28
              (tail
                 seq23)
in
recursive
  let filterOption: all a86. [Option a86] -> [a86] =
    lam optSeq.
      match
        optSeq
      with
        [ Some x101 ] ++ optSeq1 ++ ""
      then
        cons
          x101
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
    lam p27.
      lam seq22.
        match
          null
            seq22
        with
          true
        then
          None
            {}
        else
          match
            p27
              (head
                 seq22)
          with
            true
          then
            Some
              (head
                 seq22)
          else
            find
              p27
              (tail
                 seq22)
in
recursive
  let findMap: all a85. all b16. (a85 -> Option b16) -> [a85] -> Option b16 =
    lam f35.
      lam seq20.
        match
          seq20
        with
          [ h5 ] ++ t738 ++ ""
        then
          match
            f35
              h5
          with
            Some x100
          then
            Some
              x100
          else
            findMap
              f35
              t738
        else
          None
            {}
in
let lowerBoundBinarySearch: all a84. (a84 -> Int) -> [a84] -> Option Int =
  lam f34.
    lam s77.
      recursive
        let work14 =
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
                    (f34
                       (get
                          s77
                          idx3))
                    0
                with
                  true
                then
                  work14
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
                  work14
                    first
                    step1
              else
                first
      in
      let idx2 =
        work14
          0
          (length
             s77)
      in
      match
        eqi
          idx2
          (length
             s77)
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
  lam p26.
    lam seq17.
      recursive
        let work13 =
          lam l18.
            lam r15.
              lam seq18.
                match
                  seq18
                with
                  ""
                then
                  (l18, r15)
                else
                  match
                    seq18
                  with
                    [ s76 ] ++ seq19 ++ ""
                  in
                  match
                      p26
                        s76
                    with
                      true
                    then
                      work13
                        (cons
                           s76
                           l18)
                        r15
                        seq19
                    else
                      work13
                        l18
                        (cons
                           s76
                           r15)
                        seq19
      in
      work13
        ""
        ""
        (reverse
           seq17)
in
let distinct =
  lam eq4.
    lam seq15.
      recursive
        let work12 =
          lam seq16.
            lam seq21.
              match
                seq16
              with
                [ h4 ] ++ t737 ++ ""
              then
                match
                  find
                    (eq4
                       h4)
                    seq21
                with
                  Some _
                then
                  work12
                    t737
                    seq21
                else
                  cons
                    h4
                    (work12
                       t737
                       (cons
                          h4
                          seq21))
              else
                ""
      in
      work12
        seq15
        ""
in
let distinctSorted =
  lam eq3.
    lam s74.
      recursive
        let work11 =
          lam acc73.
            lam s75.
              match
                s75
              with
                [ h12 ] ++ t736 ++ ""
              then
                match
                  acc73
                with
                  [ h22 ] ++ _ ++ ""
                then
                  match
                    eq3
                      h12
                      h22
                  with
                    true
                  then
                    work11
                      acc73
                      t736
                  else
                    work11
                      (cons
                         h12
                         acc73)
                      t736
                else
                  work11
                    [ h12 ]
                    t736
              else
                acc73
      in
      reverse
        (work11
           ""
           s74)
in
recursive
  let quickSort: all a83. (a83 -> a83 -> Int) -> [a83] -> [a83] =
    lam cmp12.
      lam seq14.
        match
          null
            seq14
        with
          true
        then
          seq14
        else
          let h3 =
            head
              seq14
          in
          let t735 =
            tail
              seq14
          in
          let lr1 =
            partition
              (lam x99.
                 lti
                   (cmp12
                      x99
                      h3)
                   0)
              t735
          in
          concat
            (quickSort
               cmp12
               (lr1.0))
            (cons
               h3
               (quickSort
                  cmp12
                  (lr1.1)))
in
recursive
  let merge =
    lam cmp11.
      lam l17.
        lam r14.
          match
            l17
          with
            ""
          then
            r14
          else
            match
              r14
            with
              ""
            then
              l17
            else
              match
                (l17, r14)
              with
                ([ x98 ] ++ xs9 ++ "", [ y8 ] ++ ys1 ++ "")
              in
              match
                  leqi
                    (cmp11
                       x98
                       y8)
                    0
                with
                  true
                then
                  cons
                    x98
                    (merge
                       cmp11
                       xs9
                       r14)
                else
                  cons
                    y8
                    (merge
                       cmp11
                       l17
                       ys1)
in
recursive
  let mergeSort =
    lam cmp10.
      lam seq13.
        match
          seq13
        with
          ""
        then
          ""
        else
          match
            seq13
          with
            [ x97 ]
          then
            [ x97 ]
          else
            let lr =
              splitAt
                seq13
                (divi
                   (length
                      seq13)
                   2)
            in
            merge
              cmp10
              (mergeSort
                 cmp10
                 (lr.0))
              (mergeSort
                 cmp10
                 (lr.1))
in
let sort =
  quickSort
in
let minIdx: all a82. (a82 -> a82 -> Int) -> [a82] -> Option (Int, a82) =
  lam cmp9: a82 -> a82 -> Int.
    lam seq12: [a82].
      match
        null
          seq12
      with
        true
      then
        None
          {}
      else
        match
          foldl
            (lam acc72: (Int, Int, a82).
               lam e4: a82.
                 match
                   acc72
                 with
                   (curi, mini1, m16)
                 in
                 match
                     lti
                       (cmp9
                          m16
                          e4)
                       0
                   with
                     true
                   then
                     (addi
                       curi
                       1, mini1, m16)
                   else
                     (addi
                       curi
                       1, curi, e4))
            (1, 0, head
              seq12)
            (tail
               seq12)
        with
          (_, i20, m17)
        in
        Some
            (i20, m17)
in
let min: all a81. (a81 -> a81 -> Int) -> [a81] -> Option a81 =
  lam cmp8.
    lam seq11.
      optionMap
        (lam r13.
           match
             r13
           with
             (_, m15)
           in
           m15)
        (minIdx
           cmp8
           seq11)
in
let max =
  lam cmp7.
    min
      (lam l16.
         lam r12.
           cmp7
             r12
             l16)
in
let minOrElse =
  lam d7.
    lam cmp6.
      lam seq10.
        optionGetOrElse
          d7
          (min
             cmp6
             seq10)
in
let maxOrElse =
  lam d6.
    lam cmp5.
      minOrElse
        d6
        (lam l15.
           lam r11.
             cmp5
               r11
               l15)
in
let index =
  lam pred3.
    lam seq8.
      recursive
        let index_rechelper =
          lam i19.
            lam pred4.
              lam seq9.
                match
                  null
                    seq9
                with
                  true
                then
                  None
                    {}
                else
                  match
                    pred4
                      (head
                         seq9)
                  with
                    true
                  then
                    Some
                      i19
                  else
                    index_rechelper
                      (addi
                         i19
                         1)
                      pred4
                      (tail
                         seq9)
      in
      index_rechelper
        0
        pred3
        seq8
in
let lastIndex =
  lam pred1.
    lam seq6.
      recursive
        let lastIndex_rechelper =
          lam i18.
            lam acc71.
              lam pred2.
                lam seq7.
                  match
                    null
                      seq7
                  with
                    true
                  then
                    acc71
                  else
                    match
                      pred2
                        (head
                           seq7)
                    with
                      true
                    then
                      lastIndex_rechelper
                        (addi
                           i18
                           1)
                        (Some
                           i18)
                        pred2
                        (tail
                           seq7)
                    else
                      lastIndex_rechelper
                        (addi
                           i18
                           1)
                        acc71
                        pred2
                        (tail
                           seq7)
      in
      lastIndex_rechelper
        0
        (None
           {})
        pred1
        seq6
in
recursive
  let isPrefix =
    lam eq2.
      lam s120.
        lam s220.
          match
            null
              s120
          with
            true
          then
            true
          else
            match
              null
                s220
            with
              true
            then
              false
            else
              and
                (eq2
                   (head
                      s120)
                   (head
                      s220))
                (isPrefix
                   eq2
                   (tail
                      s120)
                   (tail
                      s220))
in
let isSuffix =
  lam eq1.
    lam s119.
      lam s219.
        isPrefix
          eq1
          (reverse
             s119)
          (reverse
             s219)
in
let seqCmp: all a80. (a80 -> a80 -> Int) -> [a80] -> [a80] -> Int =
  lam cmp4.
    lam s117.
      lam s217.
        recursive
          let work10 =
            lam s118.
              lam s218.
                match
                  (s118, s218)
                with
                  ([ h11 ] ++ t1101 ++ "", [ h21 ] ++ t2101 ++ "")
                then
                  let c28 =
                    cmp4
                      h11
                      h21
                  in
                  match
                    eqi
                      c28
                      0
                  with
                    true
                  then
                    work10
                      t1101
                      t2101
                  else
                    c28
                else
                  0
        in
        let n19 =
          length
            s117
        in
        let n22 =
          length
            s217
        in
        let ndiff =
          subi
            n19
            n22
        in
        match
          eqi
            ndiff
            0
        with
          true
        then
          work10
            s117
            s217
        else
          ndiff
in
let randIndex: all a79. [a79] -> Option Int =
  lam seq5.
    match
      seq5
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
              seq5))
in
let randElem: all a78. [a78] -> Option a78 =
  lam seq4.
    optionMap
      (get
         seq4)
      (randIndex
         seq4)
in
let permute: all a77. [a77] -> [Int] -> [a77] =
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
            (lam x96: (a77, Int).
               lam y7: (a77, Int).
                 subi
                   (x96.1)
                   (y7.1))
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
  lam x95.
    x95
in
let const =
  lam x94.
    lam #var"89".
      x94
in
let apply =
  lam f33.
    lam x93.
      f33
        x93
in
let compose =
  lam f32.
    lam g.
      lam x92.
        f32
          (g
             x92)
in
let curry =
  lam f31.
    lam x91.
      lam y6.
        f31
          (x91, y6)
in
let uncurry: all a76. all b15. all c20. (a76 -> b15 -> c20) -> (a76, b15) -> c20 =
  lam f30.
    lam t734: (a76, b15).
      f30
        (t734.0)
        (t734.1)
in
let flip =
  lam f29.
    lam x90.
      lam y5.
        f29
          y5
          x90
in
let printLn =
  lam s73.
    let #var"88" =
      print
        (concat
           s73
           "\n")
    in
    flushStdout
      {}
in
let printSeq =
  lam s72.
    print
      (join
         s72)
in
let printSeqLn =
  lam s71.
    let #var"86" =
      printSeq
        s71
    in
    let #var"87" =
      print
        "\n"
    in
    flushStdout
      {}
in
let dprintLn =
  lam x89.
    let #var"85" =
      dprint
        x89
    in
    printLn
      ""
in
recursive
  let fix: all a75. all b14. ((a75 -> b14) -> a75 -> b14) -> a75 -> b14 =
    lam f28.
      lam e3.
        f28
          (fix
             f28)
          e3
in
let repeat: (() -> ()) -> Int -> () =
  lam f27.
    lam n17.
      recursive
        let rec19 =
          lam n18.
            match
              leqi
                n18
                0
            with
              true
            then
              {}
            else
              let #var"84" =
                f27
                  {}
              in
              rec19
                (subi
                   n18
                   1)
      in
      rec19
        n17
in
let repeati: (Int -> ()) -> Int -> () =
  lam f26.
    lam n16.
      recursive
        let rec18 =
          lam i17.
            match
              geqi
                i17
                n16
            with
              true
            then
              {}
            else
              let #var"83" =
                f26
                  i17
              in
              rec18
                (addi
                   i17
                   1)
      in
      rec18
        0
in
let fixMutual: all a74. all b13. [[a74 -> b13] -> a74 -> b13] -> [a74 -> b13] =
  lam l12.
    let l13 =
      map
        (lam li1.
           (li1,))
        l12
    in
    fix
      (lam self.
         lam l14.
           map
             (lam li: ([a74 -> b13] -> a74 -> b13,).
                lam x88.
                  (li.0)
                    (self
                       l14)
                    x88)
             l14)
      l13
in
let maxf: Float -> Float -> Float =
  lam r10.
    lam l11.
      match
        gtf
          r10
          l11
      with
        true
      then
        r10
      else
        l11
in
let absf: Float -> Float =
  lam f25.
    maxf
      f25
      (negf
         f25)
in
let eqfApprox =
  lam epsilon1.
    lam r9.
      lam l10.
        match
          leqf
            (absf
               (subf
                  r9
                  l10))
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
  lam x87: Float.
    externalExp
      x87
in
external externalLog : Float -> Float
in
let log =
  lam x86: Float.
    externalLog
      x86
in
external externalAtan : Float -> Float
in
let atan =
  lam x85: Float.
    externalAtan
      x85
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
  lam x84: Float.
    externalSin
      x84
in
external externalCos : Float -> Float
in
let cos =
  lam x83: Float.
    externalCos
      x83
in
external externalAtan2 : Float -> Float -> Float
in
let atan2 =
  lam x82: Float.
    lam y4: Float.
      externalAtan2
        x82
        y4
in
external externalPow : Float -> Float -> Float
in
let pow =
  lam x81: Float.
    lam y3: Float.
      externalPow
        x81
        y3
in
external externalSqrt : Float -> Float
in
let sqrt: Float -> Float =
  lam x80.
    externalSqrt
      x80
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
  lam r8.
    lam l9.
      match
        ltf
          r8
          l9
      with
        true
      then
        r8
      else
        l9
in
let cmpfApprox: Float -> Float -> Float -> Int =
  lam epsilon.
    lam l8.
      lam r7.
        match
          eqfApprox
            epsilon
            l8
            r7
        with
          true
        then
          0
        else
          match
            ltf
              l8
              r7
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
  lam n14.
    recursive
      let work9 =
        lam acc70.
          lam n15.
            match
              gti
                n15
                0
            with
              true
            then
              work9
                (addf
                   (log
                      (int2float
                         n15))
                   acc70)
                (subi
                   n15
                   1)
            else
              acc70
    in
    work9
      0.
      n14
in
let maxi =
  lam r6.
    lam l7.
      match
        gti
          r6
          l7
      with
        true
      then
        r6
      else
        l7
in
let mini =
  lam r5.
    lam l6.
      match
        lti
          r5
          l6
      with
        true
      then
        r5
      else
        l6
in
let absi =
  lam i16.
    maxi
      i16
      (negi
         i16)
in
let succ =
  lam x79.
    addi
      x79
      1
in
let pred =
  lam x78.
    subi
      x78
      1
in
external externalGammaLogPdf : Float -> Float -> Float -> Float
in
external externalGammaSample! : Float -> Float -> Float
in
let gammaPdf =
  lam shape2: Float.
    lam scale2: Float.
      lam x77: Float.
        exp
          (externalGammaLogPdf
             x77
             shape2
             scale2)
in
let gammaLogPdf =
  lam shape1: Float.
    lam scale1: Float.
      lam x76: Float.
        externalGammaLogPdf
          x76
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
  lam p25: Float.
    lam n13: Int.
      lam x75: Int.
        exp
          (externalBinomialLogPmf
             x75
             p25
             n13)
in
let binomialLogPmf =
  lam p24: Float.
    lam n12: Int.
      lam x74: Int.
        externalBinomialLogPmf
          x74
          p24
          n12
in
let binomialSample =
  lam p23: Float.
    lam n11: Int.
      externalBinomialSample
        p23
        n11
in
let bernoulliPmf =
  lam p22: Float.
    lam x73: Bool.
      match
        x73
      with
        true
      then
        p22
      else
        subf
          1.
          p22
in
let bernoulliLogPmf =
  lam p21: Float.
    lam x72: Bool.
      log
        (bernoulliPmf
           p21
           x72)
in
let bernoulliSample =
  lam p20: Float.
    match
      eqi
        1
        (externalBinomialSample
           p20
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
  lam a73: Float.
    lam b12: Float.
      lam x71: Float.
        exp
          (externalBetaLogPdf
             x71
             a73
             b12)
in
let betaLogPdf =
  lam a72: Float.
    lam b11: Float.
      lam x70: Float.
        externalBetaLogPdf
          x70
          a72
          b11
in
let betaSample =
  lam a71: Float.
    lam b10: Float.
      externalBetaSample
        a71
        b10
in
external externalGaussianLogPdf : Float -> Float -> Float -> Float
in
external externalGaussianSample! : Float -> Float -> Float
in
let gaussianPdf =
  lam mu2: Float.
    lam sigma4: Float.
      lam x69: Float.
        exp
          (externalGaussianLogPdf
             x69
             mu2
             sigma4)
in
let gaussianLogPdf =
  lam mu1: Float.
    lam sigma3: Float.
      lam x68: Float.
        externalGaussianLogPdf
          x68
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
    lam x67.
      log
        (get
           ps3
           x67)
in
let categoricalPmf: [Float] -> Int -> Float =
  lam ps2.
    lam x66.
      get
        ps2
        x66
in
let multinomialSample: [Float] -> Int -> [Int] =
  lam ps1.
    lam n10.
      externalMultinomialSample
        n10
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
    lam xs8.
      match
        eqfApprox
          1e-15
          (foldl
             addf
             0.
             xs8)
          1.
      with
        true
      then
        externalDirichletLogPdf
          xs8
          alpha2
      else
        negf
          inf
in
let dirichletPdf: [Float] -> [Float] -> Float =
  lam alpha1.
    lam xs7.
      exp
        (externalDirichletLogPdf
           xs7
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
  lam a70.
    lam b9.
      externalUniformContinuousSample
        a70
        b9
in
let uniformContinuousLogPdf =
  lam a69.
    lam b8.
      lam x65.
        match
          geqf
            x65
            a69
        with
          true
        then
          match
            leqf
              x65
              b8
          with
            true
          then
            subf
              (log
                 1.)
              (log
                 (subf
                    b8
                    a69))
          else
            0.
        else
          0.
in
let uniformContinuousPdf =
  lam a68.
    lam b7.
      lam x64.
        match
          geqf
            x64
            a68
        with
          true
        then
          match
            leqf
              x64
              b7
          with
            true
          then
            divf
              1.
              (subf
                 b7
                 a68)
          else
            0.
        else
          0.
in
let uniformSample: () -> Float =
  lam #var"82".
    uniformContinuousSample
      0.
      1.
in
external externalUniformDiscreteSample! : Int -> Int -> Int
in
let uniformDiscreteSample =
  lam a67: Int.
    lam b6: Int.
      externalUniformDiscreteSample
        a67
        b6
in
let uniformDiscreteLogPdf: Int -> Int -> Int -> Float =
  lam a66.
    lam b5.
      lam x63.
        match
          geqi
            x63
            a66
        with
          true
        then
          match
            leqi
              x63
              b5
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
                          b5
                          a66))))
          else
            0.
        else
          0.
in
let uniformDiscretePdf: Int -> Int -> Int -> Float =
  lam a65.
    lam b4.
      lam x62.
        match
          geqi
            x62
            a65
        with
          true
        then
          match
            leqi
              x62
              b4
          with
            true
          then
            divf
              1.
              (int2float
                 (addi
                    1
                    (subi
                       b4
                       a65)))
          else
            0.
        else
          0.
in
let poissonLogPmf =
  lam lambda5: Float.
    lam x61: Int.
      subf
        (subf
           (mulf
              (int2float
                 x61)
              (log
                 lambda5))
           lambda5)
        (logFactorial
           x61)
in
let poissonPmf =
  lam lambda4: Float.
    lam x60: Int.
      exp
        (poissonLogPmf
           lambda4
           x60)
in
let poissonSample =
  lam lambda3: Float.
    let enlam =
      exp
        (negf
           lambda3)
    in
    let x58 =
      0
    in
    let prod =
      1.
    in
    recursive
      let rec17 =
        lam x59.
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
              rec17
                (addi
                   x59
                   1)
                prod2
            else
              x59
    in
    rec17
      x58
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
    lam x57.
      subf
        (log
           lambda1)
        (mulf
           lambda1
           x57)
in
let exponentialPdf: Float -> Float -> Float =
  lam lambda.
    lam x56.
      exp
        (exponentialLogPdf
           lambda
           x56)
in
external externalSetSeed! : Int -> ()
in
let setSeed: Int -> () =
  lam seed.
    externalSetSeed
      seed
in
let eqChar =
  lam c116.
    lam c27.
      eqc
        c116
        c27
in
let neqChar =
  lam c115.
    lam c26.
      not
        (eqc
           c115
           c26)
in
let ltChar =
  lam c114.
    lam c25.
      lti
        (char2int
           c114)
        (char2int
           c25)
in
let gtChar =
  lam c113.
    lam c24.
      gti
        (char2int
           c113)
        (char2int
           c24)
in
let leqChar =
  lam c112.
    lam c23.
      leqi
        (char2int
           c112)
        (char2int
           c23)
in
let geqChar =
  lam c111.
    lam c22.
      geqi
        (char2int
           c111)
        (char2int
           c22)
in
let cmpChar =
  lam c110.
    lam c21.
      subi
        (char2int
           c110)
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
  lam c19.
    match
      find
        (lam e2: (Char, [Char]).
           eqChar
             c19
             (e2.0))
        _escapes
    with
      Some n8
    then
      let n9: (Char, [Char]) =
        n8
      in
      n9.1
    else
      [ c19 ]
in
let showChar =
  lam c18.
    join
      [ "\'",
        escapeChar
          c18,
        "\'" ]
in
let char2upper =
  lam c17.
    match
      and
        (geqChar
           c17
           'a')
        (leqChar
           c17
           'z')
    with
      true
    then
      int2char
        (subi
           (char2int
              c17)
           32)
    else
      c17
in
let char2lower =
  lam c16.
    match
      and
        (geqChar
           c16
           'A')
        (leqChar
           c16
           'Z')
    with
      true
    then
      int2char
        (addi
           (char2int
              c16)
           32)
    else
      c16
in
let isWhitespace =
  lam c15.
    any
      (eqChar
         c15)
      " \n\t\r"
in
let isLowerAlpha =
  lam c14.
    let i15 =
      char2int
        c14
    in
    match
      leqi
        (char2int
           'a')
        i15
    with
      true
    then
      leqi
        i15
        (char2int
           'z')
    else
      false
in
let isUpperAlpha =
  lam c13.
    let i14 =
      char2int
        c13
    in
    match
      leqi
        (char2int
           'A')
        i14
    with
      true
    then
      leqi
        i14
        (char2int
           'Z')
    else
      false
in
let isAlpha =
  lam c12.
    match
      isLowerAlpha
        c12
    with
      true
    then
      true
    else
      isUpperAlpha
        c12
in
let isLowerAlphaOrUnderscore =
  lam c11.
    match
      isLowerAlpha
        c11
    with
      true
    then
      true
    else
      eqChar
        c11
        '_'
in
let isAlphaOrUnderscore =
  lam c10.
    match
      isAlpha
        c10
    with
      true
    then
      true
    else
      eqChar
        c10
        '_'
in
let isDigit =
  lam c9.
    let i13 =
      char2int
        c9
    in
    match
      leqi
        (char2int
           '0')
        i13
    with
      true
    then
      leqi
        i13
        (char2int
           '9')
    else
      false
in
let isAlphanum =
  lam c8.
    match
      isAlpha
        c8
    with
      true
    then
      true
    else
      isDigit
        c8
in
let randAlphanum: () -> Char =
  lam #var"81".
    let r4 =
      randIntU
        0
        62
    in
    match
      lti
        r4
        10
    with
      true
    then
      int2char
        (addi
           r4
           48)
    else
      match
        lti
          r4
          36
      with
        true
      then
        int2char
          (addi
             r4
             55)
      else
        int2char
          (addi
             r4
             61)
in
let emptyStr: [Char] =
  ""
in
let escapeString =
  lam s70.
    join
      (map
         escapeChar
         s70)
in
let eqString =
  lam s116.
    lam s216.
      eqSeq
        eqc
        s116
        s216
in
let neqString =
  lam s115.
    lam s215.
      not
        (eqString
           s115
           s215)
in
let eqStringSlice =
  lam s114.
    lam s214.
      lam o21.
        lam n21.
          recursive
            let work8 =
              lam i12.
                match
                  eqi
                    i12
                    n21
                with
                  true
                then
                  true
                else
                  match
                    eqc
                      (get
                         s114
                         i12)
                      (get
                         s214
                         (addi
                            o21
                            i12))
                  with
                    true
                  then
                    work8
                      (addi
                         i12
                         1)
                  else
                    false
          in
          match
            eqi
              (length
                 s114)
              n21
          with
            true
          then
            work8
              0
          else
            false
in
recursive
  let ltString: [Char] -> [Char] -> Bool =
    lam s113.
      lam s213.
        match
          null
            s213
        with
          true
        then
          false
        else
          match
            null
              s113
          with
            true
          then
            true
          else
            match
              eqChar
                (head
                   s113)
                (head
                   s213)
            with
              true
            then
              ltString
                (tail
                   s113)
                (tail
                   s213)
            else
              ltChar
                (head
                   s113)
                (head
                   s213)
in
let gtString: [Char] -> [Char] -> Bool =
  lam s112.
    lam s212.
      ltString
        s212
        s112
in
let cmpString: [Char] -> [Char] -> Int =
  seqCmp
    cmpChar
in
let str2upper =
  lam s69.
    map
      char2upper
      s69
in
let str2lower =
  lam s68.
    map
      char2lower
      s68
in
let string2int =
  lam s66.
    recursive
      let string2int_rechelper1 =
        lam s67.
          lam acc69.
            match
              null
                s67
            with
              true
            then
              acc69
            else
              let fsd1 =
                subi
                  (char2int
                     (head
                        s67))
                  (char2int
                     '0')
              in
              string2int_rechelper1
                (tail
                   s67)
                (addi
                   (muli
                      10
                      acc69)
                   fsd1)
    in
    match
      s66
    with
      ""
    then
      0
    else
      match
        eqChar
          '-'
          (head
             s66)
      with
        true
      then
        negi
          (string2int_rechelper1
             (tail
                s66)
             0)
      else
        string2int_rechelper1
          s66
          0
in
let digit2char =
  lam d5.
    int2char
      (addi
         d5
         (char2int
            '0'))
in
let int2string =
  lam n6.
    recursive
      let int2string_rechelper =
        lam n7.
          lam acc68.
            match
              lti
                n7
                10
            with
              true
            then
              cons
                (digit2char
                   n7)
                acc68
            else
              int2string_rechelper
                (divi
                   n7
                   10)
                (cons
                   (digit2char
                      (modi
                         n7
                         10))
                   acc68)
    in
    match
      lti
        n6
        0
    with
      true
    then
      cons
        '-'
        (int2string_rechelper
           (negi
              n6)
           "")
    else
      int2string_rechelper
        n6
        ""
in
let stringIsInt: [Char] -> Bool =
  lam s65.
    eqString
      s65
      (int2string
         (string2int
            s65))
in
let strIndex =
  lam c6.
    lam s63.
      recursive
        let strIndex_rechelper =
          lam i11.
            lam c7.
              lam s64.
                match
                  null
                    s64
                with
                  true
                then
                  None
                    {}
                else
                  match
                    eqChar
                      c7
                      (head
                         s64)
                  with
                    true
                  then
                    Some
                      i11
                  else
                    strIndex_rechelper
                      (addi
                         i11
                         1)
                      c7
                      (tail
                         s64)
      in
      strIndex_rechelper
        0
        c6
        s63
in
let strLastIndex =
  lam c4.
    lam s61.
      recursive
        let strLastIndex_rechelper =
          lam i10.
            lam acc67.
              lam c5.
                lam s62.
                  match
                    null
                      s62
                  with
                    true
                  then
                    match
                      eqi
                        acc67
                        (negi
                           1)
                    with
                      true
                    then
                      None
                        {}
                    else
                      Some
                        acc67
                  else
                    match
                      eqChar
                        c5
                        (head
                           s62)
                    with
                      true
                    then
                      strLastIndex_rechelper
                        (addi
                           i10
                           1)
                        i10
                        c5
                        (tail
                           s62)
                    else
                      strLastIndex_rechelper
                        (addi
                           i10
                           1)
                        acc67
                        c5
                        (tail
                           s62)
      in
      strLastIndex_rechelper
        0
        (negi
           1)
        c4
        s61
in
let strSplit =
  lam delim2.
    lam s60.
      let n5 =
        length
          s60
      in
      let m14 =
        length
          delim2
      in
      recursive
        let work7 =
          lam acc66.
            lam lastMatch1.
              lam i9.
                match
                  lti
                    (subi
                       n5
                       m14)
                    i9
                with
                  true
                then
                  snoc
                    acc66
                    (subsequence
                       s60
                       lastMatch1
                       n5)
                else
                  match
                    eqStringSlice
                      delim2
                      s60
                      i9
                      m14
                  with
                    true
                  then
                    let nexti1 =
                      addi
                        i9
                        m14
                    in
                    work7
                      (snoc
                         acc66
                         (subsequence
                            s60
                            lastMatch1
                            (subi
                               i9
                               lastMatch1)))
                      nexti1
                      nexti1
                  else
                    work7
                      acc66
                      lastMatch1
                      (addi
                         i9
                         1)
      in
      match
        null
          delim2
      with
        true
      then
        [ s60 ]
      else
        work7
          ""
          0
          0
in
let strTrim =
  lam s58.
    recursive
      let strTrim_init1 =
        lam s59.
          match
            eqString
              s59
              ""
          with
            true
          then
            s59
          else
            match
              isWhitespace
                (head
                   s59)
            with
              true
            then
              strTrim_init1
                (tail
                   s59)
            else
              s59
    in
    reverse
      (strTrim_init1
         (reverse
            (strTrim_init1
               s58)))
in
let stringIsInt1 =
  lam s56.
    match
      null
        s56
    with
      true
    then
      false
    else
      let s57 =
        match
          eqChar
            (get
               s56
               0)
            '-'
        with
          true
        then
          tail
            s56
        else
          s56
      in
      forAll
        isDigit
        s57
in
recursive
  let strJoin =
    lam delim1.
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
                 delim1)
              (strJoin
                 delim1
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
  lam s55.
    match
      fileExists
        s55
    with
      true
    then
      deleteFile
        s55
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
  lam c3.
    lam s54.
      writeString
        c3
        s54
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
      (s53, false)
    then
      Some
        s53
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
  lam #var"79".
    match
      neqi
        (length
           argv)
        2
    with
      true
    then
      let #var"80" =
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
  lam res10.
    lam names4.
      lam filename1.
        lam expOnLogWeights1.
          match
            writeOpen1
              filename1
          with
            Some ch
          then
            let #var"75" =
              writeString1
                ch
                (strJoin
                   ","
                   names4)
            in
            let #var"76" =
              writeString1
                ch
                "\n"
            in
            let #var"77" =
              iter
                (lam lst.
                   let #var"78" =
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
                   res10)
            in
            writeClose
              ch
          else
            writeString1
              stderr
              (join
                 [ "Cannot write to file ",
                   filename1,
                   "\n" ])
in
let printStatistics =
  lam res9.
    lam names2.
      lam normConst3.
        lam expVals2.
          lam varianceVals1.
            let pad =
              18
            in
            let padPrint =
              lam s52.
                lam n4.
                  match
                    geqi
                      n4
                      (length
                         s52)
                  with
                    true
                  then
                    let #var"73" =
                      print
                        s52
                    in
                    print
                      (create
                         (subi
                            n4
                            (length
                               s52))
                         (lam #var"74".
                            ' '))
                  else
                    print
                      s52
            in
            let #var"61" =
              padPrint
                "Variable"
                14
            in
            let #var"62" =
              padPrint
                "Expected Value"
                pad
            in
            let #var"63" =
              padPrint
                "Variance"
                pad
            in
            let #var"64" =
              padPrint
                "Standard Deviation"
                pad
            in
            let #var"65" =
              print
                "\n"
            in
            recursive
              let work6 =
                lam names3.
                  lam ev.
                    lam vv.
                      match
                        (names3, ev, vv)
                      with
                        ([ n3 ] ++ ns4 ++ "", [ e1 ] ++ es1 ++ "", [ v4 ] ++ vs ++ "")
                      then
                        match
                          isPrefix
                            eqChar
                            "#"
                            n3
                        with
                          true
                        then
                          work6
                            ns4
                            ev
                            vv
                        else
                          let #var"68" =
                            padPrint
                              n3
                              14
                          in
                          let #var"69" =
                            padPrint
                              (float2string
                                 e1)
                              pad
                          in
                          let #var"70" =
                            padPrint
                              (float2string
                                 v4)
                              pad
                          in
                          let #var"71" =
                            padPrint
                              (float2string
                                 (sqrt
                                    v4))
                              pad
                          in
                          let #var"72" =
                            print
                              "\n"
                          in
                          work6
                            ns4
                            es1
                            vs
                      else
                        {}
            in
            let #var"66" =
              work6
                names2
                expVals2
                varianceVals1
            in
            let #var"67" =
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
let systematicSample: all a64. [a64] -> [Float] -> Float -> Int -> [a64] =
  lam seq2.
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
              lam seq3.
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
                            seq3
                            weights8
                            (addf
                               u
                               step)
                            (cons
                               (head
                                  seq3)
                               out1)
                        else
                          systematicSampleRec
                            (tail
                               seq3)
                            (tail
                               weights8)
                            (subf
                               u
                               (head
                                  weights8))
                            out1
          in
          systematicSampleRec
            seq2
            weights7
            (uniformContinuousSample
               0.
               step)
            (toList
               "")
in
let normConstant: [Float] -> Float =
  lam res8.
    let max1 =
      foldl
        (lam acc65.
           lam x55.
             match
               geqf
                 x55
                 acc65
             with
               true
             then
               x55
             else
               acc65)
        negInf
        res8
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
          (lam acc64.
             lam x54.
               addf
                 (exp
                    (subf
                       x54
                       max1))
                 acc64)
          0.
          res8
      in
      subf
        (addf
           max1
           (log
              sum1))
        (log
           (int2float
              (length
                 res8)))
in
let expectedValues =
  lam res7: [[Float]].
    lam normConst2.
      foldl
        (lam acc62.
           lam t733.
             let w4 =
               exp
                 (subf
                    (head
                       t733)
                    normConst2)
             in
             let ys =
               tail
                 t733
             in
             recursive
               let work5 =
                 lam acc63.
                   lam xs5.
                     match
                       (acc63, xs5)
                     with
                       ([ a63 ] ++ as1 ++ "", [ x53 ] ++ xs6 ++ "")
                     then
                       cons
                         (addf
                            (mulf
                               x53
                               w4)
                            a63)
                         (work5
                            as1
                            xs6)
                     else
                       ""
             in
             work5
               acc62
               ys)
        (create
           (subi
              (length
                 (head
                    res7))
              1)
           (lam #var"60".
              0.))
        res7
in
let variance =
  lam res6.
    lam expVals1.
      let sum =
        foldl
          (lam acc60.
             lam t732.
               recursive
                 let work4 =
                   lam acc61.
                     lam xs3.
                       lam expv.
                         match
                           (acc61, xs3, expv)
                         with
                           ([ a62 ] ++ as ++ "", [ x52 ] ++ xs4 ++ "", [ e ] ++ es ++ "")
                         then
                           let v3 =
                             subf
                               x52
                               e
                           in
                           cons
                             (addf
                                a62
                                (mulf
                                   v3
                                   v3))
                             (work4
                                as
                                xs4
                                es)
                         else
                           ""
               in
               work4
                 acc60
                 (tail
                    t732)
                 expVals1)
          (create
             (subi
                (length
                   (head
                      res6))
                1)
             (lam #var"59".
                0.))
          res6
      in
      let dval =
        int2float
          (length
             res6)
      in
      map
        (lam x51.
           divf
             x51
             dval)
        sum
in
let expOnLogWeights =
  lam res5.
    mapReverse
      (lam t731.
         match
           t731
         with
           [ x50 ] ++ xs2 ++ ""
         in
         cons
             (exp
                x50)
             xs2)
      res5
in
let output =
  lam res4: [[Float]].
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
             res4)
      in
      let expVals =
        expectedValues
          res4
          nc
      in
      let varianceVals =
        variance
          res4
          expVals
      in
      let #var"58" =
        printStatistics
          res4
          names1
          nc
          expVals
          varianceVals
      in
      saveCSV
        res4
        names1
        "data.csv"
        expOnLogWeights
in
let printSamples: all a61. (a61 -> [Char]) -> [Float] -> [a61] -> () =
  lam printFun1.
    lam weights4.
      lam samples10.
        recursive
          let rec16: [Float] -> [a61] -> () =
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
                  let w3 =
                    head
                      weights5
                  in
                  let weights6 =
                    tail
                      weights5
                  in
                  let s51 =
                    head
                      samples11
                  in
                  let samples12 =
                    tail
                      samples11
                  in
                  let #var"54" =
                    print
                      (printFun1
                         s51)
                  in
                  let #var"55" =
                    print
                      " "
                  in
                  let #var"56" =
                    print
                      (float2string
                         w3)
                  in
                  let #var"57" =
                    print
                      "\n"
                  in
                  rec16
                    weights6
                    samples12
        in
        match
          compileOptions.printSamples
        with
          true
        then
          rec16
            weights4
            samples10
        else
          {}
in
let printSamplesOption: all a60. (a60 -> [Char]) -> [Float] -> [Option a60] -> () =
  lam printFun.
    lam weights1.
      lam samples7.
        recursive
          let rec15: [Float] -> [Option a60] -> () =
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
                  let w2 =
                    head
                      weights2
                  in
                  let weights3 =
                    tail
                      weights2
                  in
                  let s49 =
                    head
                      samples8
                  in
                  let samples9 =
                    tail
                      samples8
                  in
                  let #var"50" =
                    match
                      s49
                    with
                      Some s50
                    then
                      print
                        (printFun
                           s50)
                    else
                      print
                        "."
                  in
                  let #var"51" =
                    print
                      " "
                  in
                  let #var"52" =
                    print
                      (float2string
                         w2)
                  in
                  let #var"53" =
                    print
                      "\n"
                  in
                  rec15
                    weights3
                    samples9
        in
        match
          compileOptions.printSamples
        with
          true
        then
          rec15
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
  lam n1.
    let #var"49" =
      modref
        _mcmcSamples
        n1
    in
    modref
      _mcmcAccepts
      0
in
let mcmcAccept =
  lam #var"48".
    modref
      _mcmcAccepts
      (addi
         (deref
            _mcmcAccepts)
         1)
in
let mcmcAcceptRate =
  lam #var"47".
    divf
      (int2float
         (deref
            _mcmcAccepts))
      (int2float
         (deref
            _mcmcSamples))
in
recursive
  let #var"RuntimeDistBase_sample": all a58. Dist a58 -> a58 =
    lam __sem_target20.
      let _20 =
        dprint
          __sem_target20
      in
      error
        "No matching case for function sample"
  let #var"RuntimeDistBase_logObserve": all a59. Dist a59 -> a59 -> Float =
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
  let #var"RuntimeDistElementary_sample": all a56. Dist a56 -> a56 =
    lam __sem_target18.
      match
        __sem_target18
      with
        RuntimeDistElementary_DistGamma t709
      then
        unsafeCoerce
          (gammaSample
             (t709.shape)
             (t709.scale))
      else
        match
          __sem_target18
        with
          RuntimeDistElementary_DistExponential t710
        then
          unsafeCoerce
            (exponentialSample
               (t710.rate))
        else
          match
            __sem_target18
          with
            RuntimeDistElementary_DistPoisson t711
          then
            unsafeCoerce
              (poissonSample
                 (t711.lambda))
          else
            match
              __sem_target18
            with
              RuntimeDistElementary_DistBinomial t712
            then
              unsafeCoerce
                (binomialSample
                   (t712.p)
                   (t712.n))
            else
              match
                __sem_target18
              with
                RuntimeDistElementary_DistBernoulli t713
              then
                unsafeCoerce
                  (bernoulliSample
                     (t713.p))
              else
                match
                  __sem_target18
                with
                  RuntimeDistElementary_DistBeta t714
                then
                  unsafeCoerce
                    (betaSample
                       (t714.a)
                       (t714.b))
                else
                  match
                    __sem_target18
                  with
                    RuntimeDistElementary_DistGaussian t715
                  then
                    unsafeCoerce
                      (gaussianSample
                         (t715.mu)
                         (t715.sigma))
                  else
                    match
                      __sem_target18
                    with
                      RuntimeDistElementary_DistMultinomial t716
                    then
                      unsafeCoerce
                        (multinomialSample
                           (t716.p)
                           (t716.n))
                    else
                      match
                        __sem_target18
                      with
                        RuntimeDistElementary_DistCategorical t717
                      then
                        unsafeCoerce
                          (categoricalSample
                             (t717.p))
                      else
                        match
                          __sem_target18
                        with
                          RuntimeDistElementary_DistDirichlet t718
                        then
                          unsafeCoerce
                            (dirichletSample
                               (t718.a))
                        else
                          match
                            __sem_target18
                          with
                            RuntimeDistElementary_DistUniform t719
                          then
                            unsafeCoerce
                              (uniformContinuousSample
                                 (t719.a)
                                 (t719.b))
                          else
                            let _18 =
                              dprint
                                __sem_target18
                            in
                            error
                              "No matching case for function sample"
  let #var"RuntimeDistElementary_logObserve": all a57. Dist a57 -> a57 -> Float =
    lam __sem_target19.
      match
        __sem_target19
      with
        RuntimeDistElementary_DistGamma t720
      then
        unsafeCoerce
          (gammaLogPdf
             (t720.shape)
             (t720.scale))
      else
        match
          __sem_target19
        with
          RuntimeDistElementary_DistExponential t721
        then
          unsafeCoerce
            (exponentialLogPdf
               (t721.rate))
        else
          match
            __sem_target19
          with
            RuntimeDistElementary_DistPoisson t722
          then
            unsafeCoerce
              (poissonLogPmf
                 (t722.lambda))
          else
            match
              __sem_target19
            with
              RuntimeDistElementary_DistBinomial t723
            then
              unsafeCoerce
                (binomialLogPmf
                   (t723.p)
                   (t723.n))
            else
              match
                __sem_target19
              with
                RuntimeDistElementary_DistBernoulli t724
              then
                unsafeCoerce
                  (bernoulliLogPmf
                     (t724.p))
              else
                match
                  __sem_target19
                with
                  RuntimeDistElementary_DistBeta t725
                then
                  unsafeCoerce
                    (betaLogPdf
                       (t725.a)
                       (t725.b))
                else
                  match
                    __sem_target19
                  with
                    RuntimeDistElementary_DistGaussian t726
                  then
                    unsafeCoerce
                      (gaussianLogPdf
                         (t726.mu)
                         (t726.sigma))
                  else
                    match
                      __sem_target19
                    with
                      RuntimeDistElementary_DistMultinomial t727
                    then
                      unsafeCoerce
                        (lam o1.
                           match
                             eqi
                               (t727.n)
                               (foldl1
                                  addi
                                  o1)
                           with
                             true
                           then
                             multinomialLogPmf
                               (t727.p)
                               o1
                           else
                             negf
                               inf)
                    else
                      match
                        __sem_target19
                      with
                        RuntimeDistElementary_DistCategorical t728
                      then
                        unsafeCoerce
                          (categoricalLogPmf
                             (t728.p))
                      else
                        match
                          __sem_target19
                        with
                          RuntimeDistElementary_DistDirichlet t729
                        then
                          unsafeCoerce
                            (dirichletLogPdf
                               (t729.a))
                        else
                          match
                            __sem_target19
                          with
                            RuntimeDistElementary_DistUniform t730
                          then
                            unsafeCoerce
                              (uniformContinuousLogPdf
                                 (t730.a)
                                 (t730.b))
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
  let #var"RuntimeDistEmpirical_sample": all a54. Dist a54 -> a54 =
    lam __sem_target10.
      match
        __sem_target10
      with
        RuntimeDistEmpirical_DistEmpirical t703
      then
        let x48 =
          uniformContinuousSample
            0.
            (last
               (t703.cumulativeWeights))
        in
        let cmp3 =
          lam y2.
            match
              ltf
                (subf
                   y2
                   x48)
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
            cmp3
            (t703.cumulativeWeights)
        with
          Some idx1
        then
          unsafeCoerce
            (get
               (t703.samples)
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
  let #var"RuntimeDistEmpirical_logObserve": all a55. Dist a55 -> a55 -> Float =
    lam __sem_target11.
      match
        __sem_target11
      with
        RuntimeDistEmpirical_DistEmpirical t704
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
        RuntimeDistEmpirical_DistEmpirical t705
      then
        (t705.samples, t705.logWeights)
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
        RuntimeDistEmpirical_DistEmpirical t706
      then
        match
          t706.extra
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
        RuntimeDistEmpirical_DistEmpirical t707
      then
        match
          t707.extra
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
        RuntimeDistEmpirical_DistEmpirical t708
      then
        t708.degenerate
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
                (lam acc59.
                   lam lw6.
                     match
                       geqf
                         lw6
                         acc59
                     with
                       true
                     then
                       lw6
                     else
                       acc59)
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
                      (lam acc58.
                         lam lw5.
                           addf
                             acc58
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
            let f24 =
              lam acc56.
                lam x49.
                  let acc57 =
                    addf
                      acc56
                      (exp
                         x49)
                  in
                  (acc57, acc57)
            in
            match
              mapAccumL
                f24
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
        RuntimeDistCombined_DistCombinedIndependent t701
      then
        unsafeCoerce
          (map
             #var"RuntimeDistCombined_sample"
             (t701.combined))
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
        RuntimeDistCombined_DistCombinedIndependent t702
      then
        unsafeCoerce
          (map
             #var"RuntimeDistCombined_logObserve"
             (t702.combined))
      else
        let _9 =
          dprint
            __sem_target9
        in
        error
          "No matching case for function logObserve"
in
recursive
  let #var"RuntimeDist_sample": all a52. Dist a52 -> a52 =
    lam __sem_target.
      match
        __sem_target
      with
        RuntimeDistCombined_DistCombinedIndependent t671
      then
        unsafeCoerce
          (map
             #var"RuntimeDist_sample"
             (t671.combined))
      else
        match
          __sem_target
        with
          RuntimeDistEmpirical_DistEmpirical t672
        then
          let x46 =
            uniformContinuousSample
              0.
              (last
                 (t672.cumulativeWeights))
          in
          let cmp2 =
            lam y1.
              match
                ltf
                  (subf
                     y1
                     x46)
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
              cmp2
              (t672.cumulativeWeights)
          with
            Some idx
          then
            unsafeCoerce
              (get
                 (t672.samples)
                 idx)
          else
            error
              "Sampling from empirical distribution failed"
        else
          match
            __sem_target
          with
            RuntimeDistElementary_DistGamma t673
          then
            unsafeCoerce
              (gammaSample
                 (t673.shape)
                 (t673.scale))
          else
            match
              __sem_target
            with
              RuntimeDistElementary_DistExponential t674
            then
              unsafeCoerce
                (exponentialSample
                   (t674.rate))
            else
              match
                __sem_target
              with
                RuntimeDistElementary_DistPoisson t675
              then
                unsafeCoerce
                  (poissonSample
                     (t675.lambda))
              else
                match
                  __sem_target
                with
                  RuntimeDistElementary_DistBinomial t676
                then
                  unsafeCoerce
                    (binomialSample
                       (t676.p)
                       (t676.n))
                else
                  match
                    __sem_target
                  with
                    RuntimeDistElementary_DistBernoulli t677
                  then
                    unsafeCoerce
                      (bernoulliSample
                         (t677.p))
                  else
                    match
                      __sem_target
                    with
                      RuntimeDistElementary_DistBeta t678
                    then
                      unsafeCoerce
                        (betaSample
                           (t678.a)
                           (t678.b))
                    else
                      match
                        __sem_target
                      with
                        RuntimeDistElementary_DistGaussian t679
                      then
                        unsafeCoerce
                          (gaussianSample
                             (t679.mu)
                             (t679.sigma))
                      else
                        match
                          __sem_target
                        with
                          RuntimeDistElementary_DistMultinomial t680
                        then
                          unsafeCoerce
                            (multinomialSample
                               (t680.p)
                               (t680.n))
                        else
                          match
                            __sem_target
                          with
                            RuntimeDistElementary_DistCategorical t681
                          then
                            unsafeCoerce
                              (categoricalSample
                                 (t681.p))
                          else
                            match
                              __sem_target
                            with
                              RuntimeDistElementary_DistDirichlet t682
                            then
                              unsafeCoerce
                                (dirichletSample
                                   (t682.a))
                            else
                              match
                                __sem_target
                              with
                                RuntimeDistElementary_DistUniform t683
                              then
                                unsafeCoerce
                                  (uniformContinuousSample
                                     (t683.a)
                                     (t683.b))
                              else
                                let #var"_" =
                                  dprint
                                    __sem_target
                                in
                                error
                                  "No matching case for function sample"
  let #var"RuntimeDist_logObserve": all a53. Dist a53 -> a53 -> Float =
    lam __sem_target1.
      match
        __sem_target1
      with
        RuntimeDistCombined_DistCombinedIndependent t684
      then
        unsafeCoerce
          (map
             #var"RuntimeDist_logObserve"
             (t684.combined))
      else
        match
          __sem_target1
        with
          RuntimeDistEmpirical_DistEmpirical t685
        then
          error
            "Log observe not supported for empirical distribution"
        else
          match
            __sem_target1
          with
            RuntimeDistElementary_DistGamma t686
          then
            unsafeCoerce
              (gammaLogPdf
                 (t686.shape)
                 (t686.scale))
          else
            match
              __sem_target1
            with
              RuntimeDistElementary_DistExponential t687
            then
              unsafeCoerce
                (exponentialLogPdf
                   (t687.rate))
            else
              match
                __sem_target1
              with
                RuntimeDistElementary_DistPoisson t688
              then
                unsafeCoerce
                  (poissonLogPmf
                     (t688.lambda))
              else
                match
                  __sem_target1
                with
                  RuntimeDistElementary_DistBinomial t689
                then
                  unsafeCoerce
                    (binomialLogPmf
                       (t689.p)
                       (t689.n))
                else
                  match
                    __sem_target1
                  with
                    RuntimeDistElementary_DistBernoulli t690
                  then
                    unsafeCoerce
                      (bernoulliLogPmf
                         (t690.p))
                  else
                    match
                      __sem_target1
                    with
                      RuntimeDistElementary_DistBeta t691
                    then
                      unsafeCoerce
                        (betaLogPdf
                           (t691.a)
                           (t691.b))
                    else
                      match
                        __sem_target1
                      with
                        RuntimeDistElementary_DistGaussian t692
                      then
                        unsafeCoerce
                          (gaussianLogPdf
                             (t692.mu)
                             (t692.sigma))
                      else
                        match
                          __sem_target1
                        with
                          RuntimeDistElementary_DistMultinomial t693
                        then
                          unsafeCoerce
                            (lam o.
                               match
                                 eqi
                                   (t693.n)
                                   (foldl1
                                      addi
                                      o)
                               with
                                 true
                               then
                                 multinomialLogPmf
                                   (t693.p)
                                   o
                               else
                                 negf
                                   inf)
                        else
                          match
                            __sem_target1
                          with
                            RuntimeDistElementary_DistCategorical t694
                          then
                            unsafeCoerce
                              (categoricalLogPmf
                                 (t694.p))
                          else
                            match
                              __sem_target1
                            with
                              RuntimeDistElementary_DistDirichlet t695
                            then
                              unsafeCoerce
                                (dirichletLogPdf
                                   (t695.a))
                            else
                              match
                                __sem_target1
                              with
                                RuntimeDistElementary_DistUniform t696
                              then
                                unsafeCoerce
                                  (uniformContinuousLogPdf
                                     (t696.a)
                                     (t696.b))
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
        RuntimeDistEmpirical_DistEmpirical t697
      then
        (t697.samples, t697.logWeights)
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
        RuntimeDistEmpirical_DistEmpirical t698
      then
        match
          t698.extra
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
        RuntimeDistEmpirical_DistEmpirical t699
      then
        match
          t699.extra
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
        RuntimeDistEmpirical_DistEmpirical t700
      then
        t700.degenerate
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
                (lam acc55.
                   lam lw3.
                     match
                       geqf
                         lw3
                         acc55
                     with
                       true
                     then
                       lw3
                     else
                       acc55)
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
                      (lam acc54.
                         lam lw2.
                           addf
                             acc54
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
            let f23 =
              lam acc52.
                lam x47.
                  let acc53 =
                    addf
                      acc52
                      (exp
                         x47)
                  in
                  (acc53, acc53)
            in
            match
              mapAccumL
                f23
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
let distEmpiricalSamples: all a51. Dist a51 -> ([a51], [Float]) =
  #var"RuntimeDist_empiricalSamples"
in
let distEmpiricalDegenerate: all a50. Dist a50 -> Bool =
  #var"RuntimeDist_empiricalDegenerate"
in
let distEmpiricalNormConst: all a49. Dist a49 -> Float =
  #var"RuntimeDist_empiricalNormConst"
in
let distEmpiricalAcceptRate: all a48. Dist a48 -> Float =
  #var"RuntimeDist_empiricalAcceptRate"
in
let distCombineIndependent: all a47. [Dist a47] -> Dist a47 =
  lam dists.
    RuntimeDistCombined_DistCombinedIndependent
      { combined =
          dists }
in
let sample: all a46. Dist a46 -> a46 =
  #var"RuntimeDist_sample"
in
let logObserve: all a45. Dist a45 -> a45 -> Float =
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
    lam state4.
      modref
        state4
        (addf
           (deref
              state4)
           weight1)
in
let stopFirstAssume =
  lam dist1.
    lam cont4.
      (Some
        dist1, cont4)
in
let stopInit =
  lam cont3.
    (None
      {}, cont3)
in
let run: all a42. all b3. Unknown -> (State -> (Option (Dist b3), b3 -> Checkpoint a42)) -> Dist a42 =
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
      let state3 =
        ref
          0.
      in
      type Stop a43 =
        {weight: Float, checkpoint: Checkpoint a43}
      in
      let start: (b3 -> Checkpoint a42) -> Float -> (() -> b3) -> Int -> Stop a42 =
        lam cont2.
          lam weight.
            lam sampleFun.
              lam #var"45".
                let #var"46" =
                  modref
                    state3
                    weight
                in
                let checkpoint1: Checkpoint a42 =
                  cont2
                    (sampleFun
                       {})
                in
                { weight =
                    deref
                      state3,
                  checkpoint =
                    checkpoint1 }
      in
      let propagate =
        lam particle.
          lam contWeight1.
            let #var"44" =
              modref
                state3
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
                    state3,
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
                (lam p14.
                   match
                     p14.checkpoint
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
                   (lam p15.
                      (p15.weight, match
                        p15.checkpoint
                      with
                        End a44
                      in
                      a44))
                   particles1)
            else
              let maxWeight =
                foldl
                  (lam acc51.
                     lam p19.
                       match
                         geqf
                           (p19.weight)
                           acc51
                       with
                         true
                       then
                         p19.weight
                       else
                         acc51)
                  (negf
                     inf)
                  particles1
              in
              let expWeights =
                reverse
                  (mapReverse
                     (lam p18.
                        exp
                          (subf
                             (p18.weight)
                             maxWeight))
                     particles1)
              in
              let sums =
                foldl
                  (lam acc50.
                     lam w1.
                       (addf
                         (acc50.0)
                         w1, addf
                         (acc50.1)
                         (mulf
                            w1
                            w1)))
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
                    (lam p16.
                       propagate
                         p16
                         (p16.weight))
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
                    (lam p17.
                       propagate
                         p17
                         contWeight)
                    resampled
                in
                runRec
                  particles3
      in
      match
        model
          state3
      with
        (d3, cont1)
      in
      let particles: [Stop a42] =
          match
            d3
          with
            Some d4
          then
            match
              d4
            with
              RuntimeDistEmpirical_DistEmpirical r3
            then
              match
                eqi
                  particleCount
                  (length
                     (r3.samples))
              with
                true
              then
                foldl21
                  (lam acc49.
                     lam s48.
                       lam lw.
                         cons
                           (start
                              cont1
                              lw
                              (lam #var"40".
                                 s48)
                              0)
                           acc49)
                  (toList
                     "")
                  (r3.samples)
                  (r3.logWeights)
              else
                createList
                  particleCount
                  (start
                     cont1
                     0.
                     (lam #var"41".
                        #var"RuntimeDist_sample"
                          d4))
            else
              createList
                particleCount
                (start
                   cont1
                   0.
                   (lam #var"42".
                      #var"RuntimeDist_sample"
                        d4))
          else
            createList
              particleCount
              (start
                 cont1
                 0.
                 (lam #var"43".
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
recursive
  let work3 =
    lam eq.
      lam s111.
        lam s211.
          match
            (s111, s211)
          with
            ([ h1 ] ++ t1100 ++ "", [ h2 ] ++ t2100 ++ "")
          then
            match
              eq
                h1
                h2
            with
              true
            then
              work3
                eq
                t1100
                t2100
            else
              false
          else
            true
in
recursive
  let work2 =
    lam p13.
      lam l5.
        lam r2.
          lam seq.
            match
              seq
            with
              ""
            then
              (l5, r2)
            else
              match
                seq
              with
                [ s47 ] ++ seq1 ++ ""
              in
              match
                  p13
                    s47
                with
                  true
                then
                  work2
                    p13
                    (cons
                       s47
                       l5)
                    r2
                    seq1
                else
                  work2
                    p13
                    l5
                    (cons
                       s47
                       r2)
                    seq1
in
recursive
  let t670 =
    lam cmp1.
      lam h.
        lam x45.
          lti
            (cmp1
               x45
               h)
            0
in
recursive
  let work1 =
    lam s110.
      lam s210.
        lam o2.
          lam n2.
            lam i8.
              match
                eqi
                  i8
                  n2
              with
                true
              then
                true
              else
                match
                  eqc
                    (get
                       s110
                       i8)
                    (get
                       s210
                       (addi
                          o2
                          i8))
                with
                  true
                then
                  work1
                    s110
                    s210
                    o2
                    n2
                    (addi
                       i8
                       1)
                else
                  false
in
recursive
  let string2int_rechelper =
    lam s46.
      lam acc48.
        match
          null
            s46
        with
          true
        then
          acc48
        else
          let fsd =
            subi
              (char2int
                 (head
                    s46))
              (char2int
                 '0')
          in
          string2int_rechelper
            (tail
               s46)
            (addi
               (muli
                  10
                  acc48)
               fsd)
in
recursive
  let work =
    lam delim.
      lam s45.
        lam n.
          lam m13.
            lam acc47.
              lam lastMatch.
                lam i7.
                  match
                    lti
                      (subi
                         n
                         m13)
                      i7
                  with
                    true
                  then
                    snoc
                      acc47
                      (subsequence
                         s45
                         lastMatch
                         n)
                  else
                    match
                      eqStringSlice
                        delim
                        s45
                        i7
                        m13
                    with
                      true
                    then
                      let nexti =
                        addi
                          i7
                          m13
                      in
                      work
                        delim
                        s45
                        n
                        m13
                        (snoc
                           acc47
                           (subsequence
                              s45
                              lastMatch
                              (subi
                                 i7
                                 lastMatch)))
                        nexti
                        nexti
                    else
                      work
                        delim
                        s45
                        n
                        m13
                        acc47
                        lastMatch
                        (addi
                           i7
                           1)
in
recursive
  let strTrim_init =
    lam s44.
      match
        eqString
          s44
          ""
      with
        true
      then
        s44
      else
        match
          isWhitespace
            (head
               s44)
        with
          true
        then
          strTrim_init
            (tail
               s44)
        else
          s44
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
external externalWriteFloatPipe : Int -> (Timespec, Float) -> ()
in
external externalReadDistFloatRecordPipe : Int -> Int -> [(Timespec, [(Float, Opaque)])]
in
let nanosPerSec =
  1000000000
in
let nanosToTimespec =
  lam nanosPerSec19.
    lam nanos.
      let s24 =
        divi
          nanos
          nanosPerSec19
      in
      let ns2 =
        modi
          nanos
          nanosPerSec19
      in
      (s24, ns2)
in
let timespecToNanos =
  lam nanosPerSec12.
    lam ts.
      match
        ts
      with
        (s12, ns)
      in
      addi
          (muli
             s12
             nanosPerSec12)
          ns
in
let addTimespec =
  lam nanosPerSec18.
    lam lhs1.
      lam rhs1.
        match
          (lhs1, rhs1)
        with
          ((ls1, lns1), (rs1, rns1))
        in
        let s25 =
            addi
              ls1
              rs1
          in
          let ns3 =
            addi
              lns1
              rns1
          in
          match
            geqi
              ns3
              nanosPerSec18
          with
            true
          then
            (addi
              s25
              1, subi
              ns3
              nanosPerSec18)
          else
            (s25, ns3)
in
let diffTimespec =
  lam nanosPerSec13.
    lam lhs.
      lam rhs.
        match
          (lhs, rhs)
        with
          ((ls, lns), (rs, rns))
        in
        let s13 =
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
              s13
              1, addi
              ns1
              nanosPerSec13)
          else
            (s13, ns1)
in
let cmpTimespec: Timespec -> Timespec -> Int =
  lam lhs2.
    lam rhs2.
      match
        (lhs2, rhs2)
      with
        ((ls2, lns2), (rs2, rns2))
      in
      match
          gti
            ls2
            rs2
        with
          true
        then
          1
        else
          match
            lti
              ls2
              rs2
          with
            true
          then
            negi
              1
          else
            match
              gti
                lns2
                rns2
            with
              true
            then
              1
            else
              match
                lti
                  lns2
                  rns2
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
  lam nanosPerSec25.
    lam monoLogicalTime6.
      lam wallLogicalTime22.
        lam delay1.
          let oldPriority =
            setMaxPriority
              {}
          in
          let intervalTime =
            nanosToTimespec
              nanosPerSec25
              delay1
          in
          let endTime =
            getMonotonicTime
              {}
          in
          let elapsedTime =
            diffTimespec
              nanosPerSec25
              endTime
              (deref
                 monoLogicalTime6)
          in
          let waitTime =
            addTimespec
              nanosPerSec25
              (deref
                 monoLogicalTime6)
              intervalTime
          in
          let overrun1 =
            let c2 =
              cmpTimespec
                intervalTime
                elapsedTime
            in
            match
              gti
                c2
                0
            with
              true
            then
              let #var"39" =
                clockNanosleep
                  waitTime
              in
              0
            else
              match
                lti
                  c2
                  0
              with
                true
              then
                let elapsedTime1 =
                  diffTimespec
                    nanosPerSec25
                    endTime
                    waitTime
                in
                timespecToNanos
                  nanosPerSec25
                  elapsedTime1
              else
                0
          in
          let #var"36" =
            modref
              monoLogicalTime6
              waitTime
          in
          let #var"37" =
            modref
              wallLogicalTime22
              (addTimespec
                 nanosPerSec25
                 (deref
                    wallLogicalTime22)
                 intervalTime)
          in
          let #var"38" =
            setPriority
              oldPriority
          in
          overrun1
in
type TSV a21 =
  (Timespec, a21)
in
let timestamp =
  lam nanosPerSec11.
    lam wallLogicalTime11.
      lam tsv2.
        let lt =
          deref
            wallLogicalTime11
        in
        timespecToNanos
          nanosPerSec11
          (diffTimespec
             nanosPerSec11
             (tsv2.0)
             lt)
in
let value: TSV Unknown -> Unknown =
  lam tsv3.
    tsv3.1
in
let tsv =
  lam nanosPerSec17.
    lam wallLogicalTime15.
      lam offset.
        lam value1.
          let lt1 =
            deref
              wallLogicalTime15
          in
          (addTimespec
            nanosPerSec17
            lt1
            (nanosToTimespec
               nanosPerSec17
               offset), value1)
in
let sdelay =
  lam nanosPerSec24.
    lam monoLogicalTime5.
      lam wallLogicalTime21.
        lam flushOutputs1.
          lam updateInputs1.
            lam delay.
              let #var"34" =
                flushOutputs1
                  {}
              in
              let overrun =
                delayBy
                  nanosPerSec24
                  monoLogicalTime5
                  wallLogicalTime21
                  delay
              in
              let #var"35" =
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
let rtpplReadDistFloatRecordPort =
  lam fd2.
    lam nfields.
      externalReadDistFloatRecordPipe
        fd2
        nfields
in
let t =
  lam fd1.
    lam msg.
      externalWriteFloatPipe
        fd1
        msg
in
let rtpplWriteFloatPort =
  lam fd.
    lam msgs.
      iter
        (t
           fd)
        msgs
in
let t1 =
  lam closeFileDescriptors2.
    lam #var"32".
      let #var"33" =
        closeFileDescriptors2
          {}
      in
      exit
        0
in
let rtpplRuntimeInit =
  lam monoLogicalTime4.
    lam wallLogicalTime20.
      lam updateInputSequences.
        lam closeFileDescriptors1.
          lam cont.
            let #var"27" =
              setSigintHandler
                (t1
                   closeFileDescriptors1)
            in
            let #var"28" =
              modref
                monoLogicalTime4
                (getMonotonicTime
                   {})
            in
            let #var"29" =
              modref
                wallLogicalTime20
                (getWallClockTime
                   {})
            in
            let #var"30" =
              updateInputSequences
                {}
            in
            let #var"31" =
              cont
                {}
            in
            {}
in
let fileDescriptors =
  { speed =
      openFileDescriptor
        "brake-speed",
    pos =
      openFileDescriptor
        "brake-pos",
    brake =
      openFileDescriptor
        "brake-brake" }
in
let closeFileDescriptors =
  lam fileDescriptors6.
    lam #var"26".
      let close_pos =
        closeFileDescriptor
          (fileDescriptors6.pos)
      in
      let close_speed =
        closeFileDescriptor
          (fileDescriptors6.speed)
      in
      let close_brake =
        closeFileDescriptor
          (fileDescriptors6.brake)
      in
      {}
in
let inputSeqs =
  ref
    { speed =
        "",
      pos =
        "" }
in
let outputSeqs =
  ref
    { brake =
        "" }
in
let t2 =
  lam tsv7.
    match
      tsv7
    with
      (ts5, v2)
    in
    (ts5, #var"RuntimeDistEmpirical_constructDistEmpiricalHelper"
        v2)
in
let t3 =
  lam tsv6.
    match
      tsv6
    with
      (ts4, v1)
    in
    (ts4, #var"RuntimeDistEmpirical_constructDistEmpiricalHelper"
        v1)
in
let updateInputs =
  lam fileDescriptors5.
    lam inputSeqs4.
      lam #var"25".
        modref
          inputSeqs4
          { speed =
              map
                t2
                (rtpplReadDistFloatRecordPort
                   (fileDescriptors5.speed)
                   2),
            pos =
              map
                t3
                (rtpplReadDistFloatRecordPort
                   (fileDescriptors5.pos)
                   3) }
in
let flushOutputs =
  lam fileDescriptors4.
    lam outputSeqs4.
      lam #var"23".
        let w_brake =
          rtpplWriteFloatPort
            (fileDescriptors4.brake)
            ((deref
                outputSeqs4).brake)
        in
        let #var"24" =
          modref
            outputSeqs4
            { brake =
                "" }
        in
        {}
in
let subInt =
  subi
in
let divInt =
  divi
in
let negInt =
  subi
    0
in
let ltInt =
  lti
in
let geqInt =
  geqi
in
let floorToInt =
  floorfi
in
let intToFloat =
  int2float
in
let push: [Unknown] -> Unknown -> [Unknown] =
  lam s43.
    lam elem.
      snoc
        s43
        elem
in
let sort1: (Unknown -> Unknown -> Int) -> [Unknown] -> [Unknown] =
  lam cmp.
    lam s42.
      quickSort
        cmp
        s42
in
recursive
  let range1: Int -> Int -> [Int] =
    lam lo.
      lam hi.
        match
          lti
            lo
            hi
        with
          true
        then
          cons
            lo
            (range1
               (addi
                  lo
                  1)
               hi)
        else
          ""
in
let convChar =
  lam c1.
    eqc
      c1
      '1'
in
let t4 =
  lam rows1.
    lam r1.
      map
        convChar
        (get
           rows1
           r1)
in
let readRoomMapRuntimeHelper =
  lam #var"22".
    let filename =
      get
        argv
        1
    in
    let s41 =
      strTrim
        (readFile
           filename)
    in
    match
      strSplit
        "\n"
        s41
    with
      [ coordsLine ] ++ rows ++ ""
    then
      match
        strSplit
          " "
          coordsLine
      with
        [ nrows,
          ncols ]
      then
        let nrows1 =
          string2int
            nrows
        in
        let ncols1 =
          string2int
            ncols
        in
        create
          nrows1
          (t4
             rows)
      else
        error
          "Invalid room map format"
    else
      error
        "Invalid room map format"
in
type RoomMap =
  {cols: Int, data: [[Bool]], rows: Int}
in
type Coordinate =
  {col: Int, row: Int}
in
type Line =
  {slope: Float, intercept: Float}
in
type Pos =
  {x: Float, y: Float, direction: Float}
in
type Offset =
  {angle: Float, distance: Float, direction: Float}
in
let roomBlockWidth =
  0.1
in
let maxSpeed =
  0.5
in
let wheelCircumference =
  0.35
in
let zeroOffset =
  { direction =
      0.,
    angle =
      0.,
    distance =
      0. }
in
let frontLeftOffset =
  { direction =
      0.,
    angle =
      subf
        0.
        0.209639845874,
    distance =
      0.240260275535 }
in
let frontRightOffset =
  { direction =
      0.,
    angle =
      0.209639845874,
    distance =
      0.240260275535 }
in
let rearLeftOffset =
  { direction =
      pi,
    angle =
      subf
        0.
        2.76725903758,
    distance =
      0.30083217913 }
in
let rearRightOffset =
  { direction =
      pi,
    angle =
      2.76725903758,
    distance =
      0.30083217913 }
in
let sideLeftOffset =
  { direction =
      divf
        (mulf
           3.
           pi)
        2.,
    angle =
      subf
        0.
        2.15879893034,
    distance =
      0.126194294641 }
in
let sideRightOffset =
  { direction =
      divf
        pi
        2.,
    angle =
      2.15879893034,
    distance =
      0.126194294641 }
in
let readRoomMap: () -> RoomMap =
  lam #var"21".
    let data =
      readRoomMapRuntimeHelper
        {}
    in
    { data =
        data,
      rows =
        length
          data,
      cols =
        length
          (get
             data
             0) }
in
let posToCoordinate =
  lam floorToInt10.
    lam roomBlockWidth10.
      lam p5.
        { row =
            floorToInt10
              (divf
                 (p5.y)
                 roomBlockWidth10),
          col =
            floorToInt10
              (divf
                 (p5.x)
                 roomBlockWidth10) }
in
let positionAtOffset: Pos -> Offset -> Pos =
  lam p9.
    lam ofs2.
      let p10 =
        { p9
          with
          x =
            addf
              (p9.x)
              (mulf
                 (ofs2.distance)
                 (cos
                    (addf
                       (p9.direction)
                       (ofs2.angle)))) }
      in
      let p11 =
        { p10
          with
          y =
            addf
              (p10.y)
              (mulf
                 (ofs2.distance)
                 (sin
                    (addf
                       (p10.direction)
                       (ofs2.angle)))) }
      in
      let p12 =
        { p11
          with
          direction =
            addf
              (p11.direction)
              (ofs2.direction) }
      in
      p12
in
let withinRoomBounds =
  lam ltInt9.
    lam geqInt9.
      lam floorToInt9.
        lam roomBlockWidth9.
          lam m9.
            lam p4.
              let c =
                posToCoordinate
                  floorToInt9
                  roomBlockWidth9
                  p4
              in
              let res =
                true
              in
              let res1 =
                match
                  match
                    match
                      match
                        ltInt9
                          (c.row)
                          0
                      with
                        true
                      then
                        true
                      else
                        geqInt9
                          (c.row)
                          (m9.rows)
                    with
                      true
                    then
                      true
                    else
                      ltInt9
                        (c.col)
                        0
                  with
                    true
                  then
                    true
                  else
                    geqInt9
                      (c.col)
                      (m9.cols)
                with
                  true
                then
                  let res2 =
                    false
                  in
                  res2
                else
                  let res3 =
                    not
                      (get
                         (get
                            (m9.data)
                            (c.row))
                         (c.col))
                  in
                  res3
              in
              res1
in
let t5 =
  lam ltInt8.
    lam geqInt8.
      lam floorToInt8.
        lam roomBlockWidth8.
          lam m8.
            lam center1.
              lam acc21.
                lam ofs1.
                  let acc22 =
                    match
                      acc21
                    with
                      true
                    then
                      let p3 =
                        positionAtOffset
                          center1
                          ofs1
                      in
                      let acc23 =
                        withinRoomBounds
                          ltInt8
                          geqInt8
                          floorToInt8
                          roomBlockWidth8
                          m8
                          p3
                      in
                      acc23
                    else
                      acc21
                  in
                  acc22
in
let carWithinRoomBounds =
  lam ltInt7.
    lam geqInt7.
      lam floorToInt7.
        lam roomBlockWidth7.
          lam frontLeftOffset7.
            lam frontRightOffset7.
              lam rearLeftOffset7.
                lam rearRightOffset7.
                  lam sideLeftOffset7.
                    lam sideRightOffset7.
                      lam m7.
                        lam center.
                          let sensorOffsets =
                            [ frontLeftOffset7,
                              frontRightOffset7,
                              rearLeftOffset7,
                              rearRightOffset7,
                              sideLeftOffset7,
                              sideRightOffset7 ]
                          in
                          let acc19 =
                            true
                          in
                          let acc20 =
                            foldl
                              (t5
                                 ltInt7
                                 geqInt7
                                 floorToInt7
                                 roomBlockWidth7
                                 m7
                                 center)
                              acc19
                              sensorOffsets
                          in
                          acc20
in
let t6 =
  lam m12.
    lam row1.
      lam blocks8.
        lam col.
          let blocks9 =
            match
              not
                (get
                   (get
                      (m12.data)
                      row1)
                   col)
            with
              true
            then
              let blocks10 =
                push
                  blocks8
                  { row =
                      row1,
                    col =
                      col }
              in
              blocks10
            else
              blocks8
          in
          blocks9
in
let t7 =
  lam m11.
    lam blocks6.
      lam row.
        let blocks7 =
          foldl
            (t6
               m11
               row)
            blocks6
            (range1
               0
               (m11.cols))
        in
        blocks7
in
let findFreeRoomCoordinates: RoomMap -> [Coordinate] =
  lam m10.
    let blocks4 =
      ""
    in
    let blocks5 =
      foldl
        (t7
           m10)
        blocks4
        (range1
           0
           (m10.rows))
    in
    blocks5
in
let timestampToSeconds =
  lam intToFloat9.
    lam ts1.
      divf
        (intToFloat9
           ts1)
        (intToFloat9
           1000000000)
in
let t8 =
  lam nanosPerSec10.
    lam wallLogicalTime10.
      lam intToFloat8.
        lam wheelCircumference6.
          lam m4.
            lam b1.
              lam sigma1.
                lam #var"14".
                  lam tsv1.
                    let ts2 =
                      timestampToSeconds
                        intToFloat8
                        (timestamp
                           nanosPerSec10
                           wallLogicalTime10
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
                           wheelCircumference6)
                        60.
                    in
                    let #var"15": () =
                      error
                        "Cannot use observe outside of inferred model"
                    in
                    {}
in
let speedModel =
  lam nanosPerSec9.
    lam wallLogicalTime9.
      lam intToFloat7.
        lam maxSpeed5.
          lam wheelCircumference5.
            lam speedObs.
              let m3 =
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
              let #var"13" =
                foldl
                  (t8
                     nanosPerSec9
                     wallLogicalTime9
                     intToFloat7
                     wheelCircumference5
                     m3
                     b
                     sigma)
                  {}
                  speedObs
              in
              { slope =
                  m3,
                intercept =
                  b }
in
let t9 =
  lam nanosPerSec23.
    lam wallLogicalTime19.
      lam ltInt13.
        lam acc44.
          lam tsv5.
            let acc45 =
              match
                ltInt13
                  (timestamp
                     nanosPerSec23
                     wallLogicalTime19
                     acc44)
                  (timestamp
                     nanosPerSec23
                     wallLogicalTime19
                     tsv5)
              with
                true
              then
                let acc46 =
                  tsv5
                in
                acc46
              else
                acc44
            in
            acc45
in
let maxDistLineTimestamp =
  lam nanosPerSec22.
    lam wallLogicalTime18.
      lam ltInt12.
        lam acc42.
          lam tsvs1.
            let acc43 =
              foldl
                (t9
                   nanosPerSec22
                   wallLogicalTime18
                   ltInt12)
                acc42
                tsvs1
            in
            acc43
in
let t10 =
  lam nanosPerSec21.
    lam wallLogicalTime17.
      lam ltInt11.
        lam acc39.
          lam tsv4.
            let acc40 =
              match
                ltInt11
                  (timestamp
                     nanosPerSec21
                     wallLogicalTime17
                     acc39)
                  (timestamp
                     nanosPerSec21
                     wallLogicalTime17
                     tsv4)
              with
                true
              then
                let acc41 =
                  tsv4
                in
                acc41
              else
                acc39
            in
            acc40
in
let maxDistPosTimestamp =
  lam nanosPerSec20.
    lam wallLogicalTime16.
      lam ltInt10.
        lam acc37.
          lam tsvs.
            let acc38 =
              foldl
                (t10
                   nanosPerSec20
                   wallLogicalTime16
                   ltInt10)
                acc37
                tsvs
            in
            acc38
in
let cmpFloat =
  lam negInt5.
    lam l4.
      lam r.
        let acc32 =
          0
        in
        let acc33 =
          match
            gtf
              l4
              r
          with
            true
          then
            let acc34 =
              1
            in
            acc34
          else
            let acc35 =
              match
                ltf
                  l4
                  r
              with
                true
              then
                let acc36 =
                  negInt5
                    1
                in
                acc36
              else
                acc32
            in
            acc35
        in
        acc33
in
let initialPositionModel =
  lam pi5.
    lam blocks3.
      let d2 =
        error
          "Cannot use assume outside of inferred model"
      in
      { x =
          0.8,
        y =
          5.55,
        direction =
          d2 }
in
recursive
  let loopFn1 =
    lam inBounds2.
      lam eps1.
        lam acc24.
          match
            inBounds2
              (acc24.p)
          with
            true
          then
            let p6 =
              acc24.p
            in
            let p7 =
              { p6
                with
                x =
                  addf
                    (p6.x)
                    (mulf
                       eps1
                       (cos
                          (p6.direction))) }
            in
            let p8 =
              { p7
                with
                y =
                  addf
                    (p7.y)
                    (mulf
                       eps1
                       (sin
                          (p7.direction))) }
            in
            let acc25 =
              { acc24
                with
                d =
                  addf
                    (acc24.d)
                    eps1 }
            in
            let acc26 =
              { acc25
                with
                p =
                  p8 }
            in
            loopFn1
              inBounds2
              eps1
              acc26
          else
            acc24
in
let estimateDistance: Offset -> Pos -> (Pos -> Bool) -> Float =
  lam ofs.
    lam p1.
      lam inBounds1.
        let eps =
          0.01
        in
        let p2 =
          positionAtOffset
            p1
            ofs
        in
        let acc17 =
          { d =
              0.,
            p =
              p2 }
        in
        let acc18 =
          loopFn1
            inBounds1
            eps
            acc17
        in
        acc18.d
in
let areaUnderLine =
  lam subInt8.
    lam intToFloat13.
      lam f15.
        lam a34.
          lam b2.
            let x2 =
              f15
                a34
            in
            let y =
              f15
                b2
            in
            let delta =
              timestampToSeconds
                intToFloat13
                (subInt8
                   b2
                   a34)
            in
            divf
              (addf
                 (mulf
                    x2
                    delta)
                 (mulf
                    y
                    delta))
              2.
in
let f =
  lam nanosPerSec16.
    lam wallLogicalTime14.
      lam subInt7.
        lam intToFloat12.
          lam line.
            lam t417.
              let lineTs =
                timestamp
                  nanosPerSec16
                  wallLogicalTime14
                  line
              in
              let ts3 =
                timestampToSeconds
                  intToFloat12
                  (subInt7
                     t417
                     lineTs)
              in
              let l2 =
                value
                  line
              in
              addf
                (mulf
                   (l2.slope)
                   ts3)
                (l2.intercept)
in
let brakeDistanceModel =
  lam nanosPerSec15.
    lam wallLogicalTime13.
      lam subInt6.
        lam ltInt6.
          lam geqInt6.
            lam floorToInt6.
              lam intToFloat11.
                lam roomBlockWidth6.
                  lam zeroOffset6.
                    lam frontLeftOffset6.
                      lam frontRightOffset6.
                        lam rearLeftOffset6.
                          lam rearRightOffset6.
                            lam sideLeftOffset6.
                              lam sideRightOffset6.
                                lam m6.
                                  lam pos.
                                    lam speed.
                                      let p =
                                        error
                                          "Cannot use assume outside of inferred model"
                                      in
                                      let speedLn =
                                        error
                                          "Cannot use assume outside of inferred model"
                                      in
                                      let speedFn =
                                        f
                                          nanosPerSec15
                                          wallLogicalTime13
                                          subInt6
                                          intToFloat11
                                          (tsv
                                             nanosPerSec15
                                             wallLogicalTime13
                                             (timestamp
                                                nanosPerSec15
                                                wallLogicalTime13
                                                speed)
                                             speedLn)
                                      in
                                      let dist =
                                        areaUnderLine
                                          subInt6
                                          intToFloat11
                                          speedFn
                                          (timestamp
                                             nanosPerSec15
                                             wallLogicalTime13
                                             pos)
                                          0
                                      in
                                      let currPos =
                                        { x =
                                            addf
                                              (p.x)
                                              (mulf
                                                 dist
                                                 (cos
                                                    (p.direction))),
                                          y =
                                            addf
                                              (p.y)
                                              (mulf
                                                 dist
                                                 (sin
                                                    (p.direction))),
                                          direction =
                                            p.direction }
                                      in
                                      let v =
                                        speedFn
                                          0
                                      in
                                      let brakeDist =
                                        mulf
                                          (mulf
                                             5.
                                             v)
                                          v
                                      in
                                      let inBounds =
                                        carWithinRoomBounds
                                          ltInt6
                                          geqInt6
                                          floorToInt6
                                          roomBlockWidth6
                                          frontLeftOffset6
                                          frontRightOffset6
                                          rearLeftOffset6
                                          rearRightOffset6
                                          sideLeftOffset6
                                          sideRightOffset6
                                          m6
                                      in
                                      let crashDist =
                                        estimateDistance
                                          zeroOffset6
                                          currPos
                                          inBounds
                                      in
                                      subf
                                        crashDist
                                        brakeDist
in
let t11 =
  lam xs1.
    lam s39.
      lam i6.
        let s40 =
          push
            s39
            (get
               xs1
               i6)
        in
        s40
in
let medianValue =
  lam divInt4.
    lam negInt4.
      lam x44.
        let s36 =
          ""
        in
        let xs =
          x44.s
        in
        let s37 =
          foldl
            (t11
               xs)
            s36
            (range1
               0
               (length
                  xs))
        in
        let s38 =
          sort1
            (cmpFloat
               negInt4)
            s37
        in
        get
          s38
          (divInt4
             (length
                s38)
             2)
in
let t12 =
  lam nanosPerSec8.
    lam wallLogicalTime8.
      lam intToFloat6.
        lam maxSpeed4.
          lam wheelCircumference4.
            lam #var"12".
              speedModel
                nanosPerSec8
                wallLogicalTime8
                intToFloat6
                maxSpeed4
                wheelCircumference4
                ""
in
let t13 =
  lam pi4.
    lam blocks2.
      lam #var"8".
        initialPositionModel
          pi4
          blocks2
in
let t14 =
  lam acc16: {pos: TSV Unknown, speed: TSV Unknown}.
    lam nanosPerSec14: Int.
      lam wallLogicalTime12: Ref (Timespec).
        lam subInt5: Int -> Int -> Int.
          lam ltInt5: Int -> Int -> Bool.
            lam geqInt5: Int -> Int -> Bool.
              lam floorToInt5: Float -> Int.
                lam intToFloat10: Int -> Float.
                  lam roomBlockWidth5: Float.
                    lam zeroOffset5: {angle: Float, distance: Float, direction: Float}.
                      lam frontLeftOffset5: {angle: Float, distance: Float, direction: Float}.
                        lam frontRightOffset5: {angle: Float, distance: Float, direction: Float}.
                          lam rearLeftOffset5: {angle: Float, distance: Float, direction: Float}.
                            lam rearRightOffset5: {angle: Float, distance: Float, direction: Float}.
                              lam sideLeftOffset5: {angle: Float, distance: Float, direction: Float}.
                                lam sideRightOffset5: {angle: Float, distance: Float, direction: Float}.
                                  lam m5: RoomMap.
                                    lam state2: State.
                                      stopInit
                                        (lam #var"18".
                                           let map2 =
                                             lam f22.
                                               let t656 =
                                                 lam s34.
                                                   recursive
                                                     let rec14 =
                                                       lam s35.
                                                         let t658 =
                                                           match
                                                             s35
                                                           with
                                                             ""
                                                           then
                                                             let t659 =
                                                               ""
                                                             in
                                                             t659
                                                           else
                                                             let t660 =
                                                               match
                                                                 s35
                                                               with
                                                                 [ a40 ]
                                                               then
                                                                 let t661 =
                                                                   f22
                                                                     a40
                                                                 in
                                                                 let t662 =
                                                                   [ t661 ]
                                                                 in
                                                                 t662
                                                               else
                                                                 let t663 =
                                                                   match
                                                                     s35
                                                                   with
                                                                     [ a41 ] ++ ss11 ++ ""
                                                                   then
                                                                     let t664 =
                                                                       cons
                                                                     in
                                                                     let t665 =
                                                                       f22
                                                                         a41
                                                                     in
                                                                     let t666 =
                                                                       t664
                                                                         t665
                                                                     in
                                                                     let t667 =
                                                                       rec14
                                                                         ss11
                                                                     in
                                                                     let t668 =
                                                                       t666
                                                                         t667
                                                                     in
                                                                     t668
                                                                   else
                                                                     let t669 =
                                                                       never
                                                                     in
                                                                     t669
                                                                 in
                                                                 t663
                                                             in
                                                             t660
                                                         in
                                                         t658
                                                   in
                                                   let t657 =
                                                     rec14
                                                       s34
                                                   in
                                                   t657
                                               in
                                               t656
                                           in
                                           let iter3 =
                                             lam f21.
                                               let t653 =
                                                 lam s33.
                                                   let t654 =
                                                     map2
                                                       f21
                                                   in
                                                   let #var"20" =
                                                     t654
                                                       s33
                                                   in
                                                   let t655 =
                                                     {}
                                                   in
                                                   t655
                                               in
                                               t653
                                           in
                                           let mapi2 =
                                             lam f20.
                                               let t629 =
                                                 lam s31.
                                                   recursive
                                                     let rec13 =
                                                       lam i5.
                                                         let t633 =
                                                           lam s32.
                                                             let t634 =
                                                               match
                                                                 s32
                                                               with
                                                                 ""
                                                               then
                                                                 let t635 =
                                                                   ""
                                                                 in
                                                                 t635
                                                               else
                                                                 let t636 =
                                                                   match
                                                                     s32
                                                                   with
                                                                     [ a38 ]
                                                                   then
                                                                     let t637 =
                                                                       f20
                                                                         i5
                                                                     in
                                                                     let t638 =
                                                                       t637
                                                                         a38
                                                                     in
                                                                     let t639 =
                                                                       [ t638 ]
                                                                     in
                                                                     t639
                                                                   else
                                                                     let t640 =
                                                                       match
                                                                         s32
                                                                       with
                                                                         [ a39 ] ++ ss10 ++ ""
                                                                       then
                                                                         let t641 =
                                                                           cons
                                                                         in
                                                                         let t642 =
                                                                           f20
                                                                             i5
                                                                         in
                                                                         let t643 =
                                                                           t642
                                                                             a39
                                                                         in
                                                                         let t644 =
                                                                           t641
                                                                             t643
                                                                         in
                                                                         let t645 =
                                                                           addi
                                                                         in
                                                                         let t646 =
                                                                           t645
                                                                             i5
                                                                         in
                                                                         let t647 =
                                                                           1
                                                                         in
                                                                         let t648 =
                                                                           t646
                                                                             t647
                                                                         in
                                                                         let t649 =
                                                                           rec13
                                                                             t648
                                                                         in
                                                                         let t650 =
                                                                           t649
                                                                             ss10
                                                                         in
                                                                         let t651 =
                                                                           t644
                                                                             t650
                                                                         in
                                                                         t651
                                                                       else
                                                                         let t652 =
                                                                           never
                                                                         in
                                                                         t652
                                                                     in
                                                                     t640
                                                                 in
                                                                 t636
                                                             in
                                                             t634
                                                         in
                                                         t633
                                                   in
                                                   let t630 =
                                                     0
                                                   in
                                                   let t631 =
                                                     rec13
                                                       t630
                                                   in
                                                   let t632 =
                                                     t631
                                                       s31
                                                   in
                                                   t632
                                               in
                                               t629
                                           in
                                           let iteri2 =
                                             lam f19.
                                               let t626 =
                                                 lam s30.
                                                   let t627 =
                                                     mapi2
                                                       f19
                                                   in
                                                   let #var"19" =
                                                     t627
                                                       s30
                                                   in
                                                   let t628 =
                                                     {}
                                                   in
                                                   t628
                                               in
                                               t626
                                           in
                                           let foldl3 =
                                             lam f18.
                                               let t614 =
                                                 lam acc30.
                                                   let t615 =
                                                     lam s28.
                                                       recursive
                                                         let rec12 =
                                                           lam acc31.
                                                             let t618 =
                                                               lam s29.
                                                                 let t619 =
                                                                   match
                                                                     s29
                                                                   with
                                                                     ""
                                                                   then
                                                                     acc31
                                                                   else
                                                                     let t620 =
                                                                       match
                                                                         s29
                                                                       with
                                                                         [ a37 ] ++ ss9 ++ ""
                                                                       then
                                                                         let t621 =
                                                                           f18
                                                                             acc31
                                                                         in
                                                                         let t622 =
                                                                           t621
                                                                             a37
                                                                         in
                                                                         let t623 =
                                                                           rec12
                                                                             t622
                                                                         in
                                                                         let t624 =
                                                                           t623
                                                                             ss9
                                                                         in
                                                                         t624
                                                                       else
                                                                         let t625 =
                                                                           never
                                                                         in
                                                                         t625
                                                                     in
                                                                     t620
                                                                 in
                                                                 t619
                                                             in
                                                             t618
                                                       in
                                                       let t616 =
                                                         rec12
                                                           acc30
                                                       in
                                                       let t617 =
                                                         t616
                                                           s28
                                                       in
                                                       t617
                                                   in
                                                   t615
                                               in
                                               t614
                                           in
                                           let foldr3 =
                                             lam f17.
                                               let t602 =
                                                 lam acc28.
                                                   let t603 =
                                                     lam s26.
                                                       recursive
                                                         let rec11 =
                                                           lam acc29.
                                                             let t606 =
                                                               lam s27.
                                                                 let t607 =
                                                                   match
                                                                     s27
                                                                   with
                                                                     ""
                                                                   then
                                                                     acc29
                                                                   else
                                                                     let t608 =
                                                                       match
                                                                         s27
                                                                       with
                                                                         [ a36 ] ++ ss8 ++ ""
                                                                       then
                                                                         let t609 =
                                                                           f17
                                                                             a36
                                                                         in
                                                                         let t610 =
                                                                           rec11
                                                                             acc29
                                                                         in
                                                                         let t611 =
                                                                           t610
                                                                             ss8
                                                                         in
                                                                         let t612 =
                                                                           t609
                                                                             t611
                                                                         in
                                                                         t612
                                                                       else
                                                                         let t613 =
                                                                           never
                                                                         in
                                                                         t613
                                                                     in
                                                                     t608
                                                                 in
                                                                 t607
                                                             in
                                                             t606
                                                       in
                                                       let t604 =
                                                         rec11
                                                           acc28
                                                       in
                                                       let t605 =
                                                         t604
                                                           s26
                                                       in
                                                       t605
                                                   in
                                                   t603
                                               in
                                               t602
                                           in
                                           let create2 =
                                             lam l3.
                                               let t578 =
                                                 lam f16.
                                                   recursive
                                                     let rec10 =
                                                       lam i4.
                                                         let t586 =
                                                           lam acc27.
                                                             let t587 =
                                                               geqi
                                                             in
                                                             let t588 =
                                                               t587
                                                                 i4
                                                             in
                                                             let t589 =
                                                               0
                                                             in
                                                             let t590 =
                                                               t588
                                                                 t589
                                                             in
                                                             let t591 =
                                                               match
                                                                 t590
                                                               with
                                                                 true
                                                               then
                                                                 let t592 =
                                                                   subi
                                                                 in
                                                                 let t593 =
                                                                   t592
                                                                     i4
                                                                 in
                                                                 let t594 =
                                                                   1
                                                                 in
                                                                 let t595 =
                                                                   t593
                                                                     t594
                                                                 in
                                                                 let t596 =
                                                                   rec10
                                                                     t595
                                                                 in
                                                                 let t597 =
                                                                   cons
                                                                 in
                                                                 let t598 =
                                                                   f16
                                                                     i4
                                                                 in
                                                                 let t599 =
                                                                   t597
                                                                     t598
                                                                 in
                                                                 let t600 =
                                                                   t599
                                                                     acc27
                                                                 in
                                                                 let t601 =
                                                                   t596
                                                                     t600
                                                                 in
                                                                 t601
                                                               else
                                                                 acc27
                                                             in
                                                             t591
                                                         in
                                                         t586
                                                   in
                                                   let t579 =
                                                     subi
                                                   in
                                                   let t580 =
                                                     t579
                                                       l3
                                                   in
                                                   let t581 =
                                                     1
                                                   in
                                                   let t582 =
                                                     t580
                                                       t581
                                                   in
                                                   let t583 =
                                                     rec10
                                                       t582
                                                   in
                                                   let t584 =
                                                     ""
                                                   in
                                                   let t585 =
                                                     t583
                                                       t584
                                                   in
                                                   t585
                                               in
                                               t578
                                           in
                                           let t282 =
                                             {}
                                           in
                                           let externalSin =
                                             lam a111.
                                               externalSin
                                                 a111
                                           in
                                           let sin =
                                             lam x43: Float.
                                               let t577 =
                                                 externalSin
                                                   x43
                                               in
                                               t577
                                           in
                                           let externalCos =
                                             lam a110.
                                               externalCos
                                                 a110
                                           in
                                           let cos =
                                             lam x42: Float.
                                               let t576 =
                                                 externalCos
                                                   x42
                                               in
                                               t576
                                           in
                                           let timestamp =
                                             lam nanosPerSec11.
                                               let t543 =
                                                 lam wallLogicalTime11.
                                                   let t544 =
                                                     lam tsv2.
                                                       let t545 =
                                                         deref
                                                       in
                                                       let lt =
                                                         t545
                                                           wallLogicalTime11
                                                       in
                                                       let nanosPerSec12 =
                                                         nanosPerSec11
                                                       in
                                                       let nanosPerSec13 =
                                                         nanosPerSec11
                                                       in
                                                       let lhs =
                                                         match
                                                           tsv2
                                                         with
                                                           (#var"X1",)
                                                         then
                                                           #var"X1"
                                                         else
                                                           let t575 =
                                                             never
                                                           in
                                                           t575
                                                       in
                                                       let rhs =
                                                         lt
                                                       in
                                                       let t546 =
                                                         (lhs, rhs)
                                                       in
                                                       let t547 =
                                                         match
                                                           t546
                                                         with
                                                           ((ls, lns), (rs, rns))
                                                         then
                                                           let t556 =
                                                             subi
                                                           in
                                                           let t557 =
                                                             t556
                                                               ls
                                                           in
                                                           let s13 =
                                                             t557
                                                               rs
                                                           in
                                                           let t558 =
                                                             subi
                                                           in
                                                           let t559 =
                                                             t558
                                                               lns
                                                           in
                                                           let ns1 =
                                                             t559
                                                               rns
                                                           in
                                                           let t560 =
                                                             lti
                                                           in
                                                           let t561 =
                                                             t560
                                                               ns1
                                                           in
                                                           let t562 =
                                                             0
                                                           in
                                                           let t563 =
                                                             t561
                                                               t562
                                                           in
                                                           let t564 =
                                                             match
                                                               t563
                                                             with
                                                               true
                                                             then
                                                               let t565 =
                                                                 addi
                                                               in
                                                               let t566 =
                                                                 t565
                                                                   ns1
                                                               in
                                                               let t567 =
                                                                 t566
                                                                   nanosPerSec13
                                                               in
                                                               let t568 =
                                                                 subi
                                                               in
                                                               let t569 =
                                                                 t568
                                                                   s13
                                                               in
                                                               let t570 =
                                                                 1
                                                               in
                                                               let t571 =
                                                                 t569
                                                                   t570
                                                               in
                                                               let t572 =
                                                                 (t571, t567)
                                                               in
                                                               t572
                                                             else
                                                               let t573 =
                                                                 (s13, ns1)
                                                               in
                                                               t573
                                                           in
                                                           t564
                                                         else
                                                           let t574 =
                                                             never
                                                           in
                                                           t574
                                                       in
                                                       let ts =
                                                         t547
                                                       in
                                                       let t548 =
                                                         match
                                                           ts
                                                         with
                                                           (s12, ns)
                                                         then
                                                           let t549 =
                                                             addi
                                                           in
                                                           let t550 =
                                                             muli
                                                           in
                                                           let t551 =
                                                             t550
                                                               s12
                                                           in
                                                           let t552 =
                                                             t551
                                                               nanosPerSec12
                                                           in
                                                           let t553 =
                                                             t549
                                                               t552
                                                           in
                                                           let t554 =
                                                             t553
                                                               ns
                                                           in
                                                           t554
                                                         else
                                                           let t555 =
                                                             never
                                                           in
                                                           t555
                                                       in
                                                       t548
                                                   in
                                                   t544
                                               in
                                               t543
                                           in
                                           let value =
                                             lam tsv3.
                                               let t541 =
                                                 match
                                                   tsv3
                                                 with
                                                   {#label"1" = #var"X"}
                                                 then
                                                   #var"X"
                                                 else
                                                   let t542 =
                                                     never
                                                   in
                                                   t542
                                               in
                                               t541
                                           in
                                           let positionAtOffset =
                                             lam p9.
                                               let t497 =
                                                 lam ofs2.
                                                   let t498 =
                                                     addf
                                                   in
                                                   let t499 =
                                                     match
                                                       p9
                                                     with
                                                       {x = x41}
                                                     then
                                                       x41
                                                     else
                                                       let t540 =
                                                         never
                                                       in
                                                       t540
                                                   in
                                                   let t500 =
                                                     t498
                                                       t499
                                                   in
                                                   let t501 =
                                                     mulf
                                                   in
                                                   let t502 =
                                                     match
                                                       ofs2
                                                     with
                                                       {distance = x40}
                                                     then
                                                       x40
                                                     else
                                                       let t539 =
                                                         never
                                                       in
                                                       t539
                                                   in
                                                   let t503 =
                                                     t501
                                                       t502
                                                   in
                                                   let t504 =
                                                     addf
                                                   in
                                                   let t505 =
                                                     match
                                                       p9
                                                     with
                                                       {direction = x39}
                                                     then
                                                       x39
                                                     else
                                                       let t538 =
                                                         never
                                                       in
                                                       t538
                                                   in
                                                   let t506 =
                                                     t504
                                                       t505
                                                   in
                                                   let t507 =
                                                     match
                                                       ofs2
                                                     with
                                                       {angle = x38}
                                                     then
                                                       x38
                                                     else
                                                       let t537 =
                                                         never
                                                       in
                                                       t537
                                                   in
                                                   let t508 =
                                                     t506
                                                       t507
                                                   in
                                                   let t509 =
                                                     cos
                                                       t508
                                                   in
                                                   let t510 =
                                                     t503
                                                       t509
                                                   in
                                                   let t511 =
                                                     t500
                                                       t510
                                                   in
                                                   let p10 =
                                                     { p9
                                                       with
                                                       x =
                                                         t511 }
                                                   in
                                                   let t512 =
                                                     addf
                                                   in
                                                   let t513 =
                                                     match
                                                       p10
                                                     with
                                                       {y = x37}
                                                     then
                                                       x37
                                                     else
                                                       let t536 =
                                                         never
                                                       in
                                                       t536
                                                   in
                                                   let t514 =
                                                     t512
                                                       t513
                                                   in
                                                   let t515 =
                                                     mulf
                                                   in
                                                   let t516 =
                                                     match
                                                       ofs2
                                                     with
                                                       {distance = x36}
                                                     then
                                                       x36
                                                     else
                                                       let t535 =
                                                         never
                                                       in
                                                       t535
                                                   in
                                                   let t517 =
                                                     t515
                                                       t516
                                                   in
                                                   let t518 =
                                                     addf
                                                   in
                                                   let t519 =
                                                     match
                                                       p10
                                                     with
                                                       {direction = x35}
                                                     then
                                                       x35
                                                     else
                                                       let t534 =
                                                         never
                                                       in
                                                       t534
                                                   in
                                                   let t520 =
                                                     t518
                                                       t519
                                                   in
                                                   let t521 =
                                                     match
                                                       ofs2
                                                     with
                                                       {angle = x34}
                                                     then
                                                       x34
                                                     else
                                                       let t533 =
                                                         never
                                                       in
                                                       t533
                                                   in
                                                   let t522 =
                                                     t520
                                                       t521
                                                   in
                                                   let t523 =
                                                     sin
                                                       t522
                                                   in
                                                   let t524 =
                                                     t517
                                                       t523
                                                   in
                                                   let t525 =
                                                     t514
                                                       t524
                                                   in
                                                   let p11 =
                                                     { p10
                                                       with
                                                       y =
                                                         t525 }
                                                   in
                                                   let t526 =
                                                     addf
                                                   in
                                                   let t527 =
                                                     match
                                                       p11
                                                     with
                                                       {direction = x33}
                                                     then
                                                       x33
                                                     else
                                                       let t532 =
                                                         never
                                                       in
                                                       t532
                                                   in
                                                   let t528 =
                                                     t526
                                                       t527
                                                   in
                                                   let t529 =
                                                     match
                                                       ofs2
                                                     with
                                                       {direction = x32}
                                                     then
                                                       x32
                                                     else
                                                       let t531 =
                                                         never
                                                       in
                                                       t531
                                                   in
                                                   let t530 =
                                                     t528
                                                       t529
                                                   in
                                                   let p12 =
                                                     { p11
                                                       with
                                                       direction =
                                                         t530 }
                                                   in
                                                   p12
                                               in
                                               t497
                                           in
                                           let timestampToSeconds =
                                             lam intToFloat9.
                                               let t490 =
                                                 lam ts1.
                                                   let t491 =
                                                     divf
                                                   in
                                                   let t492 =
                                                     intToFloat9
                                                       ts1
                                                   in
                                                   let t493 =
                                                     t491
                                                       t492
                                                   in
                                                   let t494 =
                                                     1000000000
                                                   in
                                                   let t495 =
                                                     intToFloat9
                                                       t494
                                                   in
                                                   let t496 =
                                                     t493
                                                       t495
                                                   in
                                                   t496
                                               in
                                               t490
                                           in
                                           recursive
                                             let loopFn1 =
                                               lam inBounds2.
                                                 let t453 =
                                                   lam eps1.
                                                     let t454 =
                                                       lam acc24.
                                                         let t455 =
                                                           match
                                                             acc24
                                                           with
                                                             {p = x31}
                                                           then
                                                             x31
                                                           else
                                                             let t489 =
                                                               never
                                                             in
                                                             t489
                                                         in
                                                         let t456 =
                                                           inBounds2
                                                             t455
                                                         in
                                                         let t457 =
                                                           match
                                                             t456
                                                           with
                                                             true
                                                           then
                                                             let p6 =
                                                               match
                                                                 acc24
                                                               with
                                                                 {p = x30}
                                                               then
                                                                 x30
                                                               else
                                                                 let t488 =
                                                                   never
                                                                 in
                                                                 t488
                                                             in
                                                             let t458 =
                                                               addf
                                                             in
                                                             let t459 =
                                                               match
                                                                 p6
                                                               with
                                                                 {x = x29}
                                                               then
                                                                 x29
                                                               else
                                                                 let t487 =
                                                                   never
                                                                 in
                                                                 t487
                                                             in
                                                             let t460 =
                                                               t458
                                                                 t459
                                                             in
                                                             let t461 =
                                                               mulf
                                                             in
                                                             let t462 =
                                                               t461
                                                                 eps1
                                                             in
                                                             let t463 =
                                                               match
                                                                 p6
                                                               with
                                                                 {direction = x28}
                                                               then
                                                                 x28
                                                               else
                                                                 let t486 =
                                                                   never
                                                                 in
                                                                 t486
                                                             in
                                                             let t464 =
                                                               cos
                                                                 t463
                                                             in
                                                             let t465 =
                                                               t462
                                                                 t464
                                                             in
                                                             let t466 =
                                                               t460
                                                                 t465
                                                             in
                                                             let p7 =
                                                               { p6
                                                                 with
                                                                 x =
                                                                   t466 }
                                                             in
                                                             let t467 =
                                                               addf
                                                             in
                                                             let t468 =
                                                               match
                                                                 p7
                                                               with
                                                                 {y = x27}
                                                               then
                                                                 x27
                                                               else
                                                                 let t485 =
                                                                   never
                                                                 in
                                                                 t485
                                                             in
                                                             let t469 =
                                                               t467
                                                                 t468
                                                             in
                                                             let t470 =
                                                               mulf
                                                             in
                                                             let t471 =
                                                               t470
                                                                 eps1
                                                             in
                                                             let t472 =
                                                               match
                                                                 p7
                                                               with
                                                                 {direction = x26}
                                                               then
                                                                 x26
                                                               else
                                                                 let t484 =
                                                                   never
                                                                 in
                                                                 t484
                                                             in
                                                             let t473 =
                                                               sin
                                                                 t472
                                                             in
                                                             let t474 =
                                                               t471
                                                                 t473
                                                             in
                                                             let t475 =
                                                               t469
                                                                 t474
                                                             in
                                                             let p8 =
                                                               { p7
                                                                 with
                                                                 y =
                                                                   t475 }
                                                             in
                                                             let t476 =
                                                               addf
                                                             in
                                                             let t477 =
                                                               match
                                                                 acc24
                                                               with
                                                                 {d = x25}
                                                               then
                                                                 x25
                                                               else
                                                                 let t483 =
                                                                   never
                                                                 in
                                                                 t483
                                                             in
                                                             let t478 =
                                                               t476
                                                                 t477
                                                             in
                                                             let t479 =
                                                               t478
                                                                 eps1
                                                             in
                                                             let acc25 =
                                                               { acc24
                                                                 with
                                                                 d =
                                                                   t479 }
                                                             in
                                                             let acc26 =
                                                               { acc25
                                                                 with
                                                                 p =
                                                                   p8 }
                                                             in
                                                             let t480 =
                                                               loopFn1
                                                                 inBounds2
                                                             in
                                                             let t481 =
                                                               t480
                                                                 eps1
                                                             in
                                                             let t482 =
                                                               t481
                                                                 acc26
                                                             in
                                                             t482
                                                           else
                                                             acc24
                                                         in
                                                         t457
                                                     in
                                                     t454
                                                 in
                                                 t453
                                           in
                                           let nanosPerSec4 =
                                             nanosPerSec14
                                           in
                                           let wallLogicalTime4 =
                                             wallLogicalTime12
                                           in
                                           let subInt3 =
                                             subInt5
                                           in
                                           let ltInt3 =
                                             ltInt5
                                           in
                                           let geqInt3 =
                                             geqInt5
                                           in
                                           let floorToInt3 =
                                             floorToInt5
                                           in
                                           let intToFloat3 =
                                             intToFloat10
                                           in
                                           let roomBlockWidth3 =
                                             roomBlockWidth5
                                           in
                                           let zeroOffset3 =
                                             zeroOffset5
                                           in
                                           let frontLeftOffset3 =
                                             frontLeftOffset5
                                           in
                                           let frontRightOffset3 =
                                             frontRightOffset5
                                           in
                                           let rearLeftOffset3 =
                                             rearLeftOffset5
                                           in
                                           let rearRightOffset3 =
                                             rearRightOffset5
                                           in
                                           let sideLeftOffset3 =
                                             sideLeftOffset5
                                           in
                                           let sideRightOffset3 =
                                             sideRightOffset5
                                           in
                                           let m1 =
                                             m5
                                           in
                                           let acc2 =
                                             acc16
                                           in
                                           let #var"3" =
                                             {}
                                           in
                                           let nanosPerSec15 =
                                             nanosPerSec4
                                           in
                                           let wallLogicalTime13 =
                                             wallLogicalTime4
                                           in
                                           let subInt6 =
                                             subInt3
                                           in
                                           let ltInt6 =
                                             ltInt3
                                           in
                                           let geqInt6 =
                                             geqInt3
                                           in
                                           let floorToInt6 =
                                             floorToInt3
                                           in
                                           let intToFloat11 =
                                             intToFloat3
                                           in
                                           let roomBlockWidth6 =
                                             roomBlockWidth3
                                           in
                                           let zeroOffset6 =
                                             zeroOffset3
                                           in
                                           let frontLeftOffset6 =
                                             frontLeftOffset3
                                           in
                                           let frontRightOffset6 =
                                             frontRightOffset3
                                           in
                                           let rearLeftOffset6 =
                                             rearLeftOffset3
                                           in
                                           let rearRightOffset6 =
                                             rearRightOffset3
                                           in
                                           let sideLeftOffset6 =
                                             sideLeftOffset3
                                           in
                                           let sideRightOffset6 =
                                             sideRightOffset3
                                           in
                                           let m6 =
                                             m1
                                           in
                                           let pos =
                                             match
                                               acc2
                                             with
                                               {pos = x24}
                                             then
                                               x24
                                             else
                                               let t452 =
                                                 never
                                               in
                                               t452
                                           in
                                           let speed =
                                             match
                                               acc2
                                             with
                                               {speed = x23}
                                             then
                                               x23
                                             else
                                               let t451 =
                                                 never
                                               in
                                               t451
                                           in
                                           let t283 =
                                             value
                                               pos
                                           in
                                           let p =
                                             sample
                                               t283
                                           in
                                           let t284 =
                                             value
                                               speed
                                           in
                                           let speedLn =
                                             sample
                                               t284
                                           in
                                           let nanosPerSec16 =
                                             nanosPerSec15
                                           in
                                           let wallLogicalTime14 =
                                             wallLogicalTime13
                                           in
                                           let subInt7 =
                                             subInt6
                                           in
                                           let intToFloat12 =
                                             intToFloat11
                                           in
                                           let nanosPerSec17 =
                                             nanosPerSec15
                                           in
                                           let wallLogicalTime15 =
                                             wallLogicalTime13
                                           in
                                           let t285 =
                                             timestamp
                                               nanosPerSec15
                                           in
                                           let t286 =
                                             t285
                                               wallLogicalTime13
                                           in
                                           let offset =
                                             t286
                                               speed
                                           in
                                           let value1 =
                                             speedLn
                                           in
                                           let t287 =
                                             deref
                                           in
                                           let lt1 =
                                             t287
                                               wallLogicalTime15
                                           in
                                           let nanosPerSec18 =
                                             nanosPerSec17
                                           in
                                           let lhs1 =
                                             lt1
                                           in
                                           let nanosPerSec19 =
                                             nanosPerSec17
                                           in
                                           let nanos =
                                             offset
                                           in
                                           let t288 =
                                             divi
                                           in
                                           let t289 =
                                             t288
                                               nanos
                                           in
                                           let s24 =
                                             t289
                                               nanosPerSec19
                                           in
                                           let t290 =
                                             modi
                                           in
                                           let t291 =
                                             t290
                                               nanos
                                           in
                                           let ns2 =
                                             t291
                                               nanosPerSec19
                                           in
                                           let t292 =
                                             (s24, ns2)
                                           in
                                           let rhs1 =
                                             t292
                                           in
                                           let t293 =
                                             (lhs1, rhs1)
                                           in
                                           let t294 =
                                             match
                                               t293
                                             with
                                               ((ls1, lns1), (rs1, rns1))
                                             then
                                               let t433 =
                                                 addi
                                               in
                                               let t434 =
                                                 t433
                                                   ls1
                                               in
                                               let s25 =
                                                 t434
                                                   rs1
                                               in
                                               let t435 =
                                                 addi
                                               in
                                               let t436 =
                                                 t435
                                                   lns1
                                               in
                                               let ns3 =
                                                 t436
                                                   rns1
                                               in
                                               let t437 =
                                                 geqi
                                               in
                                               let t438 =
                                                 t437
                                                   ns3
                                               in
                                               let t439 =
                                                 t438
                                                   nanosPerSec18
                                               in
                                               let t440 =
                                                 match
                                                   t439
                                                 with
                                                   true
                                                 then
                                                   let t441 =
                                                     subi
                                                   in
                                                   let t442 =
                                                     t441
                                                       ns3
                                                   in
                                                   let t443 =
                                                     t442
                                                       nanosPerSec18
                                                   in
                                                   let t444 =
                                                     addi
                                                   in
                                                   let t445 =
                                                     t444
                                                       s25
                                                   in
                                                   let t446 =
                                                     1
                                                   in
                                                   let t447 =
                                                     t445
                                                       t446
                                                   in
                                                   let t448 =
                                                     (t447, t443)
                                                   in
                                                   t448
                                                 else
                                                   let t449 =
                                                     (s25, ns3)
                                                   in
                                                   t449
                                               in
                                               t440
                                             else
                                               let t450 =
                                                 never
                                               in
                                               t450
                                           in
                                           let t295 =
                                             (t294, value1)
                                           in
                                           let line =
                                             t295
                                           in
                                           let t296 =
                                             lam t417.
                                               let t418 =
                                                 timestamp
                                                   nanosPerSec16
                                               in
                                               let t419 =
                                                 t418
                                                   wallLogicalTime14
                                               in
                                               let lineTs =
                                                 t419
                                                   line
                                               in
                                               let t420 =
                                                 timestampToSeconds
                                                   intToFloat12
                                               in
                                               let t421 =
                                                 subInt7
                                                   t417
                                               in
                                               let t422 =
                                                 t421
                                                   lineTs
                                               in
                                               let ts3 =
                                                 t420
                                                   t422
                                               in
                                               let l2 =
                                                 value
                                                   line
                                               in
                                               let t423 =
                                                 addf
                                               in
                                               let t424 =
                                                 mulf
                                               in
                                               let t425 =
                                                 match
                                                   l2
                                                 with
                                                   {slope = x22}
                                                 then
                                                   x22
                                                 else
                                                   let t432 =
                                                     never
                                                   in
                                                   t432
                                               in
                                               let t426 =
                                                 t424
                                                   t425
                                               in
                                               let t427 =
                                                 t426
                                                   ts3
                                               in
                                               let t428 =
                                                 t423
                                                   t427
                                               in
                                               let t429 =
                                                 match
                                                   l2
                                                 with
                                                   {intercept = x21}
                                                 then
                                                   x21
                                                 else
                                                   let t431 =
                                                     never
                                                   in
                                                   t431
                                               in
                                               let t430 =
                                                 t428
                                                   t429
                                               in
                                               t430
                                           in
                                           let speedFn =
                                             t296
                                           in
                                           let subInt8 =
                                             subInt6
                                           in
                                           let intToFloat13 =
                                             intToFloat11
                                           in
                                           let f15 =
                                             speedFn
                                           in
                                           let t297 =
                                             timestamp
                                               nanosPerSec15
                                           in
                                           let t298 =
                                             t297
                                               wallLogicalTime13
                                           in
                                           let a34 =
                                             t298
                                               pos
                                           in
                                           let b2 =
                                             0
                                           in
                                           let x2 =
                                             f15
                                               a34
                                           in
                                           let y =
                                             f15
                                               b2
                                           in
                                           let t299 =
                                             timestampToSeconds
                                               intToFloat13
                                           in
                                           let t300 =
                                             subInt8
                                               b2
                                           in
                                           let t301 =
                                             t300
                                               a34
                                           in
                                           let delta =
                                             t299
                                               t301
                                           in
                                           let t302 =
                                             divf
                                           in
                                           let t303 =
                                             addf
                                           in
                                           let t304 =
                                             mulf
                                           in
                                           let t305 =
                                             t304
                                               x2
                                           in
                                           let t306 =
                                             t305
                                               delta
                                           in
                                           let t307 =
                                             t303
                                               t306
                                           in
                                           let t308 =
                                             mulf
                                           in
                                           let t309 =
                                             t308
                                               y
                                           in
                                           let t310 =
                                             t309
                                               delta
                                           in
                                           let t311 =
                                             t307
                                               t310
                                           in
                                           let t312 =
                                             t302
                                               t311
                                           in
                                           let t313 =
                                             2.
                                           in
                                           let t314 =
                                             t312
                                               t313
                                           in
                                           let dist =
                                             t314
                                           in
                                           let t315 =
                                             match
                                               p
                                             with
                                               {direction = x20}
                                             then
                                               x20
                                             else
                                               let t416 =
                                                 never
                                               in
                                               t416
                                           in
                                           let t316 =
                                             addf
                                           in
                                           let t317 =
                                             match
                                               p
                                             with
                                               {y = x19}
                                             then
                                               x19
                                             else
                                               let t415 =
                                                 never
                                               in
                                               t415
                                           in
                                           let t318 =
                                             t316
                                               t317
                                           in
                                           let t319 =
                                             mulf
                                           in
                                           let t320 =
                                             t319
                                               dist
                                           in
                                           let t321 =
                                             match
                                               p
                                             with
                                               {direction = x18}
                                             then
                                               x18
                                             else
                                               let t414 =
                                                 never
                                               in
                                               t414
                                           in
                                           let t322 =
                                             sin
                                               t321
                                           in
                                           let t323 =
                                             t320
                                               t322
                                           in
                                           let t324 =
                                             t318
                                               t323
                                           in
                                           let t325 =
                                             addf
                                           in
                                           let t326 =
                                             match
                                               p
                                             with
                                               {x = x17}
                                             then
                                               x17
                                             else
                                               let t413 =
                                                 never
                                               in
                                               t413
                                           in
                                           let t327 =
                                             t325
                                               t326
                                           in
                                           let t328 =
                                             mulf
                                           in
                                           let t329 =
                                             t328
                                               dist
                                           in
                                           let t330 =
                                             match
                                               p
                                             with
                                               {direction = x16}
                                             then
                                               x16
                                             else
                                               let t412 =
                                                 never
                                               in
                                               t412
                                           in
                                           let t331 =
                                             cos
                                               t330
                                           in
                                           let t332 =
                                             t329
                                               t331
                                           in
                                           let t333 =
                                             t327
                                               t332
                                           in
                                           let currPos =
                                             { x =
                                                 t333,
                                               y =
                                                 t324,
                                               direction =
                                                 t315 }
                                           in
                                           let t334 =
                                             0
                                           in
                                           let v =
                                             speedFn
                                               t334
                                           in
                                           let t335 =
                                             mulf
                                           in
                                           let t336 =
                                             mulf
                                           in
                                           let t337 =
                                             5.
                                           in
                                           let t338 =
                                             t336
                                               t337
                                           in
                                           let t339 =
                                             t338
                                               v
                                           in
                                           let t340 =
                                             t335
                                               t339
                                           in
                                           let brakeDist =
                                             t340
                                               v
                                           in
                                           let ltInt7 =
                                             ltInt6
                                           in
                                           let geqInt7 =
                                             geqInt6
                                           in
                                           let floorToInt7 =
                                             floorToInt6
                                           in
                                           let roomBlockWidth7 =
                                             roomBlockWidth6
                                           in
                                           let frontLeftOffset7 =
                                             frontLeftOffset6
                                           in
                                           let frontRightOffset7 =
                                             frontRightOffset6
                                           in
                                           let rearLeftOffset7 =
                                             rearLeftOffset6
                                           in
                                           let rearRightOffset7 =
                                             rearRightOffset6
                                           in
                                           let sideLeftOffset7 =
                                             sideLeftOffset6
                                           in
                                           let sideRightOffset7 =
                                             sideRightOffset6
                                           in
                                           let m7 =
                                             m6
                                           in
                                           let t341 =
                                             lam center.
                                               let sensorOffsets =
                                                 [ frontLeftOffset7,
                                                   frontRightOffset7,
                                                   rearLeftOffset7,
                                                   rearRightOffset7,
                                                   sideLeftOffset7,
                                                   sideRightOffset7 ]
                                               in
                                               let acc19 =
                                                 true
                                               in
                                               let t351 =
                                                 foldl3
                                               in
                                               let ltInt8 =
                                                 ltInt7
                                               in
                                               let geqInt8 =
                                                 geqInt7
                                               in
                                               let floorToInt8 =
                                                 floorToInt7
                                               in
                                               let roomBlockWidth8 =
                                                 roomBlockWidth7
                                               in
                                               let m8 =
                                                 m7
                                               in
                                               let center1 =
                                                 center
                                               in
                                               let t352 =
                                                 lam acc21.
                                                   let t355 =
                                                     lam ofs1.
                                                       let acc22 =
                                                         match
                                                           acc21
                                                         with
                                                           true
                                                         then
                                                           let t356 =
                                                             positionAtOffset
                                                               center1
                                                           in
                                                           let p3 =
                                                             t356
                                                               ofs1
                                                           in
                                                           let ltInt9 =
                                                             ltInt8
                                                           in
                                                           let geqInt9 =
                                                             geqInt8
                                                           in
                                                           let floorToInt9 =
                                                             floorToInt8
                                                           in
                                                           let roomBlockWidth9 =
                                                             roomBlockWidth8
                                                           in
                                                           let m9 =
                                                             m8
                                                           in
                                                           let p4 =
                                                             p3
                                                           in
                                                           let floorToInt10 =
                                                             floorToInt9
                                                           in
                                                           let roomBlockWidth10 =
                                                             roomBlockWidth9
                                                           in
                                                           let p5 =
                                                             p4
                                                           in
                                                           let t357 =
                                                             divf
                                                           in
                                                           let t358 =
                                                             match
                                                               p5
                                                             with
                                                               {x = x15}
                                                             then
                                                               x15
                                                             else
                                                               let t411 =
                                                                 never
                                                               in
                                                               t411
                                                           in
                                                           let t359 =
                                                             t357
                                                               t358
                                                           in
                                                           let t360 =
                                                             t359
                                                               roomBlockWidth10
                                                           in
                                                           let t361 =
                                                             floorToInt10
                                                               t360
                                                           in
                                                           let t362 =
                                                             divf
                                                           in
                                                           let t363 =
                                                             match
                                                               p5
                                                             with
                                                               {y = x14}
                                                             then
                                                               x14
                                                             else
                                                               let t410 =
                                                                 never
                                                               in
                                                               t410
                                                           in
                                                           let t364 =
                                                             t362
                                                               t363
                                                           in
                                                           let t365 =
                                                             t364
                                                               roomBlockWidth10
                                                           in
                                                           let t366 =
                                                             floorToInt10
                                                               t365
                                                           in
                                                           let t367 =
                                                             { row =
                                                                 t366,
                                                               col =
                                                                 t361 }
                                                           in
                                                           let c =
                                                             t367
                                                           in
                                                           let res =
                                                             true
                                                           in
                                                           let t368 =
                                                             match
                                                               c
                                                             with
                                                               {row = x13}
                                                             then
                                                               x13
                                                             else
                                                               let t409 =
                                                                 never
                                                               in
                                                               t409
                                                           in
                                                           let t369 =
                                                             ltInt9
                                                               t368
                                                           in
                                                           let t370 =
                                                             0
                                                           in
                                                           let t371 =
                                                             t369
                                                               t370
                                                           in
                                                           let t372 =
                                                             match
                                                               t371
                                                             with
                                                               true
                                                             then
                                                               let t402 =
                                                                 true
                                                               in
                                                               t402
                                                             else
                                                               let t403 =
                                                                 match
                                                                   c
                                                                 with
                                                                   {row = x12}
                                                                 then
                                                                   x12
                                                                 else
                                                                   let t408 =
                                                                     never
                                                                   in
                                                                   t408
                                                               in
                                                               let t404 =
                                                                 geqInt9
                                                                   t403
                                                               in
                                                               let t405 =
                                                                 match
                                                                   m9
                                                                 with
                                                                   {rows = x11}
                                                                 then
                                                                   x11
                                                                 else
                                                                   let t407 =
                                                                     never
                                                                   in
                                                                   t407
                                                               in
                                                               let t406 =
                                                                 t404
                                                                   t405
                                                               in
                                                               t406
                                                           in
                                                           let t373 =
                                                             match
                                                               t372
                                                             with
                                                               true
                                                             then
                                                               let t396 =
                                                                 true
                                                               in
                                                               t396
                                                             else
                                                               let t397 =
                                                                 match
                                                                   c
                                                                 with
                                                                   {col = x10}
                                                                 then
                                                                   x10
                                                                 else
                                                                   let t401 =
                                                                     never
                                                                   in
                                                                   t401
                                                               in
                                                               let t398 =
                                                                 ltInt9
                                                                   t397
                                                               in
                                                               let t399 =
                                                                 0
                                                               in
                                                               let t400 =
                                                                 t398
                                                                   t399
                                                               in
                                                               t400
                                                           in
                                                           let t374 =
                                                             match
                                                               t373
                                                             with
                                                               true
                                                             then
                                                               let t389 =
                                                                 true
                                                               in
                                                               t389
                                                             else
                                                               let t390 =
                                                                 match
                                                                   c
                                                                 with
                                                                   {col = x9}
                                                                 then
                                                                   x9
                                                                 else
                                                                   let t395 =
                                                                     never
                                                                   in
                                                                   t395
                                                               in
                                                               let t391 =
                                                                 geqInt9
                                                                   t390
                                                               in
                                                               let t392 =
                                                                 match
                                                                   m9
                                                                 with
                                                                   {cols = x8}
                                                                 then
                                                                   x8
                                                                 else
                                                                   let t394 =
                                                                     never
                                                                   in
                                                                   t394
                                                               in
                                                               let t393 =
                                                                 t391
                                                                   t392
                                                               in
                                                               t393
                                                           in
                                                           let res1 =
                                                             match
                                                               t374
                                                             with
                                                               true
                                                             then
                                                               let res2 =
                                                                 false
                                                               in
                                                               res2
                                                             else
                                                               let t375 =
                                                                 get
                                                               in
                                                               let t376 =
                                                                 get
                                                               in
                                                               let t377 =
                                                                 match
                                                                   m9
                                                                 with
                                                                   {data = x7}
                                                                 then
                                                                   x7
                                                                 else
                                                                   let t388 =
                                                                     never
                                                                   in
                                                                   t388
                                                               in
                                                               let t378 =
                                                                 t376
                                                                   t377
                                                               in
                                                               let t379 =
                                                                 match
                                                                   c
                                                                 with
                                                                   {row = x6}
                                                                 then
                                                                   x6
                                                                 else
                                                                   let t387 =
                                                                     never
                                                                   in
                                                                   t387
                                                               in
                                                               let t380 =
                                                                 t378
                                                                   t379
                                                               in
                                                               let t381 =
                                                                 t375
                                                                   t380
                                                               in
                                                               let t382 =
                                                                 match
                                                                   c
                                                                 with
                                                                   {col = x5}
                                                                 then
                                                                   x5
                                                                 else
                                                                   let t386 =
                                                                     never
                                                                   in
                                                                   t386
                                                               in
                                                               let a35 =
                                                                 t381
                                                                   t382
                                                               in
                                                               let t383 =
                                                                 match
                                                                   a35
                                                                 with
                                                                   true
                                                                 then
                                                                   let t384 =
                                                                     false
                                                                   in
                                                                   t384
                                                                 else
                                                                   let t385 =
                                                                     true
                                                                   in
                                                                   t385
                                                               in
                                                               let res3 =
                                                                 t383
                                                               in
                                                               res3
                                                           in
                                                           let acc23 =
                                                             res1
                                                           in
                                                           acc23
                                                         else
                                                           acc21
                                                       in
                                                       acc22
                                                   in
                                                   t355
                                               in
                                               let t353 =
                                                 t351
                                                   t352
                                               in
                                               let t354 =
                                                 t353
                                                   acc19
                                               in
                                               let acc20 =
                                                 t354
                                                   sensorOffsets
                                               in
                                               acc20
                                           in
                                           let inBounds =
                                             t341
                                           in
                                           let ofs =
                                             zeroOffset6
                                           in
                                           let p1 =
                                             currPos
                                           in
                                           let inBounds1 =
                                             inBounds
                                           in
                                           let eps =
                                             0.01
                                           in
                                           let t342 =
                                             positionAtOffset
                                               p1
                                           in
                                           let p2 =
                                             t342
                                               ofs
                                           in
                                           let t343 =
                                             0.
                                           in
                                           let acc17 =
                                             { d =
                                                 t343,
                                               p =
                                                 p2 }
                                           in
                                           let t344 =
                                             loopFn1
                                               inBounds1
                                           in
                                           let t345 =
                                             t344
                                               eps
                                           in
                                           let acc18 =
                                             t345
                                               acc17
                                           in
                                           let t346 =
                                             match
                                               acc18
                                             with
                                               {d = x4}
                                             then
                                               x4
                                             else
                                               let t350 =
                                                 never
                                               in
                                               t350
                                           in
                                           let crashDist =
                                             t346
                                           in
                                           let t347 =
                                             subf
                                           in
                                           let t348 =
                                             t347
                                               crashDist
                                           in
                                           let t349 =
                                             t348
                                               brakeDist
                                           in
                                           (lam x3.
                                              End
                                                x3)
                                             t349)
in
let t15 =
  lam nanosPerSec7: Int.
    lam wallLogicalTime7: Ref (Timespec).
      lam intToFloat5: Int -> Float.
        lam maxSpeed3: Float.
          lam wheelCircumference3: Float.
            lam state1: State.
              stopInit
                (lam #var"11".
                   let map1 =
                     lam f14.
                       let t268 =
                         lam s22.
                           recursive
                             let rec9 =
                               lam s23.
                                 let t270 =
                                   match
                                     s23
                                   with
                                     ""
                                   then
                                     let t271 =
                                       ""
                                     in
                                     t271
                                   else
                                     let t272 =
                                       match
                                         s23
                                       with
                                         [ a32 ]
                                       then
                                         let t273 =
                                           f14
                                             a32
                                         in
                                         let t274 =
                                           [ t273 ]
                                         in
                                         t274
                                       else
                                         let t275 =
                                           match
                                             s23
                                           with
                                             [ a33 ] ++ ss7 ++ ""
                                           then
                                             let t276 =
                                               cons
                                             in
                                             let t277 =
                                               f14
                                                 a33
                                             in
                                             let t278 =
                                               t276
                                                 t277
                                             in
                                             let t279 =
                                               rec9
                                                 ss7
                                             in
                                             let t280 =
                                               t278
                                                 t279
                                             in
                                             t280
                                           else
                                             let t281 =
                                               never
                                             in
                                             t281
                                         in
                                         t275
                                     in
                                     t272
                                 in
                                 t270
                           in
                           let t269 =
                             rec9
                               s22
                           in
                           t269
                       in
                       t268
                   in
                   let iter1 =
                     lam f13.
                       let t265 =
                         lam s21.
                           let t266 =
                             map1
                               f13
                           in
                           let #var"17" =
                             t266
                               s21
                           in
                           let t267 =
                             {}
                           in
                           t267
                       in
                       t265
                   in
                   let mapi1 =
                     lam f12.
                       let t241 =
                         lam s19.
                           recursive
                             let rec8 =
                               lam i3.
                                 let t245 =
                                   lam s20.
                                     let t246 =
                                       match
                                         s20
                                       with
                                         ""
                                       then
                                         let t247 =
                                           ""
                                         in
                                         t247
                                       else
                                         let t248 =
                                           match
                                             s20
                                           with
                                             [ a30 ]
                                           then
                                             let t249 =
                                               f12
                                                 i3
                                             in
                                             let t250 =
                                               t249
                                                 a30
                                             in
                                             let t251 =
                                               [ t250 ]
                                             in
                                             t251
                                           else
                                             let t252 =
                                               match
                                                 s20
                                               with
                                                 [ a31 ] ++ ss6 ++ ""
                                               then
                                                 let t253 =
                                                   cons
                                                 in
                                                 let t254 =
                                                   f12
                                                     i3
                                                 in
                                                 let t255 =
                                                   t254
                                                     a31
                                                 in
                                                 let t256 =
                                                   t253
                                                     t255
                                                 in
                                                 let t257 =
                                                   addi
                                                 in
                                                 let t258 =
                                                   t257
                                                     i3
                                                 in
                                                 let t259 =
                                                   1
                                                 in
                                                 let t260 =
                                                   t258
                                                     t259
                                                 in
                                                 let t261 =
                                                   rec8
                                                     t260
                                                 in
                                                 let t262 =
                                                   t261
                                                     ss6
                                                 in
                                                 let t263 =
                                                   t256
                                                     t262
                                                 in
                                                 t263
                                               else
                                                 let t264 =
                                                   never
                                                 in
                                                 t264
                                             in
                                             t252
                                         in
                                         t248
                                     in
                                     t246
                                 in
                                 t245
                           in
                           let t242 =
                             0
                           in
                           let t243 =
                             rec8
                               t242
                           in
                           let t244 =
                             t243
                               s19
                           in
                           t244
                       in
                       t241
                   in
                   let iteri1 =
                     lam f11.
                       let t238 =
                         lam s18.
                           let t239 =
                             mapi1
                               f11
                           in
                           let #var"16" =
                             t239
                               s18
                           in
                           let t240 =
                             {}
                           in
                           t240
                       in
                       t238
                   in
                   let foldl2 =
                     lam f10.
                       let t226 =
                         lam acc14.
                           let t227 =
                             lam s16.
                               recursive
                                 let rec7 =
                                   lam acc15.
                                     let t230 =
                                       lam s17.
                                         let t231 =
                                           match
                                             s17
                                           with
                                             ""
                                           then
                                             acc15
                                           else
                                             let t232 =
                                               match
                                                 s17
                                               with
                                                 [ a29 ] ++ ss5 ++ ""
                                               then
                                                 let t233 =
                                                   f10
                                                     acc15
                                                 in
                                                 let t234 =
                                                   t233
                                                     a29
                                                 in
                                                 let t235 =
                                                   rec7
                                                     t234
                                                 in
                                                 let t236 =
                                                   t235
                                                     ss5
                                                 in
                                                 t236
                                               else
                                                 let t237 =
                                                   never
                                                 in
                                                 t237
                                             in
                                             t232
                                         in
                                         t231
                                     in
                                     t230
                               in
                               let t228 =
                                 rec7
                                   acc14
                               in
                               let t229 =
                                 t228
                                   s16
                               in
                               t229
                           in
                           t227
                       in
                       t226
                   in
                   let foldr2 =
                     lam f9.
                       let t214 =
                         lam acc12.
                           let t215 =
                             lam s14.
                               recursive
                                 let rec6 =
                                   lam acc13.
                                     let t218 =
                                       lam s15.
                                         let t219 =
                                           match
                                             s15
                                           with
                                             ""
                                           then
                                             acc13
                                           else
                                             let t220 =
                                               match
                                                 s15
                                               with
                                                 [ a28 ] ++ ss4 ++ ""
                                               then
                                                 let t221 =
                                                   f9
                                                     a28
                                                 in
                                                 let t222 =
                                                   rec6
                                                     acc13
                                                 in
                                                 let t223 =
                                                   t222
                                                     ss4
                                                 in
                                                 let t224 =
                                                   t221
                                                     t223
                                                 in
                                                 t224
                                               else
                                                 let t225 =
                                                   never
                                                 in
                                                 t225
                                             in
                                             t220
                                         in
                                         t219
                                     in
                                     t218
                               in
                               let t216 =
                                 rec6
                                   acc12
                               in
                               let t217 =
                                 t216
                                   s14
                               in
                               t217
                           in
                           t215
                       in
                       t214
                   in
                   let create1 =
                     lam l1.
                       let t190 =
                         lam f8.
                           recursive
                             let rec5 =
                               lam i2.
                                 let t198 =
                                   lam acc11.
                                     let t199 =
                                       geqi
                                     in
                                     let t200 =
                                       t199
                                         i2
                                     in
                                     let t201 =
                                       0
                                     in
                                     let t202 =
                                       t200
                                         t201
                                     in
                                     let t203 =
                                       match
                                         t202
                                       with
                                         true
                                       then
                                         let t204 =
                                           subi
                                         in
                                         let t205 =
                                           t204
                                             i2
                                         in
                                         let t206 =
                                           1
                                         in
                                         let t207 =
                                           t205
                                             t206
                                         in
                                         let t208 =
                                           rec5
                                             t207
                                         in
                                         let t209 =
                                           cons
                                         in
                                         let t210 =
                                           f8
                                             i2
                                         in
                                         let t211 =
                                           t209
                                             t210
                                         in
                                         let t212 =
                                           t211
                                             acc11
                                         in
                                         let t213 =
                                           t208
                                             t212
                                         in
                                         t213
                                       else
                                         acc11
                                     in
                                     t203
                                 in
                                 t198
                           in
                           let t191 =
                             subi
                           in
                           let t192 =
                             t191
                               l1
                           in
                           let t193 =
                             1
                           in
                           let t194 =
                             t192
                               t193
                           in
                           let t195 =
                             rec5
                               t194
                           in
                           let t196 =
                             ""
                           in
                           let t197 =
                             t195
                               t196
                           in
                           t197
                       in
                       t190
                   in
                   let t121 =
                     {}
                   in
                   let nanosPerSec8 =
                     nanosPerSec7
                   in
                   let wallLogicalTime8 =
                     wallLogicalTime7
                   in
                   let intToFloat6 =
                     intToFloat5
                   in
                   let maxSpeed4 =
                     maxSpeed3
                   in
                   let wheelCircumference4 =
                     wheelCircumference3
                   in
                   let #var"12" =
                     {}
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
                   let maxSpeed5 =
                     maxSpeed4
                   in
                   let wheelCircumference5 =
                     wheelCircumference4
                   in
                   let speedObs =
                     ""
                   in
                   let t122 =
                     0.
                   in
                   let t123 =
                     0.1
                   in
                   let t124 =
                     RuntimeDistElementary_DistGaussian
                       { mu =
                           t122,
                         sigma =
                           t123 }
                   in
                   let m3 =
                     sample
                       t124
                   in
                   let t125 =
                     0.
                   in
                   let t126 =
                     RuntimeDistElementary_DistUniform
                       { a =
                           t125,
                         b =
                           maxSpeed5 }
                   in
                   let b =
                     sample
                       t126
                   in
                   let t127 =
                     1.
                   in
                   let t128 =
                     1.
                   in
                   let t129 =
                     RuntimeDistElementary_DistGamma
                       { scale =
                           t128,
                         shape =
                           t127 }
                   in
                   let sigma =
                     sample
                       t129
                   in
                   let t130 =
                     foldl2
                   in
                   let nanosPerSec10 =
                     nanosPerSec9
                   in
                   let wallLogicalTime10 =
                     wallLogicalTime9
                   in
                   let intToFloat8 =
                     intToFloat7
                   in
                   let wheelCircumference6 =
                     wheelCircumference5
                   in
                   let m4 =
                     m3
                   in
                   let b1 =
                     b
                   in
                   let sigma1 =
                     sigma
                   in
                   let t131 =
                     lam #var"14".
                       let t136 =
                         lam tsv1.
                           let intToFloat9 =
                             intToFloat8
                           in
                           let nanosPerSec11 =
                             nanosPerSec10
                           in
                           let wallLogicalTime11 =
                             wallLogicalTime10
                           in
                           let tsv2 =
                             tsv1
                           in
                           let t137 =
                             deref
                           in
                           let lt =
                             t137
                               wallLogicalTime11
                           in
                           let nanosPerSec12 =
                             nanosPerSec11
                           in
                           let nanosPerSec13 =
                             nanosPerSec11
                           in
                           let lhs =
                             match
                               tsv2
                             with
                               (#var"X1",)
                             then
                               #var"X1"
                             else
                               let t189 =
                                 never
                               in
                               t189
                           in
                           let rhs =
                             lt
                           in
                           let t138 =
                             (lhs, rhs)
                           in
                           let t139 =
                             match
                               t138
                             with
                               ((ls, lns), (rs, rns))
                             then
                               let t170 =
                                 subi
                               in
                               let t171 =
                                 t170
                                   ls
                               in
                               let s13 =
                                 t171
                                   rs
                               in
                               let t172 =
                                 subi
                               in
                               let t173 =
                                 t172
                                   lns
                               in
                               let ns1 =
                                 t173
                                   rns
                               in
                               let t174 =
                                 lti
                               in
                               let t175 =
                                 t174
                                   ns1
                               in
                               let t176 =
                                 0
                               in
                               let t177 =
                                 t175
                                   t176
                               in
                               let t178 =
                                 match
                                   t177
                                 with
                                   true
                                 then
                                   let t179 =
                                     addi
                                   in
                                   let t180 =
                                     t179
                                       ns1
                                   in
                                   let t181 =
                                     t180
                                       nanosPerSec13
                                   in
                                   let t182 =
                                     subi
                                   in
                                   let t183 =
                                     t182
                                       s13
                                   in
                                   let t184 =
                                     1
                                   in
                                   let t185 =
                                     t183
                                       t184
                                   in
                                   let t186 =
                                     (t185, t181)
                                   in
                                   t186
                                 else
                                   let t187 =
                                     (s13, ns1)
                                   in
                                   t187
                               in
                               t178
                             else
                               let t188 =
                                 never
                               in
                               t188
                           in
                           let ts =
                             t139
                           in
                           let t140 =
                             match
                               ts
                             with
                               (s12, ns)
                             then
                               let t163 =
                                 addi
                               in
                               let t164 =
                                 muli
                               in
                               let t165 =
                                 t164
                                   s12
                               in
                               let t166 =
                                 t165
                                   nanosPerSec12
                               in
                               let t167 =
                                 t163
                                   t166
                               in
                               let t168 =
                                 t167
                                   ns
                               in
                               t168
                             else
                               let t169 =
                                 never
                               in
                               t169
                           in
                           let ts1 =
                             t140
                           in
                           let t141 =
                             divf
                           in
                           let t142 =
                             intToFloat9
                               ts1
                           in
                           let t143 =
                             t141
                               t142
                           in
                           let t144 =
                             1000000000
                           in
                           let t145 =
                             intToFloat9
                               t144
                           in
                           let t146 =
                             t143
                               t145
                           in
                           let ts2 =
                             t146
                           in
                           let tsv3 =
                             tsv1
                           in
                           let t147 =
                             match
                               tsv3
                             with
                               {#label"1" = #var"X"}
                             then
                               #var"X"
                             else
                               let t162 =
                                 never
                               in
                               t162
                           in
                           let rpm =
                             t147
                           in
                           let t148 =
                             divf
                           in
                           let t149 =
                             mulf
                           in
                           let t150 =
                             t149
                               rpm
                           in
                           let t151 =
                             t150
                               wheelCircumference6
                           in
                           let t152 =
                             t148
                               t151
                           in
                           let t153 =
                             60.
                           in
                           let mps =
                             t152
                               t153
                           in
                           let t154 =
                             addf
                           in
                           let t155 =
                             mulf
                           in
                           let t156 =
                             t155
                               m4
                           in
                           let t157 =
                             t156
                               ts2
                           in
                           let t158 =
                             t154
                               t157
                           in
                           let t159 =
                             t158
                               b1
                           in
                           let t160 =
                             RuntimeDistElementary_DistGaussian
                               { mu =
                                   t159,
                                 sigma =
                                   sigma1 }
                           in
                           let #var"15": () =
                             updateWeight
                               (logObserve
                                  t160
                                  mps)
                               state1
                           in
                           let t161 =
                             {}
                           in
                           t161
                       in
                       t136
                   in
                   let t132 =
                     t130
                       t131
                   in
                   let t133 =
                     {}
                   in
                   let t134 =
                     t132
                       t133
                   in
                   let #var"13" =
                     t134
                       speedObs
                   in
                   let t135 =
                     { slope =
                         m3,
                       intercept =
                         b }
                   in
                   (lam x1.
                      End
                        x1)
                     t135)
in
let t16 =
  lam blocks1: [Coordinate].
    lam pi3: Float.
      lam state: State.
        stopInit
          (lam #var"7".
             let map =
               lam f7.
                 let t107 =
                   lam s10.
                     recursive
                       let rec4 =
                         lam s11.
                           let t109 =
                             match
                               s11
                             with
                               ""
                             then
                               let t110 =
                                 ""
                               in
                               t110
                             else
                               let t111 =
                                 match
                                   s11
                                 with
                                   [ a26 ]
                                 then
                                   let t112 =
                                     f7
                                       a26
                                   in
                                   let t113 =
                                     [ t112 ]
                                   in
                                   t113
                                 else
                                   let t114 =
                                     match
                                       s11
                                     with
                                       [ a27 ] ++ ss3 ++ ""
                                     then
                                       let t115 =
                                         cons
                                       in
                                       let t116 =
                                         f7
                                           a27
                                       in
                                       let t117 =
                                         t115
                                           t116
                                       in
                                       let t118 =
                                         rec4
                                           ss3
                                       in
                                       let t119 =
                                         t117
                                           t118
                                       in
                                       t119
                                     else
                                       let t120 =
                                         never
                                       in
                                       t120
                                   in
                                   t114
                               in
                               t111
                           in
                           t109
                     in
                     let t108 =
                       rec4
                         s10
                     in
                     t108
                 in
                 t107
             in
             let iter =
               lam f6.
                 let t104 =
                   lam s9.
                     let t105 =
                       map
                         f6
                     in
                     let #var"10" =
                       t105
                         s9
                     in
                     let t106 =
                       {}
                     in
                     t106
                 in
                 t104
             in
             let mapi =
               lam f5.
                 let t80 =
                   lam s7.
                     recursive
                       let rec3 =
                         lam i1.
                           let t84 =
                             lam s8.
                               let t85 =
                                 match
                                   s8
                                 with
                                   ""
                                 then
                                   let t86 =
                                     ""
                                   in
                                   t86
                                 else
                                   let t87 =
                                     match
                                       s8
                                     with
                                       [ a24 ]
                                     then
                                       let t88 =
                                         f5
                                           i1
                                       in
                                       let t89 =
                                         t88
                                           a24
                                       in
                                       let t90 =
                                         [ t89 ]
                                       in
                                       t90
                                     else
                                       let t91 =
                                         match
                                           s8
                                         with
                                           [ a25 ] ++ ss2 ++ ""
                                         then
                                           let t92 =
                                             cons
                                           in
                                           let t93 =
                                             f5
                                               i1
                                           in
                                           let t94 =
                                             t93
                                               a25
                                           in
                                           let t95 =
                                             t92
                                               t94
                                           in
                                           let t96 =
                                             addi
                                           in
                                           let t97 =
                                             t96
                                               i1
                                           in
                                           let t98 =
                                             1
                                           in
                                           let t99 =
                                             t97
                                               t98
                                           in
                                           let t100 =
                                             rec3
                                               t99
                                           in
                                           let t101 =
                                             t100
                                               ss2
                                           in
                                           let t102 =
                                             t95
                                               t101
                                           in
                                           t102
                                         else
                                           let t103 =
                                             never
                                           in
                                           t103
                                       in
                                       t91
                                   in
                                   t87
                               in
                               t85
                           in
                           t84
                     in
                     let t81 =
                       0
                     in
                     let t82 =
                       rec3
                         t81
                     in
                     let t83 =
                       t82
                         s7
                     in
                     t83
                 in
                 t80
             in
             let iteri =
               lam f4.
                 let t77 =
                   lam s6.
                     let t78 =
                       mapi
                         f4
                     in
                     let #var"9" =
                       t78
                         s6
                     in
                     let t79 =
                       {}
                     in
                     t79
                 in
                 t77
             in
             let foldl =
               lam f3.
                 let t65 =
                   lam acc9.
                     let t66 =
                       lam s4.
                         recursive
                           let rec2 =
                             lam acc10.
                               let t69 =
                                 lam s5.
                                   let t70 =
                                     match
                                       s5
                                     with
                                       ""
                                     then
                                       acc10
                                     else
                                       let t71 =
                                         match
                                           s5
                                         with
                                           [ a23 ] ++ ss1 ++ ""
                                         then
                                           let t72 =
                                             f3
                                               acc10
                                           in
                                           let t73 =
                                             t72
                                               a23
                                           in
                                           let t74 =
                                             rec2
                                               t73
                                           in
                                           let t75 =
                                             t74
                                               ss1
                                           in
                                           t75
                                         else
                                           let t76 =
                                             never
                                           in
                                           t76
                                       in
                                       t71
                                   in
                                   t70
                               in
                               t69
                         in
                         let t67 =
                           rec2
                             acc9
                         in
                         let t68 =
                           t67
                             s4
                         in
                         t68
                     in
                     t66
                 in
                 t65
             in
             let foldr =
               lam f2.
                 let t53 =
                   lam acc7.
                     let t54 =
                       lam s2.
                         recursive
                           let rec1 =
                             lam acc8.
                               let t57 =
                                 lam s3.
                                   let t58 =
                                     match
                                       s3
                                     with
                                       ""
                                     then
                                       acc8
                                     else
                                       let t59 =
                                         match
                                           s3
                                         with
                                           [ a22 ] ++ ss ++ ""
                                         then
                                           let t60 =
                                             f2
                                               a22
                                           in
                                           let t61 =
                                             rec1
                                               acc8
                                           in
                                           let t62 =
                                             t61
                                               ss
                                           in
                                           let t63 =
                                             t60
                                               t62
                                           in
                                           t63
                                         else
                                           let t64 =
                                             never
                                           in
                                           t64
                                       in
                                       t59
                                   in
                                   t58
                               in
                               t57
                         in
                         let t55 =
                           rec1
                             acc7
                         in
                         let t56 =
                           t55
                             s2
                         in
                         t56
                     in
                     t54
                 in
                 t53
             in
             let create =
               lam l.
                 let t29 =
                   lam f1.
                     recursive
                       let rec =
                         lam i.
                           let t37 =
                             lam acc6.
                               let t38 =
                                 geqi
                               in
                               let t39 =
                                 t38
                                   i
                               in
                               let t40 =
                                 0
                               in
                               let t41 =
                                 t39
                                   t40
                               in
                               let t42 =
                                 match
                                   t41
                                 with
                                   true
                                 then
                                   let t43 =
                                     subi
                                   in
                                   let t44 =
                                     t43
                                       i
                                   in
                                   let t45 =
                                     1
                                   in
                                   let t46 =
                                     t44
                                       t45
                                   in
                                   let t47 =
                                     rec
                                       t46
                                   in
                                   let t48 =
                                     cons
                                   in
                                   let t49 =
                                     f1
                                       i
                                   in
                                   let t50 =
                                     t48
                                       t49
                                   in
                                   let t51 =
                                     t50
                                       acc6
                                   in
                                   let t52 =
                                     t47
                                       t51
                                   in
                                   t52
                                 else
                                   acc6
                               in
                               t42
                           in
                           t37
                     in
                     let t30 =
                       subi
                     in
                     let t31 =
                       t30
                         l
                     in
                     let t32 =
                       1
                     in
                     let t33 =
                       t31
                         t32
                     in
                     let t34 =
                       rec
                         t33
                     in
                     let t35 =
                       ""
                     in
                     let t36 =
                       t34
                         t35
                     in
                     t36
                 in
                 t29
             in
             let t19 =
               {}
             in
             let pi4 =
               pi3
             in
             let blocks2 =
               blocks1
             in
             let #var"8" =
               {}
             in
             let pi5 =
               pi4
             in
             let blocks3 =
               blocks2
             in
             let t20 =
               0.
             in
             let t21 =
               mulf
             in
             let t22 =
               2.
             in
             let t23 =
               t21
                 t22
             in
             let t24 =
               t23
                 pi5
             in
             let t25 =
               RuntimeDistElementary_DistUniform
                 { a =
                     t20,
                   b =
                     t24 }
             in
             let d2 =
               sample
                 t25
             in
             let t26 =
               5.55
             in
             let t27 =
               0.8
             in
             let t28 =
               { x =
                   t27,
                 y =
                   t26,
                 direction =
                   d2 }
             in
             (lam x.
                End
                  x)
               t28)
in
recursive
  let t18 =
    lam nanosPerSec4.
      lam wallLogicalTime4.
        lam subInt3.
          lam ltInt3.
            lam geqInt3.
              lam floorToInt3.
                lam intToFloat3.
                  lam roomBlockWidth3.
                    lam zeroOffset3.
                      lam frontLeftOffset3.
                        lam frontRightOffset3.
                          lam rearLeftOffset3.
                            lam rearRightOffset3.
                              lam sideLeftOffset3.
                                lam sideRightOffset3.
                                  lam m1.
                                    lam acc2.
                                      lam #var"3".
                                        brakeDistanceModel
                                          nanosPerSec4
                                          wallLogicalTime4
                                          subInt3
                                          ltInt3
                                          geqInt3
                                          floorToInt3
                                          intToFloat3
                                          roomBlockWidth3
                                          zeroOffset3
                                          frontLeftOffset3
                                          frontRightOffset3
                                          rearLeftOffset3
                                          rearRightOffset3
                                          sideLeftOffset3
                                          sideRightOffset3
                                          m1
                                          (acc2.pos)
                                          (acc2.speed)
  let loopFn =
    lam fileDescriptors3.
      lam inputSeqs3.
        lam outputSeqs3.
          lam nanosPerSec5.
            lam monoLogicalTime3.
              lam wallLogicalTime5.
                lam nanosPerSec6.
                  lam wallLogicalTime6.
                    lam subInt4.
                      lam divInt3.
                        lam negInt3.
                          lam ltInt4.
                            lam geqInt4.
                              lam floorToInt4.
                                lam intToFloat4.
                                  lam roomBlockWidth4.
                                    lam zeroOffset4.
                                      lam frontLeftOffset4.
                                        lam frontRightOffset4.
                                          lam rearLeftOffset4.
                                            lam rearRightOffset4.
                                              lam sideLeftOffset4.
                                                lam sideRightOffset4.
                                                  lam period1.
                                                    lam safetyMargin1.
                                                      lam m2.
                                                        lam acc3.
                                                          match
                                                            true
                                                          with
                                                            true
                                                          then
                                                            let posEst =
                                                              unsafeCoerce
                                                                ((deref
                                                                    inputSeqs3).pos)
                                                            in
                                                            let speedEst =
                                                              unsafeCoerce
                                                                ((deref
                                                                    inputSeqs3).speed)
                                                            in
                                                            let acc4 =
                                                              { acc3
                                                                with
                                                                pos =
                                                                  maxDistPosTimestamp
                                                                    nanosPerSec6
                                                                    wallLogicalTime6
                                                                    ltInt4
                                                                    (acc3.pos)
                                                                    posEst }
                                                            in
                                                            let acc5 =
                                                              { acc4
                                                                with
                                                                speed =
                                                                  maxDistLineTimestamp
                                                                    nanosPerSec6
                                                                    wallLogicalTime6
                                                                    ltInt4
                                                                    (acc4.speed)
                                                                    speedEst }
                                                            in
                                                            let d1 =
                                                              run
                                                                { particles =
                                                                    1000 }
                                                                (t14
                                                                   acc5
                                                                   nanosPerSec6
                                                                   wallLogicalTime6
                                                                   subInt4
                                                                   ltInt4
                                                                   geqInt4
                                                                   floorToInt4
                                                                   intToFloat4
                                                                   roomBlockWidth4
                                                                   zeroOffset4
                                                                   frontLeftOffset4
                                                                   frontRightOffset4
                                                                   rearLeftOffset4
                                                                   rearRightOffset4
                                                                   sideLeftOffset4
                                                                   sideRightOffset4
                                                                   m2)
                                                            in
                                                            let #var"4" =
                                                              match
                                                                ltf
                                                                  (medianValue
                                                                     divInt3
                                                                     negInt3
                                                                     (match
                                                                        distEmpiricalSamples
                                                                          d1
                                                                      with
                                                                        (s1, w)
                                                                      in
                                                                      { s =
                                                                            s1,
                                                                          w =
                                                                            w }))
                                                                  safetyMargin1
                                                              with
                                                                true
                                                              then
                                                                let #var"6" =
                                                                  let out =
                                                                    deref
                                                                      outputSeqs3
                                                                  in
                                                                  modref
                                                                    outputSeqs3
                                                                    { out
                                                                      with
                                                                      brake =
                                                                        cons
                                                                          (tsv
                                                                             nanosPerSec5
                                                                             wallLogicalTime5
                                                                             0
                                                                             0.)
                                                                          (out.brake) }
                                                                in
                                                                {}
                                                              else
                                                                {}
                                                            in
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
                                                            loopFn
                                                              fileDescriptors3
                                                              inputSeqs3
                                                              outputSeqs3
                                                              nanosPerSec5
                                                              monoLogicalTime3
                                                              wallLogicalTime5
                                                              nanosPerSec6
                                                              wallLogicalTime6
                                                              subInt4
                                                              divInt3
                                                              negInt3
                                                              ltInt4
                                                              geqInt4
                                                              floorToInt4
                                                              intToFloat4
                                                              roomBlockWidth4
                                                              zeroOffset4
                                                              frontLeftOffset4
                                                              frontRightOffset4
                                                              rearLeftOffset4
                                                              rearRightOffset4
                                                              sideLeftOffset4
                                                              sideRightOffset4
                                                              period1
                                                              safetyMargin1
                                                              m2
                                                              acc5
                                                          else
                                                            acc3
in
let #var"RTPPL_brakeEstimate" =
  lam fileDescriptors2.
    lam inputSeqs2.
      lam outputSeqs2.
        lam nanosPerSec2.
          lam monoLogicalTime2.
            lam wallLogicalTime2.
              lam pi2.
                lam nanosPerSec3.
                  lam wallLogicalTime3.
                    lam subInt2.
                      lam divInt2.
                        lam negInt2.
                          lam ltInt2.
                            lam geqInt2.
                              lam floorToInt2.
                                lam intToFloat2.
                                  lam roomBlockWidth2.
                                    lam maxSpeed2.
                                      lam wheelCircumference2.
                                        lam zeroOffset2.
                                          lam frontLeftOffset2.
                                            lam frontRightOffset2.
                                              lam rearLeftOffset2.
                                                lam rearRightOffset2.
                                                  lam sideLeftOffset2.
                                                    lam sideRightOffset2.
                                                      lam period.
                                                        lam safetyMargin.
                                                          let m =
                                                            readRoomMap
                                                              {}
                                                          in
                                                          let blocks =
                                                            findFreeRoomCoordinates
                                                              m
                                                          in
                                                          let initSpeed =
                                                            run
                                                              { particles =
                                                                  1000 }
                                                              (t15
                                                                 nanosPerSec3
                                                                 wallLogicalTime3
                                                                 intToFloat2
                                                                 maxSpeed2
                                                                 wheelCircumference2)
                                                          in
                                                          let d =
                                                            run
                                                              { particles =
                                                                  1000 }
                                                              (t16
                                                                 blocks
                                                                 pi2)
                                                          in
                                                          let acc =
                                                            { speed =
                                                                tsv
                                                                  nanosPerSec3
                                                                  wallLogicalTime3
                                                                  0
                                                                  initSpeed,
                                                              pos =
                                                                tsv
                                                                  nanosPerSec3
                                                                  wallLogicalTime3
                                                                  0
                                                                  d }
                                                          in
                                                          let #var"2" =
                                                            sdelay
                                                              nanosPerSec2
                                                              monoLogicalTime2
                                                              wallLogicalTime2
                                                              (flushOutputs
                                                                 fileDescriptors2
                                                                 outputSeqs2)
                                                              (updateInputs
                                                                 fileDescriptors2
                                                                 inputSeqs2)
                                                              5000000000
                                                          in
                                                          let acc1 =
                                                            loopFn
                                                              fileDescriptors2
                                                              inputSeqs2
                                                              outputSeqs2
                                                              nanosPerSec2
                                                              monoLogicalTime2
                                                              wallLogicalTime2
                                                              nanosPerSec3
                                                              wallLogicalTime3
                                                              subInt2
                                                              divInt2
                                                              negInt2
                                                              ltInt2
                                                              geqInt2
                                                              floorToInt2
                                                              intToFloat2
                                                              roomBlockWidth2
                                                              zeroOffset2
                                                              frontLeftOffset2
                                                              frontRightOffset2
                                                              rearLeftOffset2
                                                              rearRightOffset2
                                                              sideLeftOffset2
                                                              sideRightOffset2
                                                              period
                                                              safetyMargin
                                                              m
                                                              acc
                                                          in
                                                          {}
in
let t17 =
  lam fileDescriptors1.
    lam inputSeqs1.
      lam outputSeqs1.
        lam pi1.
          lam nanosPerSec1.
            lam monoLogicalTime1.
              lam wallLogicalTime1.
                lam subInt1.
                  lam divInt1.
                    lam negInt1.
                      lam ltInt1.
                        lam geqInt1.
                          lam floorToInt1.
                            lam intToFloat1.
                              lam roomBlockWidth1.
                                lam maxSpeed1.
                                  lam wheelCircumference1.
                                    lam zeroOffset1.
                                      lam frontLeftOffset1.
                                        lam frontRightOffset1.
                                          lam rearLeftOffset1.
                                            lam rearRightOffset1.
                                              lam sideLeftOffset1.
                                                lam sideRightOffset1.
                                                  lam #var"1".
                                                    #var"RTPPL_brakeEstimate"
                                                      fileDescriptors1
                                                      inputSeqs1
                                                      outputSeqs1
                                                      nanosPerSec1
                                                      monoLogicalTime1
                                                      wallLogicalTime1
                                                      pi1
                                                      nanosPerSec1
                                                      wallLogicalTime1
                                                      subInt1
                                                      divInt1
                                                      negInt1
                                                      ltInt1
                                                      geqInt1
                                                      floorToInt1
                                                      intToFloat1
                                                      roomBlockWidth1
                                                      maxSpeed1
                                                      wheelCircumference1
                                                      zeroOffset1
                                                      frontLeftOffset1
                                                      frontRightOffset1
                                                      rearLeftOffset1
                                                      rearRightOffset1
                                                      sideLeftOffset1
                                                      sideRightOffset1
                                                      250000000
                                                      0.1
in
rtpplRuntimeInit
  monoLogicalTime
  wallLogicalTime
  (updateInputs
     fileDescriptors
     inputSeqs)
  (closeFileDescriptors
     fileDescriptors)
  (t17
     fileDescriptors
     inputSeqs
     outputSeqs
     pi
     nanosPerSec
     monoLogicalTime
     wallLogicalTime
     subInt
     divInt
     negInt
     ltInt
     geqInt
     floorToInt
     intToFloat
     roomBlockWidth
     maxSpeed
     wheelCircumference
     zeroOffset
     frontLeftOffset
     frontRightOffset
     rearLeftOffset
     rearRightOffset
     sideLeftOffset
     sideRightOffset)