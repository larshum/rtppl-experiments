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
  lam a128.
    match
      a128
    with
      true
    then
      false
    else
      true
in
let and: Bool -> Bool -> Bool =
  lam a127.
    lam b49.
      match
        a127
      with
        true
      then
        b49
      else
        false
in
let or: Bool -> Bool -> Bool =
  lam a126.
    lam b48.
      match
        a126
      with
        true
      then
        true
      else
        b48
in
let xor: Bool -> Bool -> Bool =
  lam a125.
    lam b47.
      match
        a125
      with
        true
      then
        not
          b47
      else
        b47
in
let xnor: Bool -> Bool -> Bool =
  lam a124.
    lam b46.
      not
        (xor
           a124
           b46)
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
  lam b45.
    match
      b45
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
let optionEq: all a123. all b44. (a123 -> b44 -> Bool) -> Option a123 -> Option b44 -> Bool =
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
let optionMap: all a122. all b43. (a122 -> b43) -> Option a122 -> Option b43 =
  lam f46.
    lam o19.
      match
        o19
      with
        Some t212
      then
        Some
          (f46
             t212)
      else
        None
          {}
in
let optionMapAccum: all a120. all b41. all acc55. (acc55 -> a120 -> (acc55, b41)) -> acc55 -> Option a120 -> (acc55, Option b41) =
  lam f45.
    lam acc56.
      lam o18.
        match
          o18
        with
          Some a121
        then
          match
            f45
              acc56
              a121
          with
            (acc57, b42)
          in
          (acc57, Some
              b42)
        else
          (acc56, None
            {})
in
let optionJoin: all a119. Option (Option a119) -> Option a119 =
  lam o17.
    match
      o17
    with
      Some t209
    then
      t209
    else
      None
        {}
in
let optionBind: all a118. all b40. Option a118 -> (a118 -> Option b40) -> Option b40 =
  lam o16.
    lam f44.
      optionJoin
        (optionMap
           f44
           o16)
in
let optionCompose: all a117. all b39. all c40. (b39 -> Option c40) -> (a117 -> Option b39) -> a117 -> Option c40 =
  lam f43.
    lam g3.
      lam x68.
        optionBind
          (g3
             x68)
          f43
in
let optionZipWith: all a116. all b38. all c39. (a116 -> b38 -> c39) -> Option a116 -> Option b38 -> Option c39 =
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
let optionZipWithOrElse: all a115. all b37. all c38. (() -> c38) -> (a115 -> b37 -> c38) -> Option a115 -> Option b37 -> c38 =
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
let optionZipWithOr: all a114. all b36. all c37. c37 -> (a114 -> b36 -> c37) -> Option a114 -> Option b36 -> c37 =
  lam v4.
    optionZipWithOrElse
      (lam #var"80".
         v4)
in
let optionGetOrElse: all a113. (() -> a113) -> Option a113 -> a113 =
  lam d9.
    lam o10.
      match
        o10
      with
        Some t208
      then
        t208
      else
        d9
          {}
in
let optionGetOr: all a112. a112 -> Option a112 -> a112 =
  lam d8.
    optionGetOrElse
      (lam #var"79".
         d8)
in
let optionMapOrElse: all a111. all b35. (() -> b35) -> (a111 -> b35) -> Option a111 -> b35 =
  lam d7.
    lam f40.
      lam o9.
        optionGetOrElse
          d7
          (optionMap
             f40
             o9)
in
let optionMapOr: all a109. all b34. b34 -> (a109 -> b34) -> Option a109 -> b34 =
  lam d6.
    lam f39.
      lam o8.
        optionGetOr
          d6
          (optionMap
             f39
             o8)
in
let optionMapM: all a108. all b33. (a108 -> Option b33) -> [a108] -> Option [b33] =
  lam f38.
    lam l19.
      recursive
        let g2 =
          lam l20.
            lam acc54.
              match
                l20
              with
                [ hd ] ++ rest1 ++ ""
              then
                match
                  f38
                    hd
                with
                  Some x67
                then
                  g2
                    rest1
                    (snoc
                       acc54
                       x67)
                else
                  None
                    {}
              else
                Some
                  acc54
      in
      g2
        l19
        ""
in
let optionFoldlM: all a105. all b31. (a105 -> b31 -> Option a105) -> a105 -> [b31] -> Option a105 =
  lam f37.
    recursive
      let recur =
        lam a106.
          lam bs1.
            match
              bs1
            with
              [ b32 ] ++ bs2 ++ ""
            then
              let res19 =
                f37
                  a106
                  b32
              in
              match
                res19
              with
                Some a107
              then
                recur
                  a107
                  bs2
              else
                match
                  res19
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
                  a106
    in
    recur
in
let optionContains: all a104. Option a104 -> (a104 -> Bool) -> Bool =
  lam o7.
    lam p20.
      optionMapOr
        false
        p20
        o7
in
let optionIsSome: all a103. Option a103 -> Bool =
  lam o6.
    optionContains
      o6
      (lam #var"78".
         true)
in
let optionIsNone: all a102. Option a102 -> Bool =
  lam o5.
    not
      (optionIsSome
         o5)
in
let optionAnd: all a101. Option a101 -> Option a101 -> Option a101 =
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
let optionFilter: all a100. (a100 -> Bool) -> Option a100 -> Option a100 =
  lam p19.
    lam o4.
      match
        optionContains
          o4
          p19
      with
        true
      then
        o4
      else
        None
          {}
in
let optionOrElse: all a99. (() -> Option a99) -> Option a99 -> Option a99 =
  lam f36.
    lam o3.
      optionGetOrElse
        f36
        (optionMap
           (lam x66.
              Some
                x66)
           o3)
in
let optionOr: all a98. Option a98 -> Option a98 -> Option a98 =
  lam o12.
    lam o22.
      optionOrElse
        (lam #var"77".
           o22)
        o12
in
let optionXor: all a97. Option a97 -> Option a97 -> Option a97 =
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
  lam n20.
    lam v3.
      create
        n20
        (lam #var"76".
           v3)
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
let eqSeq: all a96. all b30. (a96 -> b30 -> Bool) -> [a96] -> [b30] -> Bool =
  lam eq4.
    lam s119.
      lam s219.
        recursive
          let work15 =
            lam s120.
              lam s220.
                match
                  (s120, s220)
                with
                  ([ h12 ] ++ t1101 ++ "", [ h22 ] ++ t211 ++ "")
                then
                  match
                    eq4
                      h12
                      h22
                  with
                    true
                  then
                    work15
                      t1101
                      t211
                  else
                    false
                else
                  true
        in
        let n110 =
          length
            s119
        in
        let n23 =
          length
            s219
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
          work15
            s119
            s219
        else
          false
in
let toRope =
  lam seq34.
    createRope
      (length
         seq34)
      (lam i17.
         get
           seq34
           i17)
in
let toList =
  lam seq33.
    createList
      (length
         seq33)
      (lam i16.
         get
           seq33
           i16)
in
let mapOption: all a94. all b28. (a94 -> Option b28) -> [a94] -> [b28] =
  lam f35.
    recursive
      let work14 =
        lam as3.
          match
            as3
          with
            [ a95 ] ++ as4 ++ ""
          then
            match
              f35
                a95
            with
              Some b29
            then
              cons
                b29
                (work14
                   as4)
            else
              work14
                as4
          else
            ""
    in
    work14
in
let for_: all a93. [a93] -> (a93 -> ()) -> () =
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
        (lam acc53.
           lam x65.
             cons
               (f33
                  x65)
               acc53)
        (toList
           "")
        lst1
in
let mapK: all a92. all b27. all c36. (a92 -> (b27 -> c36) -> c36) -> [a92] -> ([b27] -> c36) -> c36 =
  lam f32.
    lam seq32.
      lam k2.
        foldl
          (lam k3.
             lam x63.
               lam xs8.
                 f32
                   x63
                   (lam x64.
                      k3
                        (cons
                           x64
                           xs8)))
          k2
          seq32
          ""
in
let foldl1 =
  lam f31.
    lam l18.
      foldl
        f31
        (head
           l18)
        (tail
           l18)
in
let foldr1 =
  lam f30.
    lam seq31.
      foldr
        f30
        (last
           seq31)
        (init
           seq31)
in
recursive
  let unfoldr: all a91. all c35. (a91 -> Option (c35, a91)) -> a91 -> [c35] =
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
            Some (x62, b110)
          in
          cons
              x62
              (unfoldr
                 f29
                 b110)
in
let range =
  lam s48.
    lam e6.
      lam by.
        unfoldr
          (lam b26.
             match
               leqi
                 e6
                 b26
             with
               true
             then
               None
                 {}
             else
               Some
                 (b26, addi
                   b26
                   by))
          s48
in
recursive
  let foldl2: all a90. all b25. all c34. (a90 -> b25 -> c34 -> a90) -> a90 -> [b25] -> [c34] -> a90 =
    lam f28.
      lam acc48.
        lam seq112.
          lam seq210.
            let g1 =
              lam acc51: (a90, [b25]).
                lam x213.
                  match
                    acc51
                  with
                    (acc52, [ x113 ] ++ xs11 ++ "")
                  in
                  (f28
                      acc52
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
                  (acc48, seq112)
                  seq210
              with
                (acc49, _)
              in
              acc49
            else
              foldl2
                (lam acc50.
                   lam x112.
                     lam x212.
                       f28
                         acc50
                         x212
                         x112)
                acc48
                seq210
                seq112
in
let foldli: all a89. all b24. (a89 -> Int -> b24 -> a89) -> a89 -> [b24] -> a89 =
  lam fn.
    lam initAcc.
      lam seq30.
        recursive
          let work13 =
            lam acc47.
              lam i15.
                lam s47.
                  match
                    s47
                  with
                    [ e5 ] ++ rest ++ ""
                  then
                    work13
                      (fn
                         acc47
                         i15
                         e5)
                      (addi
                         i15
                         1)
                      rest
                  else
                    acc47
        in
        work13
          initAcc
          0
          seq30
in
let zipWith: all a88. all b23. all c33. (a88 -> b23 -> c33) -> [a88] -> [b23] -> [c33] =
  lam f27.
    foldl2
      (lam acc46.
         lam x111.
           lam x211.
             snoc
               acc46
               (f27
                  x111
                  x211))
      ""
in
let zipWithIndex: all a87. all b22. all c32. (Int -> a87 -> b22 -> c32) -> [a87] -> [b22] -> [c32] =
  lam f26.
    lam a110.
      lam a210.
        recursive
          let work12 =
            lam acc45.
              lam i14.
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
                        work12
                          (cons
                             (f26
                                i14
                                e11
                                e21)
                             acc45)
                          (addi
                             i14
                             1)
                          seq1tail
                          seq2tail
                      else
                        reverse
                          acc45
                    else
                      reverse
                        acc45
        in
        work12
          (toList
             "")
          0
          a110
          a210
in
let zip: all a86. all b21. [a86] -> [b21] -> [(a86, b21)] =
  zipWith
    (lam x61.
       lam y10.
         (x61, y10))
in
let mapAccumL: all a85. all b20. all c31. (a85 -> b20 -> (a85, c31)) -> a85 -> [b20] -> (a85, [c31]) =
  lam f25: a85 -> b20 -> (a85, c31).
    lam acc43.
      lam seq28.
        foldl
          (lam tacc1: (a85, [c31]).
             lam x60.
               match
                 f25
                   (tacc1.0)
                   x60
               with
                 (acc44, y9)
               in
               (acc44, snoc
                   (tacc1.1)
                   y9))
          (acc43, "")
          seq28
in
let mapAccumR: all a84. all b19. all c30. (a84 -> b19 -> (a84, c30)) -> a84 -> [b19] -> (a84, [c30]) =
  lam f24: a84 -> b19 -> (a84, c30).
    lam acc41.
      lam seq27.
        foldr
          (lam x59.
             lam tacc: (a84, [c30]).
               match
                 f24
                   (tacc.0)
                   x59
               with
                 (acc42, y8)
               in
               (acc42, cons
                   y8
                   (tacc.1)))
          (acc41, "")
          seq27
in
let unzip: all a83. all b18. [(a83, b18)] -> ([a83], [b18]) =
  mapAccumL
    (lam l17.
       lam p18: (a83, b18).
         (snoc
           l17
           (p18.0), p18.1))
    ""
in
let iter2: all a82. all b17. (a82 -> b17 -> ()) -> [a82] -> [b17] -> () =
  lam f22.
    lam seq110.
      lam seq26.
        let f23 =
          lam x58: (a82, b17).
            match
              x58
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
             seq26)
in
recursive
  let any =
    lam p17.
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
            p17
              (head
                 seq25)
          with
            true
          then
            true
          else
            any
              p17
              (tail
                 seq25)
in
recursive
  let forAll =
    lam p16.
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
            p16
              (head
                 seq24)
          with
            true
          then
            forAll
              p16
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
let seqLiftA2: all a80. all b16. all c29. (a80 -> b16 -> c29) -> [a80] -> [b16] -> [c29] =
  lam f21.
    lam as2.
      lam bs.
        join
          (map
             (lam a81.
                map
                  (f21
                     a81)
                  bs)
             as2)
in
let seqMapM: all a78. all b15. (a78 -> [b15]) -> [a78] -> [[b15]] =
  lam f20.
    foldr
      (lam a79.
         lam acc40.
           seqLiftA2
             cons
             (f20
                a79)
             acc40)
      [ "" ]
in
recursive
  let filter =
    lam p15.
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
            p15
              (head
                 seq23)
          with
            true
          then
            cons
              (head
                 seq23)
              (filter
                 p15
                 (tail
                    seq23))
          else
            filter
              p15
              (tail
                 seq23)
in
recursive
  let filterOption: all a77. [Option a77] -> [a77] =
    lam optSeq.
      match
        optSeq
      with
        [ Some x57 ] ++ optSeq1 ++ ""
      then
        cons
          x57
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
    lam p14.
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
            p14
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
              p14
              (tail
                 seq22)
in
recursive
  let findMap: all a76. all b14. (a76 -> Option b14) -> [a76] -> Option b14 =
    lam f19.
      lam seq20.
        match
          seq20
        with
          [ h5 ] ++ t207 ++ ""
        then
          match
            f19
              h5
          with
            Some x56
          then
            Some
              x56
          else
            findMap
              f19
              t207
        else
          None
            {}
in
let lowerBoundBinarySearch: all a75. (a75 -> Int) -> [a75] -> Option Int =
  lam f18.
    lam s46.
      recursive
        let work11 =
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
                          s46
                          idx3))
                    0
                with
                  true
                then
                  work11
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
                  work11
                    first
                    step1
              else
                first
      in
      let idx2 =
        work11
          0
          (length
             s46)
      in
      match
        eqi
          idx2
          (length
             s46)
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
  lam p13.
    lam seq17.
      recursive
        let work10 =
          lam l16.
            lam r15.
              lam seq18.
                match
                  seq18
                with
                  ""
                then
                  (l16, r15)
                else
                  match
                    seq18
                  with
                    [ s45 ] ++ seq19 ++ ""
                  in
                  match
                      p13
                        s45
                    with
                      true
                    then
                      work10
                        (cons
                           s45
                           l16)
                        r15
                        seq19
                    else
                      work10
                        l16
                        (cons
                           s45
                           r15)
                        seq19
      in
      work10
        ""
        ""
        (reverse
           seq17)
in
let distinct =
  lam eq3.
    lam seq15.
      recursive
        let work9 =
          lam seq16.
            lam seq21.
              match
                seq16
              with
                [ h4 ] ++ t206 ++ ""
              then
                match
                  find
                    (eq3
                       h4)
                    seq21
                with
                  Some _
                then
                  work9
                    t206
                    seq21
                else
                  cons
                    h4
                    (work9
                       t206
                       (cons
                          h4
                          seq21))
              else
                ""
      in
      work9
        seq15
        ""
in
let distinctSorted =
  lam eq2.
    lam s43.
      recursive
        let work8 =
          lam acc39.
            lam s44.
              match
                s44
              with
                [ h11 ] ++ t205 ++ ""
              then
                match
                  acc39
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
                    work8
                      acc39
                      t205
                  else
                    work8
                      (cons
                         h11
                         acc39)
                      t205
                else
                  work8
                    [ h11 ]
                    t205
              else
                acc39
      in
      reverse
        (work8
           ""
           s43)
in
recursive
  let quickSort: all a74. (a74 -> a74 -> Int) -> [a74] -> [a74] =
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
          let t204 =
            tail
              seq14
          in
          let lr1 =
            partition
              (lam x55.
                 lti
                   (cmp12
                      x55
                      h3)
                   0)
              t204
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
      lam l15.
        lam r14.
          match
            l15
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
              l15
            else
              match
                (l15, r14)
              with
                ([ x54 ] ++ xs7 ++ "", [ y7 ] ++ ys1 ++ "")
              in
              match
                  leqi
                    (cmp11
                       x54
                       y7)
                    0
                with
                  true
                then
                  cons
                    x54
                    (merge
                       cmp11
                       xs7
                       r14)
                else
                  cons
                    y7
                    (merge
                       cmp11
                       l15
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
            [ x53 ]
          then
            [ x53 ]
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
let minIdx: all a73. (a73 -> a73 -> Int) -> [a73] -> Option (Int, a73) =
  lam cmp9: a73 -> a73 -> Int.
    lam seq12: [a73].
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
            (lam acc38: (Int, Int, a73).
               lam e4: a73.
                 match
                   acc38
                 with
                   (curi, mini1, m2)
                 in
                 match
                     lti
                       (cmp9
                          m2
                          e4)
                       0
                   with
                     true
                   then
                     (addi
                       curi
                       1, mini1, m2)
                   else
                     (addi
                       curi
                       1, curi, e4))
            (1, 0, head
              seq12)
            (tail
               seq12)
        with
          (_, i13, m3)
        in
        Some
            (i13, m3)
in
let min: all a72. (a72 -> a72 -> Int) -> [a72] -> Option a72 =
  lam cmp8.
    lam seq11.
      optionMap
        (lam r13.
           match
             r13
           with
             (_, m1)
           in
           m1)
        (minIdx
           cmp8
           seq11)
in
let max =
  lam cmp7.
    min
      (lam l14.
         lam r12.
           cmp7
             r12
             l14)
in
let minOrElse =
  lam d5.
    lam cmp6.
      lam seq10.
        optionGetOrElse
          d5
          (min
             cmp6
             seq10)
in
let maxOrElse =
  lam d4.
    lam cmp5.
      minOrElse
        d4
        (lam l13.
           lam r11.
             cmp5
               r11
               l13)
in
let index =
  lam pred3.
    lam seq8.
      recursive
        let index_rechelper =
          lam i12.
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
                      i12
                  else
                    index_rechelper
                      (addi
                         i12
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
          lam i11.
            lam acc37.
              lam pred2.
                lam seq7.
                  match
                    null
                      seq7
                  with
                    true
                  then
                    acc37
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
                           i11
                           1)
                        (Some
                           i11)
                        pred2
                        (tail
                           seq7)
                    else
                      lastIndex_rechelper
                        (addi
                           i11
                           1)
                        acc37
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
let seqCmp: all a71. (a71 -> a71 -> Int) -> [a71] -> [a71] -> Int =
  lam cmp4.
    lam s115.
      lam s215.
        recursive
          let work7 =
            lam s116.
              lam s216.
                match
                  (s116, s216)
                with
                  ([ h1 ] ++ t1100 ++ "", [ h2 ] ++ t210 ++ "")
                then
                  let c28 =
                    cmp4
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
                    work7
                      t1100
                      t210
                  else
                    c28
                else
                  0
        in
        let n19 =
          length
            s115
        in
        let n22 =
          length
            s215
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
          work7
            s115
            s215
        else
          ndiff
in
let randIndex: all a70. [a70] -> Option Int =
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
let randElem: all a69. [a69] -> Option a69 =
  lam seq4.
    optionMap
      (get
         seq4)
      (randIndex
         seq4)
in
let permute: all a68. [a68] -> [Int] -> [a68] =
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
            (lam x52: (a68, Int).
               lam y6: (a68, Int).
                 subi
                   (x52.1)
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
  lam x51.
    x51
in
let const =
  lam x50.
    lam #var"75".
      x50
in
let apply =
  lam f17.
    lam x49.
      f17
        x49
in
let compose =
  lam f16.
    lam g.
      lam x48.
        f16
          (g
             x48)
in
let curry =
  lam f15.
    lam x47.
      lam y5.
        f15
          (x47, y5)
in
let uncurry: all a67. all b13. all c20. (a67 -> b13 -> c20) -> (a67, b13) -> c20 =
  lam f14.
    lam t203: (a67, b13).
      f14
        (t203.0)
        (t203.1)
in
let flip =
  lam f13.
    lam x46.
      lam y4.
        f13
          y4
          x46
in
let printLn =
  lam s42.
    let #var"74" =
      print
        (concat
           s42
           "\n")
    in
    flushStdout
      {}
in
let printSeq =
  lam s41.
    print
      (join
         s41)
in
let printSeqLn =
  lam s40.
    let #var"72" =
      printSeq
        s40
    in
    let #var"73" =
      print
        "\n"
    in
    flushStdout
      {}
in
let dprintLn =
  lam x45.
    let #var"71" =
      dprint
        x45
    in
    printLn
      ""
in
recursive
  let fix: all a66. all b12. ((a66 -> b12) -> a66 -> b12) -> a66 -> b12 =
    lam f12.
      lam e3.
        f12
          (fix
             f12)
          e3
in
let repeat: (() -> ()) -> Int -> () =
  lam f11.
    lam n17.
      recursive
        let rec9 =
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
              let #var"70" =
                f11
                  {}
              in
              rec9
                (subi
                   n18
                   1)
      in
      rec9
        n17
in
let repeati: (Int -> ()) -> Int -> () =
  lam f10.
    lam n16.
      recursive
        let rec8 =
          lam i10.
            match
              geqi
                i10
                n16
            with
              true
            then
              {}
            else
              let #var"69" =
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
let fixMutual: all a65. all b11. [[a65 -> b11] -> a65 -> b11] -> [a65 -> b11] =
  lam l10.
    let l11 =
      map
        (lam li1.
           (li1,))
        l10
    in
    fix
      (lam self.
         lam l12.
           map
             (lam li: ([a65 -> b11] -> a65 -> b11,).
                lam x44.
                  (li.0)
                    (self
                       l12)
                    x44)
             l12)
      l11
in
let maxf: Float -> Float -> Float =
  lam r10.
    lam l9.
      match
        gtf
          r10
          l9
      with
        true
      then
        r10
      else
        l9
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
    lam r9.
      lam l8.
        match
          leqf
            (absf
               (subf
                  r9
                  l8))
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
  lam x43: Float.
    externalExp
      x43
in
external externalLog : Float -> Float
in
let log =
  lam x42: Float.
    externalLog
      x42
in
external externalAtan : Float -> Float
in
let atan =
  lam x41: Float.
    externalAtan
      x41
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
  lam x40: Float.
    externalSin
      x40
in
external externalCos : Float -> Float
in
let cos =
  lam x39: Float.
    externalCos
      x39
in
external externalAtan2 : Float -> Float -> Float
in
let atan2 =
  lam x38: Float.
    lam y3: Float.
      externalAtan2
        x38
        y3
in
external externalPow : Float -> Float -> Float
in
let pow =
  lam x37: Float.
    lam y2: Float.
      externalPow
        x37
        y2
in
external externalSqrt : Float -> Float
in
let sqrt: Float -> Float =
  lam x36.
    externalSqrt
      x36
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
    lam l7.
      match
        ltf
          r8
          l7
      with
        true
      then
        r8
      else
        l7
in
let cmpfApprox: Float -> Float -> Float -> Int =
  lam epsilon.
    lam l6.
      lam r7.
        match
          eqfApprox
            epsilon
            l6
            r7
        with
          true
        then
          0
        else
          match
            ltf
              l6
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
      let work6 =
        lam acc36.
          lam n15.
            match
              gti
                n15
                0
            with
              true
            then
              work6
                (addf
                   (log
                      (int2float
                         n15))
                   acc36)
                (subi
                   n15
                   1)
            else
              acc36
    in
    work6
      0.
      n14
in
let maxi =
  lam r6.
    lam l5.
      match
        gti
          r6
          l5
      with
        true
      then
        r6
      else
        l5
in
let mini =
  lam r5.
    lam l4.
      match
        lti
          r5
          l4
      with
        true
      then
        r5
      else
        l4
in
let absi =
  lam i9.
    maxi
      i9
      (negi
         i9)
in
let succ =
  lam x35.
    addi
      x35
      1
in
let pred =
  lam x34.
    subi
      x34
      1
in
external externalGammaLogPdf : Float -> Float -> Float -> Float
in
external externalGammaSample! : Float -> Float -> Float
in
let gammaPdf =
  lam shape2: Float.
    lam scale2: Float.
      lam x33: Float.
        exp
          (externalGammaLogPdf
             x33
             shape2
             scale2)
in
let gammaLogPdf =
  lam shape1: Float.
    lam scale1: Float.
      lam x32: Float.
        externalGammaLogPdf
          x32
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
  lam p12: Float.
    lam n13: Int.
      lam x31: Int.
        exp
          (externalBinomialLogPmf
             x31
             p12
             n13)
in
let binomialLogPmf =
  lam p11: Float.
    lam n12: Int.
      lam x30: Int.
        externalBinomialLogPmf
          x30
          p11
          n12
in
let binomialSample =
  lam p10: Float.
    lam n11: Int.
      externalBinomialSample
        p10
        n11
in
let bernoulliPmf =
  lam p9: Float.
    lam x29: Bool.
      match
        x29
      with
        true
      then
        p9
      else
        subf
          1.
          p9
in
let bernoulliLogPmf =
  lam p8: Float.
    lam x28: Bool.
      log
        (bernoulliPmf
           p8
           x28)
in
let bernoulliSample =
  lam p7: Float.
    match
      eqi
        1
        (externalBinomialSample
           p7
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
  lam a64: Float.
    lam b10: Float.
      lam x27: Float.
        exp
          (externalBetaLogPdf
             x27
             a64
             b10)
in
let betaLogPdf =
  lam a63: Float.
    lam b9: Float.
      lam x26: Float.
        externalBetaLogPdf
          x26
          a63
          b9
in
let betaSample =
  lam a62: Float.
    lam b8: Float.
      externalBetaSample
        a62
        b8
in
external externalGaussianLogPdf : Float -> Float -> Float -> Float
in
external externalGaussianSample! : Float -> Float -> Float
in
let gaussianPdf =
  lam mu2: Float.
    lam sigma2: Float.
      lam x25: Float.
        exp
          (externalGaussianLogPdf
             x25
             mu2
             sigma2)
in
let gaussianLogPdf =
  lam mu1: Float.
    lam sigma1: Float.
      lam x24: Float.
        externalGaussianLogPdf
          x24
          mu1
          sigma1
in
let gaussianSample =
  lam mu: Float.
    lam sigma: Float.
      externalGaussianSample
        mu
        sigma
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
    lam x23.
      log
        (get
           ps3
           x23)
in
let categoricalPmf: [Float] -> Int -> Float =
  lam ps2.
    lam x22.
      get
        ps2
        x22
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
  lam a61.
    lam b7.
      externalUniformContinuousSample
        a61
        b7
in
let uniformContinuousLogPdf =
  lam a60.
    lam b6.
      lam x21.
        match
          geqf
            x21
            a60
        with
          true
        then
          match
            leqf
              x21
              b6
          with
            true
          then
            subf
              (log
                 1.)
              (log
                 (subf
                    b6
                    a60))
          else
            0.
        else
          0.
in
let uniformContinuousPdf =
  lam a59.
    lam b5.
      lam x20.
        match
          geqf
            x20
            a59
        with
          true
        then
          match
            leqf
              x20
              b5
          with
            true
          then
            divf
              1.
              (subf
                 b5
                 a59)
          else
            0.
        else
          0.
in
let uniformSample: () -> Float =
  lam #var"68".
    uniformContinuousSample
      0.
      1.
in
external externalUniformDiscreteSample! : Int -> Int -> Int
in
let uniformDiscreteSample =
  lam a58: Int.
    lam b4: Int.
      externalUniformDiscreteSample
        a58
        b4
in
let uniformDiscreteLogPdf: Int -> Int -> Int -> Float =
  lam a57.
    lam b3.
      lam x19.
        match
          geqi
            x19
            a57
        with
          true
        then
          match
            leqi
              x19
              b3
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
                          b3
                          a57))))
          else
            0.
        else
          0.
in
let uniformDiscretePdf: Int -> Int -> Int -> Float =
  lam a56.
    lam b2.
      lam x18.
        match
          geqi
            x18
            a56
        with
          true
        then
          match
            leqi
              x18
              b2
          with
            true
          then
            divf
              1.
              (int2float
                 (addi
                    1
                    (subi
                       b2
                       a56)))
          else
            0.
        else
          0.
in
let poissonLogPmf =
  lam lambda5: Float.
    lam x17: Int.
      subf
        (subf
           (mulf
              (int2float
                 x17)
              (log
                 lambda5))
           lambda5)
        (logFactorial
           x17)
in
let poissonPmf =
  lam lambda4: Float.
    lam x16: Int.
      exp
        (poissonLogPmf
           lambda4
           x16)
in
let poissonSample =
  lam lambda3: Float.
    let enlam =
      exp
        (negf
           lambda3)
    in
    let x14 =
      0
    in
    let prod =
      1.
    in
    recursive
      let rec7 =
        lam x15.
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
                   x15
                   1)
                prod2
            else
              x15
    in
    rec7
      x14
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
    lam x13.
      subf
        (log
           lambda1)
        (mulf
           lambda1
           x13)
in
let exponentialPdf: Float -> Float -> Float =
  lam lambda.
    lam x12.
      exp
        (exponentialLogPdf
           lambda
           x12)
in
external externalSetSeed! : Int -> ()
in
let setSeed: Int -> () =
  lam seed.
    externalSetSeed
      seed
in
let eqChar =
  lam c115.
    lam c27.
      eqc
        c115
        c27
in
let neqChar =
  lam c114.
    lam c26.
      not
        (eqc
           c114
           c26)
in
let ltChar =
  lam c113.
    lam c25.
      lti
        (char2int
           c113)
        (char2int
           c25)
in
let gtChar =
  lam c112.
    lam c24.
      gti
        (char2int
           c112)
        (char2int
           c24)
in
let leqChar =
  lam c111.
    lam c23.
      leqi
        (char2int
           c111)
        (char2int
           c23)
in
let geqChar =
  lam c110.
    lam c22.
      geqi
        (char2int
           c110)
        (char2int
           c22)
in
let cmpChar =
  lam c19.
    lam c21.
      subi
        (char2int
           c19)
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
  lam c18.
    match
      find
        (lam e2: (Char, [Char]).
           eqChar
             c18
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
      [ c18 ]
in
let showChar =
  lam c17.
    join
      [ "\'",
        escapeChar
          c17,
        "\'" ]
in
let char2upper =
  lam c16.
    match
      and
        (geqChar
           c16
           'a')
        (leqChar
           c16
           'z')
    with
      true
    then
      int2char
        (subi
           (char2int
              c16)
           32)
    else
      c16
in
let char2lower =
  lam c15.
    match
      and
        (geqChar
           c15
           'A')
        (leqChar
           c15
           'Z')
    with
      true
    then
      int2char
        (addi
           (char2int
              c15)
           32)
    else
      c15
in
let isWhitespace =
  lam c14.
    any
      (eqChar
         c14)
      " \n\t\r"
in
let isLowerAlpha =
  lam c13.
    let i8 =
      char2int
        c13
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
  lam c12.
    let i7 =
      char2int
        c12
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
  lam c11.
    match
      isLowerAlpha
        c11
    with
      true
    then
      true
    else
      isUpperAlpha
        c11
in
let isLowerAlphaOrUnderscore =
  lam c10.
    match
      isLowerAlpha
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
let isAlphaOrUnderscore =
  lam c9.
    match
      isAlpha
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
let isDigit =
  lam c8.
    let i6 =
      char2int
        c8
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
  lam c7.
    match
      isAlpha
        c7
    with
      true
    then
      true
    else
      isDigit
        c7
in
let randAlphanum: () -> Char =
  lam #var"67".
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
  lam s39.
    join
      (map
         escapeChar
         s39)
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
            let work5 =
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
                    work5
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
            work5
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
  lam s38.
    map
      char2upper
      s38
in
let str2lower =
  lam s37.
    map
      char2lower
      s37
in
let string2int =
  lam s35.
    recursive
      let string2int_rechelper =
        lam s36.
          lam acc35.
            match
              null
                s36
            with
              true
            then
              acc35
            else
              let fsd =
                subi
                  (char2int
                     (head
                        s36))
                  (char2int
                     '0')
              in
              string2int_rechelper
                (tail
                   s36)
                (addi
                   (muli
                      10
                      acc35)
                   fsd)
    in
    match
      s35
    with
      ""
    then
      0
    else
      match
        eqChar
          '-'
          (head
             s35)
      with
        true
      then
        negi
          (string2int_rechelper
             (tail
                s35)
             0)
      else
        string2int_rechelper
          s35
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
  lam n6.
    recursive
      let int2string_rechelper =
        lam n7.
          lam acc34.
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
                acc34
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
                   acc34)
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
  lam s34.
    eqString
      s34
      (int2string
         (string2int
            s34))
in
let strIndex =
  lam c5.
    lam s32.
      recursive
        let strIndex_rechelper =
          lam i4.
            lam c6.
              lam s33.
                match
                  null
                    s33
                with
                  true
                then
                  None
                    {}
                else
                  match
                    eqChar
                      c6
                      (head
                         s33)
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
                      c6
                      (tail
                         s33)
      in
      strIndex_rechelper
        0
        c5
        s32
in
let strLastIndex =
  lam c3.
    lam s30.
      recursive
        let strLastIndex_rechelper =
          lam i3.
            lam acc33.
              lam c4.
                lam s31.
                  match
                    null
                      s31
                  with
                    true
                  then
                    match
                      eqi
                        acc33
                        (negi
                           1)
                    with
                      true
                    then
                      None
                        {}
                    else
                      Some
                        acc33
                  else
                    match
                      eqChar
                        c4
                        (head
                           s31)
                    with
                      true
                    then
                      strLastIndex_rechelper
                        (addi
                           i3
                           1)
                        i3
                        c4
                        (tail
                           s31)
                    else
                      strLastIndex_rechelper
                        (addi
                           i3
                           1)
                        acc33
                        c4
                        (tail
                           s31)
      in
      strLastIndex_rechelper
        0
        (negi
           1)
        c3
        s30
in
let strSplit =
  lam delim1.
    lam s29.
      let n5 =
        length
          s29
      in
      let m =
        length
          delim1
      in
      recursive
        let work4 =
          lam acc32.
            lam lastMatch.
              lam i2.
                match
                  lti
                    (subi
                       n5
                       m)
                    i2
                with
                  true
                then
                  snoc
                    acc32
                    (subsequence
                       s29
                       lastMatch
                       n5)
                else
                  match
                    eqStringSlice
                      delim1
                      s29
                      i2
                      m
                  with
                    true
                  then
                    let nexti =
                      addi
                        i2
                        m
                    in
                    work4
                      (snoc
                         acc32
                         (subsequence
                            s29
                            lastMatch
                            (subi
                               i2
                               lastMatch)))
                      nexti
                      nexti
                  else
                    work4
                      acc32
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
        [ s29 ]
      else
        work4
          ""
          0
          0
in
let strTrim =
  lam s27.
    recursive
      let strTrim_init =
        lam s28.
          match
            eqString
              s28
              ""
          with
            true
          then
            s28
          else
            match
              isWhitespace
                (head
                   s28)
            with
              true
            then
              strTrim_init
                (tail
                   s28)
            else
              s28
    in
    reverse
      (strTrim_init
         (reverse
            (strTrim_init
               s27)))
in
let stringIsInt1 =
  lam s25.
    match
      null
        s25
    with
      true
    then
      false
    else
      let s26 =
        match
          eqChar
            (get
               s25
               0)
            '-'
        with
          true
        then
          tail
            s25
        else
          s25
      in
      forAll
        isDigit
        s26
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
  lam s24.
    match
      fileExists
        s24
    with
      true
    then
      deleteFile
        s24
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
  lam c2.
    lam s23.
      writeString
        c2
        s23
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
      (s22, false)
    then
      Some
        s22
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
  lam #var"65".
    match
      neqi
        (length
           argv)
        2
    with
      true
    then
      let #var"66" =
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
  lam res18.
    lam names4.
      lam filename.
        lam expOnLogWeights1.
          match
            writeOpen1
              filename
          with
            Some ch
          then
            let #var"61" =
              writeString1
                ch
                (strJoin
                   ","
                   names4)
            in
            let #var"62" =
              writeString1
                ch
                "\n"
            in
            let #var"63" =
              iter
                (lam lst.
                   let #var"64" =
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
                   res18)
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
  lam res17.
    lam names2.
      lam normConst3.
        lam expVals2.
          lam varianceVals1.
            let pad =
              18
            in
            let padPrint =
              lam s21.
                lam n4.
                  match
                    geqi
                      n4
                      (length
                         s21)
                  with
                    true
                  then
                    let #var"59" =
                      print
                        s21
                    in
                    print
                      (create
                         (subi
                            n4
                            (length
                               s21))
                         (lam #var"60".
                            ' '))
                  else
                    print
                      s21
            in
            let #var"47" =
              padPrint
                "Variable"
                14
            in
            let #var"48" =
              padPrint
                "Expected Value"
                pad
            in
            let #var"49" =
              padPrint
                "Variance"
                pad
            in
            let #var"50" =
              padPrint
                "Standard Deviation"
                pad
            in
            let #var"51" =
              print
                "\n"
            in
            recursive
              let work3 =
                lam names3.
                  lam ev.
                    lam vv.
                      match
                        (names3, ev, vv)
                      with
                        ([ n3 ] ++ ns4 ++ "", [ e1 ] ++ es1 ++ "", [ v2 ] ++ vs ++ "")
                      then
                        match
                          isPrefix
                            eqChar
                            "#"
                            n3
                        with
                          true
                        then
                          work3
                            ns4
                            ev
                            vv
                        else
                          let #var"54" =
                            padPrint
                              n3
                              14
                          in
                          let #var"55" =
                            padPrint
                              (float2string
                                 e1)
                              pad
                          in
                          let #var"56" =
                            padPrint
                              (float2string
                                 v2)
                              pad
                          in
                          let #var"57" =
                            padPrint
                              (float2string
                                 (sqrt
                                    v2))
                              pad
                          in
                          let #var"58" =
                            print
                              "\n"
                          in
                          work3
                            ns4
                            es1
                            vs
                      else
                        {}
            in
            let #var"52" =
              work3
                names2
                expVals2
                varianceVals1
            in
            let #var"53" =
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
let systematicSample: all a55. [a55] -> [Float] -> Float -> Int -> [a55] =
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
  lam res16.
    let max1 =
      foldl
        (lam acc31.
           lam x11.
             match
               geqf
                 x11
                 acc31
             with
               true
             then
               x11
             else
               acc31)
        negInf
        res16
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
          (lam acc30.
             lam x10.
               addf
                 (exp
                    (subf
                       x10
                       max1))
                 acc30)
          0.
          res16
      in
      subf
        (addf
           max1
           (log
              sum1))
        (log
           (int2float
              (length
                 res16)))
in
let expectedValues =
  lam res15: [[Float]].
    lam normConst2.
      foldl
        (lam acc28.
           lam t202.
             let w3 =
               exp
                 (subf
                    (head
                       t202)
                    normConst2)
             in
             let ys =
               tail
                 t202
             in
             recursive
               let work2 =
                 lam acc29.
                   lam xs3.
                     match
                       (acc29, xs3)
                     with
                       ([ a54 ] ++ as1 ++ "", [ x9 ] ++ xs4 ++ "")
                     then
                       cons
                         (addf
                            (mulf
                               x9
                               w3)
                            a54)
                         (work2
                            as1
                            xs4)
                     else
                       ""
             in
             work2
               acc28
               ys)
        (create
           (subi
              (length
                 (head
                    res15))
              1)
           (lam #var"46".
              0.))
        res15
in
let variance =
  lam res14.
    lam expVals1.
      let sum =
        foldl
          (lam acc26.
             lam t201.
               recursive
                 let work1 =
                   lam acc27.
                     lam xs1.
                       lam expv.
                         match
                           (acc27, xs1, expv)
                         with
                           ([ a53 ] ++ as ++ "", [ x8 ] ++ xs2 ++ "", [ e ] ++ es ++ "")
                         then
                           let v1 =
                             subf
                               x8
                               e
                           in
                           cons
                             (addf
                                a53
                                (mulf
                                   v1
                                   v1))
                             (work1
                                as
                                xs2
                                es)
                         else
                           ""
               in
               work1
                 acc26
                 (tail
                    t201)
                 expVals1)
          (create
             (subi
                (length
                   (head
                      res14))
                1)
             (lam #var"45".
                0.))
          res14
      in
      let dval =
        int2float
          (length
             res14)
      in
      map
        (lam x7.
           divf
             x7
             dval)
        sum
in
let expOnLogWeights =
  lam res13.
    mapReverse
      (lam t200.
         match
           t200
         with
           [ x6 ] ++ xs ++ ""
         in
         cons
             (exp
                x6)
             xs)
      res13
in
let output =
  lam res12: [[Float]].
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
             res12)
      in
      let expVals =
        expectedValues
          res12
          nc
      in
      let varianceVals =
        variance
          res12
          expVals
      in
      let #var"44" =
        printStatistics
          res12
          names1
          nc
          expVals
          varianceVals
      in
      saveCSV
        res12
        names1
        "data.csv"
        expOnLogWeights
in
let printSamples: all a52. (a52 -> [Char]) -> [Float] -> [a52] -> () =
  lam printFun1.
    lam weights4.
      lam samples10.
        recursive
          let rec6: [Float] -> [a52] -> () =
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
                  let s20 =
                    head
                      samples11
                  in
                  let samples12 =
                    tail
                      samples11
                  in
                  let #var"40" =
                    print
                      (printFun1
                         s20)
                  in
                  let #var"41" =
                    print
                      " "
                  in
                  let #var"42" =
                    print
                      (float2string
                         w2)
                  in
                  let #var"43" =
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
let printSamplesOption: all a51. (a51 -> [Char]) -> [Float] -> [Option a51] -> () =
  lam printFun.
    lam weights1.
      lam samples7.
        recursive
          let rec5: [Float] -> [Option a51] -> () =
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
                  let s18 =
                    head
                      samples8
                  in
                  let samples9 =
                    tail
                      samples8
                  in
                  let #var"36" =
                    match
                      s18
                    with
                      Some s19
                    then
                      print
                        (printFun
                           s19)
                    else
                      print
                        "."
                  in
                  let #var"37" =
                    print
                      " "
                  in
                  let #var"38" =
                    print
                      (float2string
                         w1)
                  in
                  let #var"39" =
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
  lam n2.
    let #var"35" =
      modref
        _mcmcSamples
        n2
    in
    modref
      _mcmcAccepts
      0
in
let mcmcAccept =
  lam #var"34".
    modref
      _mcmcAccepts
      (addi
         (deref
            _mcmcAccepts)
         1)
in
let mcmcAcceptRate =
  lam #var"33".
    divf
      (int2float
         (deref
            _mcmcAccepts))
      (int2float
         (deref
            _mcmcSamples))
in
recursive
  let #var"RuntimeDistBase_sample": all a49. Dist a49 -> a49 =
    lam __sem_target20.
      let _20 =
        dprint
          __sem_target20
      in
      error
        "No matching case for function sample"
  let #var"RuntimeDistBase_logObserve": all a50. Dist a50 -> a50 -> Float =
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
  let #var"RuntimeDistElementary_sample": all a47. Dist a47 -> a47 =
    lam __sem_target18.
      match
        __sem_target18
      with
        RuntimeDistElementary_DistGamma t178
      then
        unsafeCoerce
          (gammaSample
             (t178.shape)
             (t178.scale))
      else
        match
          __sem_target18
        with
          RuntimeDistElementary_DistExponential t179
        then
          unsafeCoerce
            (exponentialSample
               (t179.rate))
        else
          match
            __sem_target18
          with
            RuntimeDistElementary_DistPoisson t180
          then
            unsafeCoerce
              (poissonSample
                 (t180.lambda))
          else
            match
              __sem_target18
            with
              RuntimeDistElementary_DistBinomial t181
            then
              unsafeCoerce
                (binomialSample
                   (t181.p)
                   (t181.n))
            else
              match
                __sem_target18
              with
                RuntimeDistElementary_DistBernoulli t182
              then
                unsafeCoerce
                  (bernoulliSample
                     (t182.p))
              else
                match
                  __sem_target18
                with
                  RuntimeDistElementary_DistBeta t183
                then
                  unsafeCoerce
                    (betaSample
                       (t183.a)
                       (t183.b))
                else
                  match
                    __sem_target18
                  with
                    RuntimeDistElementary_DistGaussian t184
                  then
                    unsafeCoerce
                      (gaussianSample
                         (t184.mu)
                         (t184.sigma))
                  else
                    match
                      __sem_target18
                    with
                      RuntimeDistElementary_DistMultinomial t185
                    then
                      unsafeCoerce
                        (multinomialSample
                           (t185.p)
                           (t185.n))
                    else
                      match
                        __sem_target18
                      with
                        RuntimeDistElementary_DistCategorical t186
                      then
                        unsafeCoerce
                          (categoricalSample
                             (t186.p))
                      else
                        match
                          __sem_target18
                        with
                          RuntimeDistElementary_DistDirichlet t187
                        then
                          unsafeCoerce
                            (dirichletSample
                               (t187.a))
                        else
                          match
                            __sem_target18
                          with
                            RuntimeDistElementary_DistUniform t188
                          then
                            unsafeCoerce
                              (uniformContinuousSample
                                 (t188.a)
                                 (t188.b))
                          else
                            let _18 =
                              dprint
                                __sem_target18
                            in
                            error
                              "No matching case for function sample"
  let #var"RuntimeDistElementary_logObserve": all a48. Dist a48 -> a48 -> Float =
    lam __sem_target19.
      match
        __sem_target19
      with
        RuntimeDistElementary_DistGamma t189
      then
        unsafeCoerce
          (gammaLogPdf
             (t189.shape)
             (t189.scale))
      else
        match
          __sem_target19
        with
          RuntimeDistElementary_DistExponential t190
        then
          unsafeCoerce
            (exponentialLogPdf
               (t190.rate))
        else
          match
            __sem_target19
          with
            RuntimeDistElementary_DistPoisson t191
          then
            unsafeCoerce
              (poissonLogPmf
                 (t191.lambda))
          else
            match
              __sem_target19
            with
              RuntimeDistElementary_DistBinomial t192
            then
              unsafeCoerce
                (binomialLogPmf
                   (t192.p)
                   (t192.n))
            else
              match
                __sem_target19
              with
                RuntimeDistElementary_DistBernoulli t193
              then
                unsafeCoerce
                  (bernoulliLogPmf
                     (t193.p))
              else
                match
                  __sem_target19
                with
                  RuntimeDistElementary_DistBeta t194
                then
                  unsafeCoerce
                    (betaLogPdf
                       (t194.a)
                       (t194.b))
                else
                  match
                    __sem_target19
                  with
                    RuntimeDistElementary_DistGaussian t195
                  then
                    unsafeCoerce
                      (gaussianLogPdf
                         (t195.mu)
                         (t195.sigma))
                  else
                    match
                      __sem_target19
                    with
                      RuntimeDistElementary_DistMultinomial t196
                    then
                      unsafeCoerce
                        (lam o1.
                           match
                             eqi
                               (t196.n)
                               (foldl1
                                  addi
                                  o1)
                           with
                             true
                           then
                             multinomialLogPmf
                               (t196.p)
                               o1
                           else
                             negf
                               inf)
                    else
                      match
                        __sem_target19
                      with
                        RuntimeDistElementary_DistCategorical t197
                      then
                        unsafeCoerce
                          (categoricalLogPmf
                             (t197.p))
                      else
                        match
                          __sem_target19
                        with
                          RuntimeDistElementary_DistDirichlet t198
                        then
                          unsafeCoerce
                            (dirichletLogPdf
                               (t198.a))
                        else
                          match
                            __sem_target19
                          with
                            RuntimeDistElementary_DistUniform t199
                          then
                            unsafeCoerce
                              (uniformContinuousLogPdf
                                 (t199.a)
                                 (t199.b))
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
  let #var"RuntimeDistEmpirical_sample": all a45. Dist a45 -> a45 =
    lam __sem_target10.
      match
        __sem_target10
      with
        RuntimeDistEmpirical_DistEmpirical t172
      then
        let x4 =
          uniformContinuousSample
            0.
            (last
               (t172.cumulativeWeights))
        in
        let cmp3 =
          lam y1.
            match
              ltf
                (subf
                   y1
                   x4)
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
            (t172.cumulativeWeights)
        with
          Some idx1
        then
          unsafeCoerce
            (get
               (t172.samples)
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
  let #var"RuntimeDistEmpirical_logObserve": all a46. Dist a46 -> a46 -> Float =
    lam __sem_target11.
      match
        __sem_target11
      with
        RuntimeDistEmpirical_DistEmpirical t173
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
        RuntimeDistEmpirical_DistEmpirical t174
      then
        (t174.samples, t174.logWeights)
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
        RuntimeDistEmpirical_DistEmpirical t175
      then
        match
          t175.extra
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
        RuntimeDistEmpirical_DistEmpirical t176
      then
        match
          t176.extra
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
        RuntimeDistEmpirical_DistEmpirical t177
      then
        t177.degenerate
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
                (lam acc25.
                   lam lw6.
                     match
                       geqf
                         lw6
                         acc25
                     with
                       true
                     then
                       lw6
                     else
                       acc25)
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
                      (lam acc24.
                         lam lw5.
                           addf
                             acc24
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
              lam acc22.
                lam x5.
                  let acc23 =
                    addf
                      acc22
                      (exp
                         x5)
                  in
                  (acc23, acc23)
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
        RuntimeDistCombined_DistCombinedIndependent t170
      then
        unsafeCoerce
          (map
             #var"RuntimeDistCombined_sample"
             (t170.combined))
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
        RuntimeDistCombined_DistCombinedIndependent t171
      then
        unsafeCoerce
          (map
             #var"RuntimeDistCombined_logObserve"
             (t171.combined))
      else
        let _9 =
          dprint
            __sem_target9
        in
        error
          "No matching case for function logObserve"
in
recursive
  let #var"RuntimeDist_sample": all a43. Dist a43 -> a43 =
    lam __sem_target.
      match
        __sem_target
      with
        RuntimeDistCombined_DistCombinedIndependent t140
      then
        unsafeCoerce
          (map
             #var"RuntimeDist_sample"
             (t140.combined))
      else
        match
          __sem_target
        with
          RuntimeDistEmpirical_DistEmpirical t141
        then
          let x2 =
            uniformContinuousSample
              0.
              (last
                 (t141.cumulativeWeights))
          in
          let cmp2 =
            lam y.
              match
                ltf
                  (subf
                     y
                     x2)
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
              (t141.cumulativeWeights)
          with
            Some idx
          then
            unsafeCoerce
              (get
                 (t141.samples)
                 idx)
          else
            error
              "Sampling from empirical distribution failed"
        else
          match
            __sem_target
          with
            RuntimeDistElementary_DistGamma t142
          then
            unsafeCoerce
              (gammaSample
                 (t142.shape)
                 (t142.scale))
          else
            match
              __sem_target
            with
              RuntimeDistElementary_DistExponential t143
            then
              unsafeCoerce
                (exponentialSample
                   (t143.rate))
            else
              match
                __sem_target
              with
                RuntimeDistElementary_DistPoisson t144
              then
                unsafeCoerce
                  (poissonSample
                     (t144.lambda))
              else
                match
                  __sem_target
                with
                  RuntimeDistElementary_DistBinomial t145
                then
                  unsafeCoerce
                    (binomialSample
                       (t145.p)
                       (t145.n))
                else
                  match
                    __sem_target
                  with
                    RuntimeDistElementary_DistBernoulli t146
                  then
                    unsafeCoerce
                      (bernoulliSample
                         (t146.p))
                  else
                    match
                      __sem_target
                    with
                      RuntimeDistElementary_DistBeta t147
                    then
                      unsafeCoerce
                        (betaSample
                           (t147.a)
                           (t147.b))
                    else
                      match
                        __sem_target
                      with
                        RuntimeDistElementary_DistGaussian t148
                      then
                        unsafeCoerce
                          (gaussianSample
                             (t148.mu)
                             (t148.sigma))
                      else
                        match
                          __sem_target
                        with
                          RuntimeDistElementary_DistMultinomial t149
                        then
                          unsafeCoerce
                            (multinomialSample
                               (t149.p)
                               (t149.n))
                        else
                          match
                            __sem_target
                          with
                            RuntimeDistElementary_DistCategorical t150
                          then
                            unsafeCoerce
                              (categoricalSample
                                 (t150.p))
                          else
                            match
                              __sem_target
                            with
                              RuntimeDistElementary_DistDirichlet t151
                            then
                              unsafeCoerce
                                (dirichletSample
                                   (t151.a))
                            else
                              match
                                __sem_target
                              with
                                RuntimeDistElementary_DistUniform t152
                              then
                                unsafeCoerce
                                  (uniformContinuousSample
                                     (t152.a)
                                     (t152.b))
                              else
                                let #var"_" =
                                  dprint
                                    __sem_target
                                in
                                error
                                  "No matching case for function sample"
  let #var"RuntimeDist_logObserve": all a44. Dist a44 -> a44 -> Float =
    lam __sem_target1.
      match
        __sem_target1
      with
        RuntimeDistCombined_DistCombinedIndependent t153
      then
        unsafeCoerce
          (map
             #var"RuntimeDist_logObserve"
             (t153.combined))
      else
        match
          __sem_target1
        with
          RuntimeDistEmpirical_DistEmpirical t154
        then
          error
            "Log observe not supported for empirical distribution"
        else
          match
            __sem_target1
          with
            RuntimeDistElementary_DistGamma t155
          then
            unsafeCoerce
              (gammaLogPdf
                 (t155.shape)
                 (t155.scale))
          else
            match
              __sem_target1
            with
              RuntimeDistElementary_DistExponential t156
            then
              unsafeCoerce
                (exponentialLogPdf
                   (t156.rate))
            else
              match
                __sem_target1
              with
                RuntimeDistElementary_DistPoisson t157
              then
                unsafeCoerce
                  (poissonLogPmf
                     (t157.lambda))
              else
                match
                  __sem_target1
                with
                  RuntimeDistElementary_DistBinomial t158
                then
                  unsafeCoerce
                    (binomialLogPmf
                       (t158.p)
                       (t158.n))
                else
                  match
                    __sem_target1
                  with
                    RuntimeDistElementary_DistBernoulli t159
                  then
                    unsafeCoerce
                      (bernoulliLogPmf
                         (t159.p))
                  else
                    match
                      __sem_target1
                    with
                      RuntimeDistElementary_DistBeta t160
                    then
                      unsafeCoerce
                        (betaLogPdf
                           (t160.a)
                           (t160.b))
                    else
                      match
                        __sem_target1
                      with
                        RuntimeDistElementary_DistGaussian t161
                      then
                        unsafeCoerce
                          (gaussianLogPdf
                             (t161.mu)
                             (t161.sigma))
                      else
                        match
                          __sem_target1
                        with
                          RuntimeDistElementary_DistMultinomial t162
                        then
                          unsafeCoerce
                            (lam o.
                               match
                                 eqi
                                   (t162.n)
                                   (foldl1
                                      addi
                                      o)
                               with
                                 true
                               then
                                 multinomialLogPmf
                                   (t162.p)
                                   o
                               else
                                 negf
                                   inf)
                        else
                          match
                            __sem_target1
                          with
                            RuntimeDistElementary_DistCategorical t163
                          then
                            unsafeCoerce
                              (categoricalLogPmf
                                 (t163.p))
                          else
                            match
                              __sem_target1
                            with
                              RuntimeDistElementary_DistDirichlet t164
                            then
                              unsafeCoerce
                                (dirichletLogPdf
                                   (t164.a))
                            else
                              match
                                __sem_target1
                              with
                                RuntimeDistElementary_DistUniform t165
                              then
                                unsafeCoerce
                                  (uniformContinuousLogPdf
                                     (t165.a)
                                     (t165.b))
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
        RuntimeDistEmpirical_DistEmpirical t166
      then
        (t166.samples, t166.logWeights)
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
        RuntimeDistEmpirical_DistEmpirical t167
      then
        match
          t167.extra
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
        RuntimeDistEmpirical_DistEmpirical t168
      then
        match
          t168.extra
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
        RuntimeDistEmpirical_DistEmpirical t169
      then
        t169.degenerate
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
                (lam acc21.
                   lam lw3.
                     match
                       geqf
                         lw3
                         acc21
                     with
                       true
                     then
                       lw3
                     else
                       acc21)
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
                      (lam acc20.
                         lam lw2.
                           addf
                             acc20
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
              lam acc18.
                lam x3.
                  let acc19 =
                    addf
                      acc18
                      (exp
                         x3)
                  in
                  (acc19, acc19)
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
let distEmpiricalSamples: all a42. Dist a42 -> ([a42], [Float]) =
  #var"RuntimeDist_empiricalSamples"
in
let distEmpiricalDegenerate: all a41. Dist a41 -> Bool =
  #var"RuntimeDist_empiricalDegenerate"
in
let distEmpiricalNormConst: all a40. Dist a40 -> Float =
  #var"RuntimeDist_empiricalNormConst"
in
let distEmpiricalAcceptRate: all a39. Dist a39 -> Float =
  #var"RuntimeDist_empiricalAcceptRate"
in
let distCombineIndependent: all a38. [Dist a38] -> Dist a38 =
  lam dists.
    RuntimeDistCombined_DistCombinedIndependent
      { combined =
          dists }
in
let sample: all a37. Dist a37 -> a37 =
  #var"RuntimeDist_sample"
in
let logObserve: all a36. Dist a36 -> a36 -> Float =
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
let run: all a33. all b1. Unknown -> (State -> (Option (Dist b1), b1 -> Checkpoint a33)) -> Dist a33 =
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
      type Stop a34 =
        {weight: Float, checkpoint: Checkpoint a34}
      in
      let start: (b1 -> Checkpoint a33) -> Float -> (() -> b1) -> Int -> Stop a33 =
        lam cont2.
          lam weight.
            lam sampleFun.
              lam #var"31".
                let #var"32" =
                  modref
                    state1
                    weight
                in
                let checkpoint1: Checkpoint a33 =
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
            let #var"30" =
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
                (lam p1.
                   match
                     p1.checkpoint
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
                   (lam p2.
                      (p2.weight, match
                        p2.checkpoint
                      with
                        End a35
                      in
                      a35))
                   particles1)
            else
              let maxWeight =
                foldl
                  (lam acc17.
                     lam p6.
                       match
                         geqf
                           (p6.weight)
                           acc17
                       with
                         true
                       then
                         p6.weight
                       else
                         acc17)
                  (negf
                     inf)
                  particles1
              in
              let expWeights =
                reverse
                  (mapReverse
                     (lam p5.
                        exp
                          (subf
                             (p5.weight)
                             maxWeight))
                     particles1)
              in
              let sums =
                foldl
                  (lam acc16.
                     lam w.
                       (addf
                         (acc16.0)
                         w, addf
                         (acc16.1)
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
                    (lam p3.
                       propagate
                         p3
                         (p3.weight))
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
                    (lam p4.
                       propagate
                         p4
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
      let particles: [Stop a33] =
          match
            d1
          with
            Some d2
          then
            match
              d2
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
                foldl2
                  (lam acc15.
                     lam s17.
                       lam lw.
                         cons
                           (start
                              cont1
                              lw
                              (lam #var"26".
                                 s17)
                              0)
                           acc15)
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
                     (lam #var"27".
                        #var"RuntimeDist_sample"
                          d2))
            else
              createList
                particleCount
                (start
                   cont1
                   0.
                   (lam #var"28".
                      #var"RuntimeDist_sample"
                        d2))
          else
            createList
              particleCount
              (start
                 cont1
                 0.
                 (lam #var"29".
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
  let work =
    lam p.
      lam l3.
        lam r2.
          lam seq.
            match
              seq
            with
              ""
            then
              (l3, r2)
            else
              match
                seq
              with
                [ s16 ] ++ seq1 ++ ""
              in
              match
                  p
                    s16
                with
                  true
                then
                  work
                    p
                    (cons
                       s16
                       l3)
                    r2
                    seq1
                else
                  work
                    p
                    l3
                    (cons
                       s16
                       r2)
                    seq1
in
recursive
  let t139 =
    lam cmp1.
      lam h.
        lam x1.
          lti
            (cmp1
               x1
               h)
            0
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
external externalReadFloatPipe : Int -> [(Timespec, Float)]
in
external externalWriteDistFloatPipe : Int -> (Timespec, ([Float], [Float])) -> ()
in
let nanosPerSec =
  1000000000
in
let nanosToTimespec =
  lam nanosPerSec15.
    lam nanos.
      let s15 =
        divi
          nanos
          nanosPerSec15
      in
      let ns3 =
        modi
          nanos
          nanosPerSec15
      in
      (s15, ns3)
in
let timespecToNanos =
  lam nanosPerSec14.
    lam ts1.
      match
        ts1
      with
        (s14, ns2)
      in
      addi
          (muli
             s14
             nanosPerSec14)
          ns2
in
let addTimespec =
  lam nanosPerSec13.
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
          let ns1 =
            addi
              lns2
              rns2
          in
          match
            geqi
              ns1
              nanosPerSec13
          with
            true
          then
            (addi
              s13
              1, subi
              ns1
              nanosPerSec13)
          else
            (s13, ns1)
in
let diffTimespec =
  lam nanosPerSec12.
    lam lhs1.
      lam rhs1.
        match
          (lhs1, rhs1)
        with
          ((ls1, lns1), (rs1, rns1))
        in
        let s12 =
            subi
              ls1
              rs1
          in
          let ns =
            subi
              lns1
              rns1
          in
          match
            lti
              ns
              0
          with
            true
          then
            (subi
              s12
              1, addi
              ns
              nanosPerSec12)
          else
            (s12, ns)
in
let cmpTimespec: Timespec -> Timespec -> Int =
  lam lhs.
    lam rhs.
      match
        (lhs, rhs)
      with
        ((ls, lns), (rs, rns))
      in
      match
          gti
            ls
            rs
        with
          true
        then
          1
        else
          match
            lti
              ls
              rs
          with
            true
          then
            negi
              1
          else
            match
              gti
                lns
                rns
            with
              true
            then
              1
            else
              match
                lti
                  lns
                  rns
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
  lam nanosPerSec11.
    lam monoLogicalTime6.
      lam wallLogicalTime12.
        lam delay1.
          let oldPriority =
            setMaxPriority
              {}
          in
          let intervalTime =
            nanosToTimespec
              nanosPerSec11
              delay1
          in
          let endTime =
            getMonotonicTime
              {}
          in
          let elapsedTime =
            diffTimespec
              nanosPerSec11
              endTime
              (deref
                 monoLogicalTime6)
          in
          let waitTime =
            addTimespec
              nanosPerSec11
              (deref
                 monoLogicalTime6)
              intervalTime
          in
          let overrun1 =
            let c1 =
              cmpTimespec
                intervalTime
                elapsedTime
            in
            match
              gti
                c1
                0
            with
              true
            then
              let #var"25" =
                clockNanosleep
                  waitTime
              in
              0
            else
              match
                lti
                  c1
                  0
              with
                true
              then
                let elapsedTime1 =
                  diffTimespec
                    nanosPerSec11
                    endTime
                    waitTime
                in
                timespecToNanos
                  nanosPerSec11
                  elapsedTime1
              else
                0
          in
          let #var"22" =
            modref
              monoLogicalTime6
              waitTime
          in
          let #var"23" =
            modref
              wallLogicalTime12
              (addTimespec
                 nanosPerSec11
                 (deref
                    wallLogicalTime12)
                 intervalTime)
          in
          let #var"24" =
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
    lam wallLogicalTime11.
      lam tsv4.
        let lt1 =
          deref
            wallLogicalTime11
        in
        timespecToNanos
          nanosPerSec10
          (diffTimespec
             nanosPerSec10
             (tsv4.0)
             lt1)
in
let value: TSV Unknown -> Unknown =
  lam tsv2.
    tsv2.1
in
let tsv =
  lam nanosPerSec9.
    lam wallLogicalTime10.
      lam offset.
        lam value1.
          let lt =
            deref
              wallLogicalTime10
          in
          (addTimespec
            nanosPerSec9
            lt
            (nanosToTimespec
               nanosPerSec9
               offset), value1)
in
let sdelay =
  lam nanosPerSec8.
    lam monoLogicalTime5.
      lam wallLogicalTime9.
        lam flushOutputs1.
          lam updateInputs1.
            lam delay.
              let #var"20" =
                flushOutputs1
                  {}
              in
              let overrun =
                delayBy
                  nanosPerSec8
                  monoLogicalTime5
                  wallLogicalTime9
                  delay
              in
              let #var"21" =
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
    lam msg.
      externalWriteDistFloatPipe
        fd1
        msg
in
let rtpplWriteDistFloatPort =
  lam fd.
    lam msgs.
      iter
        (t
           fd)
        msgs
in
let t1 =
  lam closeFileDescriptors2.
    lam #var"18".
      let #var"19" =
        closeFileDescriptors2
          {}
      in
      exit
        0
in
let rtpplRuntimeInit =
  lam monoLogicalTime4.
    lam wallLogicalTime8.
      lam updateInputSequences.
        lam closeFileDescriptors1.
          lam cont.
            let #var"13" =
              setSigintHandler
                (t1
                   closeFileDescriptors1)
            in
            let #var"14" =
              modref
                monoLogicalTime4
                (getMonotonicTime
                   {})
            in
            let #var"15" =
              modref
                wallLogicalTime8
                (getWallClockTime
                   {})
            in
            let #var"16" =
              updateInputSequences
                {}
            in
            let #var"17" =
              cont
                {}
            in
            {}
in
let fileDescriptors =
  { distObs =
      openFileDescriptor
        "leftSide-distObs",
    distEst =
      openFileDescriptor
        "leftSide-distEst" }
in
let closeFileDescriptors =
  lam fileDescriptors6.
    lam #var"12".
      let close_distObs =
        closeFileDescriptor
          (fileDescriptors6.distObs)
      in
      let close_distEst =
        closeFileDescriptor
          (fileDescriptors6.distEst)
      in
      {}
in
let inputSeqs =
  ref
    { distObs =
        "" }
in
let outputSeqs =
  ref
    { distEst =
        "" }
in
let updateInputs =
  lam fileDescriptors5.
    lam inputSeqs4.
      lam #var"11".
        modref
          inputSeqs4
          { distObs =
              rtpplReadFloatPort
                (fileDescriptors5.distObs) }
in
let t2 =
  lam tsv3.
    match
      tsv3
    with
      (ts, v)
    in
    (ts, unsafeCoerce
        (distEmpiricalSamples
           v))
in
let flushOutputs =
  lam fileDescriptors4.
    lam outputSeqs4.
      lam #var"9".
        let w_distEst =
          rtpplWriteDistFloatPort
            (fileDescriptors4.distEst)
            (map
               t2
               ((deref
                   outputSeqs4).distEst))
        in
        let #var"10" =
          modref
            outputSeqs4
            { distEst =
                "" }
        in
        {}
in
let subInt =
  subi
in
let negInt =
  subi
    0
in
let ltInt =
  lti
in
let gtInt =
  gti
in
let geqInt =
  geqi
in
let concat: [Unknown] -> [Unknown] -> [Unknown] =
  lam l2.
    lam r1.
      concat
        l2
        r1
in
let sort1: (Unknown -> Unknown -> Int) -> [Unknown] -> [Unknown] =
  lam cmp.
    lam s11.
      quickSort
        cmp
        s11
in
let tofMaxRange =
  2.
in
let cmpFloatTimestamp =
  lam nanosPerSec7.
    lam wallLogicalTime7.
      lam negInt5.
        lam ltInt5.
          lam gtInt5.
            lam l1.
              lam r.
                let acc10 =
                  0
                in
                let acc11 =
                  match
                    gtInt5
                      (timestamp
                         nanosPerSec7
                         wallLogicalTime7
                         l1)
                      (timestamp
                         nanosPerSec7
                         wallLogicalTime7
                         r)
                  with
                    true
                  then
                    let acc12 =
                      1
                    in
                    acc12
                  else
                    let acc13 =
                      match
                        ltInt5
                          (timestamp
                             nanosPerSec7
                             wallLogicalTime7
                             l1)
                          (timestamp
                             nanosPerSec7
                             wallLogicalTime7
                             r)
                      with
                        true
                      then
                        let acc14 =
                          negInt5
                            1
                        in
                        acc14
                      else
                        acc10
                    in
                    acc13
                in
                acc11
in
let medianOfLastThreeFloatTsv =
  lam subInt7.
    lam tsvs.
      let n =
        length
          tsvs
      in
      let a22 =
        value
          (get
             tsvs
             (subInt7
                n
                3))
      in
      let b =
        value
          (get
             tsvs
             (subInt7
                n
                2))
      in
      let c =
        value
          (get
             tsvs
             (subInt7
                n
                1))
      in
      let res3 =
        0.
      in
      let res4 =
        match
          ltf
            a22
            b
        with
          true
        then
          let res6 =
            match
              ltf
                b
                c
            with
              true
            then
              let res7 =
                b
              in
              res7
            else
              let res8 =
                c
              in
              res8
          in
          res6
        else
          let res9 =
            match
              ltf
                a22
                c
            with
              true
            then
              let res10 =
                a22
              in
              res10
            else
              let res11 =
                c
              in
              res11
          in
          res9
      in
      res4
in
let t3 =
  lam maxRange1.
    lam acc2.
      lam tsv1.
        let acc3 =
          match
            ltf
              (value
                 tsv1)
              maxRange1
          with
            true
          then
            let acc4 =
              false
            in
            acc4
          else
            acc2
        in
        acc3
in
let allOutOfRange: [TSV Float] -> Float -> Bool =
  lam distanceObs.
    lam maxRange.
      let acc =
        true
      in
      let acc1 =
        foldl
          (t3
             maxRange)
          acc
          distanceObs
      in
      acc1
in
let takeThreeMostRecent =
  lam nanosPerSec6.
    lam wallLogicalTime6.
      lam subInt8.
        lam negInt4.
          lam ltInt4.
            lam gtInt4.
              lam geqInt4.
                lam a29.
                  let n1 =
                    length
                      a29
                  in
                  let a30 =
                    sort1
                      (cmpFloatTimestamp
                         nanosPerSec6
                         wallLogicalTime6
                         negInt4
                         ltInt4
                         gtInt4)
                      a29
                  in
                  let a31 =
                    match
                      geqInt4
                        n1
                        3
                    with
                      true
                    then
                      let a32 =
                        [ get
                            a30
                            (subInt8
                               n1
                               3),
                          get
                            a30
                            (subInt8
                               n1
                               2),
                          get
                            a30
                            (subInt8
                               n1
                               1) ]
                      in
                      a32
                    else
                      a30
                  in
                  a31
in
let sideDistanceModel =
  lam subInt6.
    lam distObs.
      lam maxSensorRange.
        let res =
          0.
        in
        let res1 =
          match
            allOutOfRange
              distObs
              maxSensorRange
          with
            true
          then
            let res2 =
              maxSensorRange
            in
            res2
          else
            let res5 =
              medianOfLastThreeFloatTsv
                subInt6
                distObs
            in
            res5
        in
        res1
in
let t4 =
  lam lastObs6: [TSV Float].
    lam subInt5: Int -> Int -> Int.
      lam tofMaxRange5: Float.
        lam state: State.
          stopInit
            (lam #var"6".
               let map =
                 lam f6.
                   let t125 =
                     lam s9.
                       recursive
                         let rec4 =
                           lam s10.
                             let t127 =
                               match
                                 s10
                               with
                                 ""
                               then
                                 let t128 =
                                   ""
                                 in
                                 t128
                               else
                                 let t129 =
                                   match
                                     s10
                                   with
                                     [ a27 ]
                                   then
                                     let t130 =
                                       f6
                                         a27
                                     in
                                     let t131 =
                                       [ t130 ]
                                     in
                                     t131
                                   else
                                     let t132 =
                                       match
                                         s10
                                       with
                                         [ a28 ] ++ ss3 ++ ""
                                       then
                                         let t133 =
                                           cons
                                         in
                                         let t134 =
                                           f6
                                             a28
                                         in
                                         let t135 =
                                           t133
                                             t134
                                         in
                                         let t136 =
                                           rec4
                                             ss3
                                         in
                                         let t137 =
                                           t135
                                             t136
                                         in
                                         t137
                                       else
                                         let t138 =
                                           never
                                         in
                                         t138
                                     in
                                     t132
                                 in
                                 t129
                             in
                             t127
                       in
                       let t126 =
                         rec4
                           s9
                       in
                       t126
                   in
                   t125
               in
               let iter =
                 lam f5.
                   let t122 =
                     lam s8.
                       let t123 =
                         map
                           f5
                       in
                       let #var"8" =
                         t123
                           s8
                       in
                       let t124 =
                         {}
                       in
                       t124
                   in
                   t122
               in
               let mapi =
                 lam f4.
                   let t98 =
                     lam s6.
                       recursive
                         let rec3 =
                           lam i1.
                             let t102 =
                               lam s7.
                                 let t103 =
                                   match
                                     s7
                                   with
                                     ""
                                   then
                                     let t104 =
                                       ""
                                     in
                                     t104
                                   else
                                     let t105 =
                                       match
                                         s7
                                       with
                                         [ a25 ]
                                       then
                                         let t106 =
                                           f4
                                             i1
                                         in
                                         let t107 =
                                           t106
                                             a25
                                         in
                                         let t108 =
                                           [ t107 ]
                                         in
                                         t108
                                       else
                                         let t109 =
                                           match
                                             s7
                                           with
                                             [ a26 ] ++ ss2 ++ ""
                                           then
                                             let t110 =
                                               cons
                                             in
                                             let t111 =
                                               f4
                                                 i1
                                             in
                                             let t112 =
                                               t111
                                                 a26
                                             in
                                             let t113 =
                                               t110
                                                 t112
                                             in
                                             let t114 =
                                               addi
                                             in
                                             let t115 =
                                               t114
                                                 i1
                                             in
                                             let t116 =
                                               1
                                             in
                                             let t117 =
                                               t115
                                                 t116
                                             in
                                             let t118 =
                                               rec3
                                                 t117
                                             in
                                             let t119 =
                                               t118
                                                 ss2
                                             in
                                             let t120 =
                                               t113
                                                 t119
                                             in
                                             t120
                                           else
                                             let t121 =
                                               never
                                             in
                                             t121
                                         in
                                         t109
                                     in
                                     t105
                                 in
                                 t103
                             in
                             t102
                       in
                       let t99 =
                         0
                       in
                       let t100 =
                         rec3
                           t99
                       in
                       let t101 =
                         t100
                           s6
                       in
                       t101
                   in
                   t98
               in
               let iteri =
                 lam f3.
                   let t95 =
                     lam s5.
                       let t96 =
                         mapi
                           f3
                       in
                       let #var"7" =
                         t96
                           s5
                       in
                       let t97 =
                         {}
                       in
                       t97
                   in
                   t95
               in
               let foldl =
                 lam f2.
                   let t83 =
                     lam acc8.
                       let t84 =
                         lam s3.
                           recursive
                             let rec2 =
                               lam acc9.
                                 let t87 =
                                   lam s4.
                                     let t88 =
                                       match
                                         s4
                                       with
                                         ""
                                       then
                                         acc9
                                       else
                                         let t89 =
                                           match
                                             s4
                                           with
                                             [ a24 ] ++ ss1 ++ ""
                                           then
                                             let t90 =
                                               f2
                                                 acc9
                                             in
                                             let t91 =
                                               t90
                                                 a24
                                             in
                                             let t92 =
                                               rec2
                                                 t91
                                             in
                                             let t93 =
                                               t92
                                                 ss1
                                             in
                                             t93
                                           else
                                             let t94 =
                                               never
                                             in
                                             t94
                                         in
                                         t89
                                     in
                                     t88
                                 in
                                 t87
                           in
                           let t85 =
                             rec2
                               acc8
                           in
                           let t86 =
                             t85
                               s3
                           in
                           t86
                       in
                       t84
                   in
                   t83
               in
               let foldr =
                 lam f1.
                   let t71 =
                     lam acc6.
                       let t72 =
                         lam s1.
                           recursive
                             let rec1 =
                               lam acc7.
                                 let t75 =
                                   lam s2.
                                     let t76 =
                                       match
                                         s2
                                       with
                                         ""
                                       then
                                         acc7
                                       else
                                         let t77 =
                                           match
                                             s2
                                           with
                                             [ a23 ] ++ ss ++ ""
                                           then
                                             let t78 =
                                               f1
                                                 a23
                                             in
                                             let t79 =
                                               rec1
                                                 acc7
                                             in
                                             let t80 =
                                               t79
                                                 ss
                                             in
                                             let t81 =
                                               t78
                                                 t80
                                             in
                                             t81
                                           else
                                             let t82 =
                                               never
                                             in
                                             t82
                                         in
                                         t77
                                     in
                                     t76
                                 in
                                 t75
                           in
                           let t73 =
                             rec1
                               acc6
                           in
                           let t74 =
                             t73
                               s1
                           in
                           t74
                       in
                       t72
                   in
                   t71
               in
               let create =
                 lam l.
                   let t47 =
                     lam f.
                       recursive
                         let rec =
                           lam i.
                             let t55 =
                               lam acc5.
                                 let t56 =
                                   geqi
                                 in
                                 let t57 =
                                   t56
                                     i
                                 in
                                 let t58 =
                                   0
                                 in
                                 let t59 =
                                   t57
                                     t58
                                 in
                                 let t60 =
                                   match
                                     t59
                                   with
                                     true
                                   then
                                     let t61 =
                                       subi
                                     in
                                     let t62 =
                                       t61
                                         i
                                     in
                                     let t63 =
                                       1
                                     in
                                     let t64 =
                                       t62
                                         t63
                                     in
                                     let t65 =
                                       rec
                                         t64
                                     in
                                     let t66 =
                                       cons
                                     in
                                     let t67 =
                                       f
                                         i
                                     in
                                     let t68 =
                                       t66
                                         t67
                                     in
                                     let t69 =
                                       t68
                                         acc5
                                     in
                                     let t70 =
                                       t65
                                         t69
                                     in
                                     t70
                                   else
                                     acc5
                                 in
                                 t60
                             in
                             t55
                       in
                       let t48 =
                         subi
                       in
                       let t49 =
                         t48
                           l
                       in
                       let t50 =
                         1
                       in
                       let t51 =
                         t49
                           t50
                       in
                       let t52 =
                         rec
                           t51
                       in
                       let t53 =
                         ""
                       in
                       let t54 =
                         t52
                           t53
                       in
                       t54
                   in
                   t47
               in
               let t7 =
                 {}
               in
               let value =
                 lam tsv2.
                   let t45 =
                     match
                       tsv2
                     with
                       {#label"1" = #var"X"}
                     then
                       #var"X"
                     else
                       let t46 =
                         never
                       in
                       t46
                   in
                   t45
               in
               let subInt3 =
                 subInt5
               in
               let tofMaxRange3 =
                 tofMaxRange5
               in
               let lastObs2 =
                 lastObs6
               in
               let #var"2" =
                 {}
               in
               let subInt6 =
                 subInt3
               in
               let distObs =
                 lastObs2
               in
               let maxSensorRange =
                 tofMaxRange3
               in
               let res =
                 0.
               in
               let distanceObs =
                 distObs
               in
               let maxRange =
                 maxSensorRange
               in
               let acc =
                 true
               in
               let t8 =
                 foldl
               in
               let maxRange1 =
                 maxRange
               in
               let t9 =
                 lam acc2.
                   let t40 =
                     lam tsv1.
                       let t41 =
                         ltf
                       in
                       let t42 =
                         value
                           tsv1
                       in
                       let t43 =
                         t41
                           t42
                       in
                       let t44 =
                         t43
                           maxRange1
                       in
                       let acc3 =
                         match
                           t44
                         with
                           true
                         then
                           let acc4 =
                             false
                           in
                           acc4
                         else
                           acc2
                       in
                       acc3
                   in
                   t40
               in
               let t10 =
                 t8
                   t9
               in
               let t11 =
                 t10
                   acc
               in
               let acc1 =
                 t11
                   distanceObs
               in
               let res1 =
                 match
                   acc1
                 with
                   true
                 then
                   let res2 =
                     maxSensorRange
                   in
                   res2
                 else
                   let subInt7 =
                     subInt6
                   in
                   let tsvs =
                     distObs
                   in
                   let t12 =
                     length
                   in
                   let n =
                     t12
                       tsvs
                   in
                   let t13 =
                     get
                   in
                   let t14 =
                     t13
                       tsvs
                   in
                   let t15 =
                     subInt7
                       n
                   in
                   let t16 =
                     3
                   in
                   let t17 =
                     t15
                       t16
                   in
                   let t18 =
                     t14
                       t17
                   in
                   let a22 =
                     value
                       t18
                   in
                   let t19 =
                     get
                   in
                   let t20 =
                     t19
                       tsvs
                   in
                   let t21 =
                     subInt7
                       n
                   in
                   let t22 =
                     2
                   in
                   let t23 =
                     t21
                       t22
                   in
                   let t24 =
                     t20
                       t23
                   in
                   let b =
                     value
                       t24
                   in
                   let t25 =
                     get
                   in
                   let t26 =
                     t25
                       tsvs
                   in
                   let t27 =
                     subInt7
                       n
                   in
                   let t28 =
                     1
                   in
                   let t29 =
                     t27
                       t28
                   in
                   let t30 =
                     t26
                       t29
                   in
                   let c =
                     value
                       t30
                   in
                   let res3 =
                     0.
                   in
                   let t31 =
                     ltf
                   in
                   let t32 =
                     t31
                       a22
                   in
                   let t33 =
                     t32
                       b
                   in
                   let res4 =
                     match
                       t33
                     with
                       true
                     then
                       let t34 =
                         ltf
                       in
                       let t35 =
                         t34
                           b
                       in
                       let t36 =
                         t35
                           c
                       in
                       let res6 =
                         match
                           t36
                         with
                           true
                         then
                           let res7 =
                             b
                           in
                           res7
                         else
                           let res8 =
                             c
                           in
                           res8
                       in
                       res6
                     else
                       let t37 =
                         ltf
                       in
                       let t38 =
                         t37
                           a22
                       in
                       let t39 =
                         t38
                           c
                       in
                       let res9 =
                         match
                           t39
                         with
                           true
                         then
                           let res10 =
                             a22
                           in
                           res10
                         else
                           let res11 =
                             c
                           in
                           res11
                       in
                       res9
                   in
                   let res5 =
                     res4
                   in
                   res5
               in
               (lam x.
                  End
                    x)
                 res1)
in
recursive
  let t6 =
    lam subInt3.
      lam tofMaxRange3.
        lam lastObs2.
          lam #var"2".
            sideDistanceModel
              subInt3
              lastObs2
              tofMaxRange3
  let loopFn =
    lam fileDescriptors3.
      lam inputSeqs3.
        lam outputSeqs3.
          lam nanosPerSec4.
            lam monoLogicalTime3.
              lam wallLogicalTime4.
                lam nanosPerSec5.
                  lam wallLogicalTime5.
                    lam subInt4.
                      lam negInt3.
                        lam ltInt3.
                          lam gtInt3.
                            lam geqInt3.
                              lam tofMaxRange4.
                                lam period1.
                                  lam lastObs3.
                                    match
                                      true
                                    with
                                      true
                                    then
                                      let #var"3" =
                                        sdelay
                                          nanosPerSec4
                                          monoLogicalTime3
                                          wallLogicalTime4
                                          (flushOutputs
                                             fileDescriptors3
                                             outputSeqs3)
                                          (updateInputs
                                             fileDescriptors3
                                             inputSeqs3)
                                          period1
                                      in
                                      let obs =
                                        unsafeCoerce
                                          ((deref
                                              inputSeqs3).distObs)
                                      in
                                      let lastObs4 =
                                        match
                                          gtInt3
                                            (length
                                               obs)
                                            0
                                        with
                                          true
                                        then
                                          let lastObs5 =
                                            takeThreeMostRecent
                                              nanosPerSec5
                                              wallLogicalTime5
                                              subInt4
                                              negInt3
                                              ltInt3
                                              gtInt3
                                              geqInt3
                                              (concat
                                                 lastObs3
                                                 obs)
                                          in
                                          lastObs5
                                        else
                                          lastObs3
                                      in
                                      let #var"4" =
                                        match
                                          geqInt3
                                            (length
                                               lastObs4)
                                            3
                                        with
                                          true
                                        then
                                          let d =
                                            run
                                              { particles =
                                                  1000 }
                                              (t4
                                                 lastObs4
                                                 subInt4
                                                 tofMaxRange4)
                                          in
                                          let #var"5" =
                                            let out =
                                              deref
                                                outputSeqs3
                                            in
                                            modref
                                              outputSeqs3
                                              { out
                                                with
                                                distEst =
                                                  cons
                                                    (tsv
                                                       nanosPerSec4
                                                       wallLogicalTime4
                                                       0
                                                       d)
                                                    (out.distEst) }
                                          in
                                          {}
                                        else
                                          {}
                                      in
                                      loopFn
                                        fileDescriptors3
                                        inputSeqs3
                                        outputSeqs3
                                        nanosPerSec4
                                        monoLogicalTime3
                                        wallLogicalTime4
                                        nanosPerSec5
                                        wallLogicalTime5
                                        subInt4
                                        negInt3
                                        ltInt3
                                        gtInt3
                                        geqInt3
                                        tofMaxRange4
                                        period1
                                        lastObs4
                                    else
                                      lastObs3
in
let #var"RTPPL_sideDistance" =
  lam fileDescriptors2.
    lam inputSeqs2.
      lam outputSeqs2.
        lam nanosPerSec2.
          lam monoLogicalTime2.
            lam wallLogicalTime2.
              lam nanosPerSec3.
                lam wallLogicalTime3.
                  lam subInt2.
                    lam negInt2.
                      lam ltInt2.
                        lam gtInt2.
                          lam geqInt2.
                            lam tofMaxRange2.
                              lam period.
                                let lastObs =
                                  ""
                                in
                                let lastObs1 =
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
                                    negInt2
                                    ltInt2
                                    gtInt2
                                    geqInt2
                                    tofMaxRange2
                                    period
                                    lastObs
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
              lam subInt1.
                lam negInt1.
                  lam ltInt1.
                    lam gtInt1.
                      lam geqInt1.
                        lam tofMaxRange1.
                          lam #var"1".
                            #var"RTPPL_sideDistance"
                              fileDescriptors1
                              inputSeqs1
                              outputSeqs1
                              nanosPerSec1
                              monoLogicalTime1
                              wallLogicalTime1
                              nanosPerSec1
                              wallLogicalTime1
                              subInt1
                              negInt1
                              ltInt1
                              gtInt1
                              geqInt1
                              tofMaxRange1
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
     subInt
     negInt
     ltInt
     gtInt
     geqInt
     tofMaxRange)