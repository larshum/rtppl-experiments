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
          (Some v15, Some v23)
        then
          eq6
            v15
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
        Some t835
      then
        Some
          (f62
             t835)
      else
        None
          {}
in
let optionMapAccum: all a131. all b43. all acc80. (acc80 -> a131 -> (acc80, b43)) -> acc80 -> Option a131 -> (acc80, Option b43) =
  lam f61.
    lam acc81.
      lam o18.
        match
          o18
        with
          Some a132
        then
          match
            f61
              acc81
              a132
          with
            (acc82, b44)
          in
          (acc82, Some
              b44)
        else
          (acc81, None
            {})
in
let optionJoin: all a130. Option (Option a130) -> Option a130 =
  lam o17.
    match
      o17
    with
      Some t834
    then
      t834
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
          (Some v14, Some v22)
        then
          Some
            (f58
               v14
               v22)
        else
          None
            {}
in
let optionZipWithOrElse: all a126. all b39. all c38. (() -> c38) -> (a126 -> b39 -> c38) -> Option a126 -> Option b39 -> c38 =
  lam d18.
    lam f57.
      lam o14.
        lam o25.
          match
            (o14, o25)
          with
            (Some v13, Some v21)
          then
            f57
              v13
              v21
          else
            d18
              {}
in
let optionZipWithOr: all a125. all b38. all c37. c37 -> (a125 -> b38 -> c37) -> Option a125 -> Option b38 -> c37 =
  lam v12.
    optionZipWithOrElse
      (lam #var"101".
         v12)
in
let optionGetOrElse: all a124. (() -> a124) -> Option a124 -> a124 =
  lam d17.
    lam o10.
      match
        o10
      with
        Some t833
      then
        t833
      else
        d17
          {}
in
let optionGetOr: all a123. a123 -> Option a123 -> a123 =
  lam d16.
    optionGetOrElse
      (lam #var"100".
         d16)
in
let optionMapOrElse: all a122. all b37. (() -> b37) -> (a122 -> b37) -> Option a122 -> b37 =
  lam d15.
    lam f56.
      lam o9.
        optionGetOrElse
          d15
          (optionMap
             f56
             o9)
in
let optionMapOr: all a121. all b36. b36 -> (a121 -> b36) -> Option a121 -> b36 =
  lam d14.
    lam f55.
      lam o8.
        optionGetOr
          d14
          (optionMap
             f55
             o8)
in
let optionMapM: all a120. all b35. (a120 -> Option b35) -> [a120] -> Option [b35] =
  lam f54.
    lam l19.
      recursive
        let g2 =
          lam l20.
            lam acc79.
              match
                l20
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
                       acc79
                       x115)
                else
                  None
                    {}
              else
                Some
                  acc79
      in
      g2
        l19
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
    lam p31.
      optionMapOr
        false
        p31
        o7
in
let optionIsSome: all a115. Option a115 -> Bool =
  lam o6.
    optionContains
      o6
      (lam #var"99".
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
  lam p30.
    lam o4.
      match
        optionContains
          o4
          p30
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
        (lam #var"98".
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
    lam v11.
      create
        n20
        (lam #var"97".
           v11)
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
let eqSeq: all a105. all b32. (a105 -> b32 -> Bool) -> [a105] -> [b32] -> Bool =
  lam eq5.
    lam s121.
      lam s221.
        recursive
          let work17 =
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
                    work17
                      t1102
                      t2102
                  else
                    false
                else
                  true
        in
        let n111 =
          length
            s121
        in
        let n23 =
          length
            s221
        in
        let ndiff1 =
          subi
            n111
            n23
        in
        match
          eqi
            ndiff1
            0
        with
          true
        then
          work17
            s121
            s221
        else
          false
in
let toRope =
  lam seq33.
    createRope
      (length
         seq33)
      (lam i24.
         get
           seq33
           i24)
in
let toList =
  lam seq32.
    createList
      (length
         seq32)
      (lam i23.
         get
           seq32
           i23)
in
let mapOption: all a103. all b30. (a103 -> Option b30) -> [a103] -> [b30] =
  lam f51.
    recursive
      let work16 =
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
                (work16
                   as4)
            else
              work16
                as4
          else
            ""
    in
    work16
in
let for_: all a102. [a102] -> (a102 -> ()) -> () =
  lam xs9.
    lam f50.
      iter
        f50
        xs9
in
let mapReverse =
  lam f49.
    lam lst1.
      foldl
        (lam acc78.
           lam x109.
             cons
               (f49
                  x109)
               acc78)
        (toList
           "")
        lst1
in
let mapK: all a101. all b29. all c36. (a101 -> (b29 -> c36) -> c36) -> [a101] -> ([b29] -> c36) -> c36 =
  lam f48.
    lam seq31.
      lam k2.
        foldl
          (lam k3.
             lam x107.
               lam xs8.
                 f48
                   x107
                   (lam x108.
                      k3
                        (cons
                           x108
                           xs8)))
          k2
          seq31
          ""
in
let foldl1 =
  lam f47.
    lam l18.
      foldl
        f47
        (head
           l18)
        (tail
           l18)
in
let foldr1 =
  lam f46.
    lam seq30.
      foldr
        f46
        (last
           seq30)
        (init
           seq30)
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
  lam s76.
    lam e6.
      lam by3.
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
                   by3))
          s76
in
recursive
  let foldl21: all a99. all b27. all c34. (a99 -> b27 -> c34 -> a99) -> a99 -> [b27] -> [c34] -> a99 =
    lam f44.
      lam acc73.
        lam seq112.
          lam seq29.
            let g1 =
              lam acc76: (a99, [b27]).
                lam x213.
                  match
                    acc76
                  with
                    (acc77, [ x113 ] ++ xs11 ++ "")
                  in
                  (f44
                      acc77
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
                  (acc73, seq112)
                  seq29
              with
                (acc74, _)
              in
              acc74
            else
              foldl21
                (lam acc75.
                   lam x112.
                     lam x212.
                       f44
                         acc75
                         x212
                         x112)
                acc73
                seq29
                seq112
in
let foldli: all a98. all b26. (a98 -> Int -> b26 -> a98) -> a98 -> [b26] -> a98 =
  lam fn.
    lam initAcc.
      lam seq28.
        recursive
          let work15 =
            lam acc72.
              lam i22.
                lam s75.
                  match
                    s75
                  with
                    [ e5 ] ++ rest ++ ""
                  then
                    work15
                      (fn
                         acc72
                         i22
                         e5)
                      (addi
                         i22
                         1)
                      rest
                  else
                    acc72
        in
        work15
          initAcc
          0
          seq28
in
let zipWith: all a97. all b25. all c33. (a97 -> b25 -> c33) -> [a97] -> [b25] -> [c33] =
  lam f43.
    foldl21
      (lam acc71.
         lam x111.
           lam x211.
             snoc
               acc71
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
          let work14 =
            lam acc70.
              lam i21.
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
                        work14
                          (cons
                             (f42
                                i21
                                e11
                                e21)
                             acc70)
                          (addi
                             i21
                             1)
                          seq1tail
                          seq2tail
                      else
                        reverse
                          acc70
                    else
                      reverse
                        acc70
        in
        work14
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
    lam acc68.
      lam seq26.
        foldl
          (lam tacc1: (a94, [c31]).
             lam x104.
               match
                 f41
                   (tacc1.0)
                   x104
               with
                 (acc69, y10)
               in
               (acc69, snoc
                   (tacc1.1)
                   y10))
          (acc68, "")
          seq26
in
let mapAccumR: all a93. all b21. all c30. (a93 -> b21 -> (a93, c30)) -> a93 -> [b21] -> (a93, [c30]) =
  lam f40: a93 -> b21 -> (a93, c30).
    lam acc66.
      lam seq25.
        foldr
          (lam x103.
             lam tacc: (a93, [c30]).
               match
                 f40
                   (tacc.0)
                   x103
               with
                 (acc67, y9)
               in
               (acc67, cons
                   y9
                   (tacc.1)))
          (acc66, "")
          seq25
in
let unzip: all a92. all b20. [(a92, b20)] -> ([a92], [b20]) =
  mapAccumL
    (lam l17.
       lam p29: (a92, b20).
         (snoc
           l17
           (p29.0), p29.1))
    ""
in
let iter2: all a91. all b19. (a91 -> b19 -> ()) -> [a91] -> [b19] -> () =
  lam f38.
    lam seq110.
      lam seq24.
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
             seq24)
in
recursive
  let any =
    lam p28.
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
            p28
              (head
                 seq23)
          with
            true
          then
            true
          else
            any
              p28
              (tail
                 seq23)
in
recursive
  let forAll =
    lam p27.
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
            p27
              (head
                 seq22)
          with
            true
          then
            forAll
              p27
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
         lam acc65.
           seqLiftA2
             cons
             (f36
                a88)
             acc65)
      [ "" ]
in
recursive
  let filter =
    lam p26.
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
            p26
              (head
                 seq20)
          with
            true
          then
            cons
              (head
                 seq20)
              (filter
                 p26
                 (tail
                    seq20))
          else
            filter
              p26
              (tail
                 seq20)
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
    lam p25.
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
            p25
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
              p25
              (tail
                 seq19)
in
recursive
  let findMap: all a85. all b16. (a85 -> Option b16) -> [a85] -> Option b16 =
    lam f35.
      lam seq18.
        match
          seq18
        with
          [ h4 ] ++ t832 ++ ""
        then
          match
            f35
              h4
          with
            Some x100
          then
            Some
              x100
          else
            findMap
              f35
              t832
        else
          None
            {}
in
let lowerBoundBinarySearch: all a84. (a84 -> Int) -> [a84] -> Option Int =
  lam f34.
    lam s74.
      recursive
        let work13 =
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
                          s74
                          idx3))
                    0
                with
                  true
                then
                  work13
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
                  work13
                    first
                    step1
              else
                first
      in
      let idx2 =
        work13
          0
          (length
             s74)
      in
      match
        eqi
          idx2
          (length
             s74)
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
  lam p24.
    lam seq15.
      recursive
        let work12 =
          lam l16.
            lam r13.
              lam seq16.
                match
                  seq16
                with
                  ""
                then
                  (l16, r13)
                else
                  match
                    seq16
                  with
                    [ s73 ] ++ seq17 ++ ""
                  in
                  match
                      p24
                        s73
                    with
                      true
                    then
                      work12
                        (cons
                           s73
                           l16)
                        r13
                        seq17
                    else
                      work12
                        l16
                        (cons
                           s73
                           r13)
                        seq17
      in
      work12
        ""
        ""
        (reverse
           seq15)
in
let distinct =
  lam eq4.
    lam seq13.
      recursive
        let work11 =
          lam seq14.
            lam seq21.
              match
                seq14
              with
                [ h3 ] ++ t831 ++ ""
              then
                match
                  find
                    (eq4
                       h3)
                    seq21
                with
                  Some _
                then
                  work11
                    t831
                    seq21
                else
                  cons
                    h3
                    (work11
                       t831
                       (cons
                          h3
                          seq21))
              else
                ""
      in
      work11
        seq13
        ""
in
let distinctSorted =
  lam eq3.
    lam s71.
      recursive
        let work10 =
          lam acc64.
            lam s72.
              match
                s72
              with
                [ h12 ] ++ t830 ++ ""
              then
                match
                  acc64
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
                    work10
                      acc64
                      t830
                  else
                    work10
                      (cons
                         h12
                         acc64)
                      t830
                else
                  work10
                    [ h12 ]
                    t830
              else
                acc64
      in
      reverse
        (work10
           ""
           s71)
in
recursive
  let quickSort: all a83. (a83 -> a83 -> Int) -> [a83] -> [a83] =
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
          let t829 =
            tail
              seq12
          in
          let lr1 =
            partition
              (lam x99.
                 lti
                   (cmp10
                      x99
                      h)
                   0)
              t829
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
      lam l15.
        lam r12.
          match
            l15
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
              l15
            else
              match
                (l15, r12)
              with
                ([ x98 ] ++ xs7 ++ "", [ y8 ] ++ ys1 ++ "")
              in
              match
                  leqi
                    (cmp9
                       x98
                       y8)
                    0
                with
                  true
                then
                  cons
                    x98
                    (merge
                       cmp9
                       xs7
                       r12)
                else
                  cons
                    y8
                    (merge
                       cmp9
                       l15
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
            [ x97 ]
          then
            [ x97 ]
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
let minIdx: all a82. (a82 -> a82 -> Int) -> [a82] -> Option (Int, a82) =
  lam cmp7: a82 -> a82 -> Int.
    lam seq10: [a82].
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
            (lam acc63: (Int, Int, a82).
               lam e4: a82.
                 match
                   acc63
                 with
                   (curi, mini1, m18)
                 in
                 match
                     lti
                       (cmp7
                          m18
                          e4)
                       0
                   with
                     true
                   then
                     (addi
                       curi
                       1, mini1, m18)
                   else
                     (addi
                       curi
                       1, curi, e4))
            (1, 0, head
              seq10)
            (tail
               seq10)
        with
          (_, i20, m19)
        in
        Some
            (i20, m19)
in
let min: all a81. (a81 -> a81 -> Int) -> [a81] -> Option a81 =
  lam cmp6.
    lam seq9.
      optionMap
        (lam r11.
           match
             r11
           with
             (_, m17)
           in
           m17)
        (minIdx
           cmp6
           seq9)
in
let max =
  lam cmp5.
    min
      (lam l14.
         lam r10.
           cmp5
             r10
             l14)
in
let minOrElse =
  lam d13.
    lam cmp4.
      lam seq8.
        optionGetOrElse
          d13
          (min
             cmp4
             seq8)
in
let maxOrElse =
  lam d12.
    lam cmp3.
      minOrElse
        d12
        (lam l13.
           lam r9.
             cmp3
               r9
               l13)
in
let index =
  lam pred3.
    lam seq6.
      recursive
        let index_rechelper =
          lam i19.
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
                      i19
                  else
                    index_rechelper
                      (addi
                         i19
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
          lam i18.
            lam acc62.
              lam pred2.
                lam seq5.
                  match
                    null
                      seq5
                  with
                    true
                  then
                    acc62
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
                           i18
                           1)
                        (Some
                           i18)
                        pred2
                        (tail
                           seq5)
                    else
                      lastIndex_rechelper
                        (addi
                           i18
                           1)
                        acc62
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
  lam cmp2.
    lam s117.
      lam s217.
        recursive
          let work9 =
            lam s118.
              lam s218.
                match
                  (s118, s218)
                with
                  ([ h11 ] ++ t1101 ++ "", [ h21 ] ++ t2101 ++ "")
                then
                  let c28 =
                    cmp2
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
                    work9
                      t1101
                      t2101
                  else
                    c28
                else
                  0
        in
        let n110 =
          length
            s117
        in
        let n22 =
          length
            s217
        in
        let ndiff =
          subi
            n110
            n22
        in
        match
          eqi
            ndiff
            0
        with
          true
        then
          work9
            s117
            s217
        else
          ndiff
in
let randIndex: all a79. [a79] -> Option Int =
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
let randElem: all a78. [a78] -> Option a78 =
  lam seq2.
    optionMap
      (get
         seq2)
      (randIndex
         seq2)
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
    lam #var"96".
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
    lam t828: (a76, b15).
      f30
        (t828.0)
        (t828.1)
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
  lam s70.
    let #var"95" =
      print
        (concat
           s70
           "\n")
    in
    flushStdout
      {}
in
let printSeq =
  lam s69.
    print
      (join
         s69)
in
let printSeqLn =
  lam s68.
    let #var"93" =
      printSeq
        s68
    in
    let #var"94" =
      print
        "\n"
    in
    flushStdout
      {}
in
let dprintLn =
  lam x89.
    let #var"92" =
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
    lam n18.
      recursive
        let rec19 =
          lam n19.
            match
              leqi
                n19
                0
            with
              true
            then
              {}
            else
              let #var"91" =
                f27
                  {}
              in
              rec19
                (subi
                   n19
                   1)
      in
      rec19
        n18
in
let repeati: (Int -> ()) -> Int -> () =
  lam f26.
    lam n17.
      recursive
        let rec18 =
          lam i17.
            match
              geqi
                i17
                n17
            with
              true
            then
              {}
            else
              let #var"90" =
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
             (lam li: ([a74 -> b13] -> a74 -> b13,).
                lam x88.
                  (li.0)
                    (self
                       l12)
                    x88)
             l12)
      l11
in
let maxf: Float -> Float -> Float =
  lam r8.
    lam l9.
      match
        gtf
          r8
          l9
      with
        true
      then
        r8
      else
        l9
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
    lam r7.
      lam l8.
        match
          leqf
            (absf
               (subf
                  r7
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
  lam r6.
    lam l7.
      match
        ltf
          r6
          l7
      with
        true
      then
        r6
      else
        l7
in
let cmpfApprox: Float -> Float -> Float -> Int =
  lam epsilon.
    lam l6.
      lam r5.
        match
          eqfApprox
            epsilon
            l6
            r5
        with
          true
        then
          0
        else
          match
            ltf
              l6
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
  lam n15.
    recursive
      let work8 =
        lam acc61.
          lam n16.
            match
              gti
                n16
                0
            with
              true
            then
              work8
                (addf
                   (log
                      (int2float
                         n16))
                   acc61)
                (subi
                   n16
                   1)
            else
              acc61
    in
    work8
      0.
      n15
in
let maxi =
  lam r4.
    lam l5.
      match
        gti
          r4
          l5
      with
        true
      then
        r4
      else
        l5
in
let mini =
  lam r3.
    lam l4.
      match
        lti
          r3
          l4
      with
        true
      then
        r3
      else
        l4
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
  lam p23: Float.
    lam n14: Int.
      lam x75: Int.
        exp
          (externalBinomialLogPmf
             x75
             p23
             n14)
in
let binomialLogPmf =
  lam p22: Float.
    lam n13: Int.
      lam x74: Int.
        externalBinomialLogPmf
          x74
          p22
          n13
in
let binomialSample =
  lam p21: Float.
    lam n12: Int.
      externalBinomialSample
        p21
        n12
in
let bernoulliPmf =
  lam p20: Float.
    lam x73: Bool.
      match
        x73
      with
        true
      then
        p20
      else
        subf
          1.
          p20
in
let bernoulliLogPmf =
  lam p19: Float.
    lam x72: Bool.
      log
        (bernoulliPmf
           p19
           x72)
in
let bernoulliSample =
  lam p18: Float.
    match
      eqi
        1
        (externalBinomialSample
           p18
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
    lam sigma7: Float.
      lam x69: Float.
        exp
          (externalGaussianLogPdf
             x69
             mu2
             sigma7)
in
let gaussianLogPdf =
  lam mu1: Float.
    lam sigma6: Float.
      lam x68: Float.
        externalGaussianLogPdf
          x68
          mu1
          sigma6
in
let gaussianSample =
  lam mu: Float.
    lam sigma5: Float.
      externalGaussianSample
        mu
        sigma5
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
    lam n11.
      externalMultinomialSample
        n11
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
  lam #var"89".
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
      Some n9
    then
      let n10: (Char, [Char]) =
        n9
      in
      n10.1
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
  lam #var"88".
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
  lam s67.
    join
      (map
         escapeChar
         s67)
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
            let work7 =
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
                    work7
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
            work7
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
  lam s66.
    map
      char2upper
      s66
in
let str2lower =
  lam s65.
    map
      char2lower
      s65
in
let string2int =
  lam s63.
    recursive
      let string2int_rechelper1 =
        lam s64.
          lam acc60.
            match
              null
                s64
            with
              true
            then
              acc60
            else
              let fsd1 =
                subi
                  (char2int
                     (head
                        s64))
                  (char2int
                     '0')
              in
              string2int_rechelper1
                (tail
                   s64)
                (addi
                   (muli
                      10
                      acc60)
                   fsd1)
    in
    match
      s63
    with
      ""
    then
      0
    else
      match
        eqChar
          '-'
          (head
             s63)
      with
        true
      then
        negi
          (string2int_rechelper1
             (tail
                s63)
             0)
      else
        string2int_rechelper1
          s63
          0
in
let digit2char =
  lam d11.
    int2char
      (addi
         d11
         (char2int
            '0'))
in
let int2string =
  lam n7.
    recursive
      let int2string_rechelper =
        lam n8.
          lam acc59.
            match
              lti
                n8
                10
            with
              true
            then
              cons
                (digit2char
                   n8)
                acc59
            else
              int2string_rechelper
                (divi
                   n8
                   10)
                (cons
                   (digit2char
                      (modi
                         n8
                         10))
                   acc59)
    in
    match
      lti
        n7
        0
    with
      true
    then
      cons
        '-'
        (int2string_rechelper
           (negi
              n7)
           "")
    else
      int2string_rechelper
        n7
        ""
in
let stringIsInt: [Char] -> Bool =
  lam s62.
    eqString
      s62
      (int2string
         (string2int
            s62))
in
let strIndex =
  lam c6.
    lam s60.
      recursive
        let strIndex_rechelper =
          lam i11.
            lam c7.
              lam s61.
                match
                  null
                    s61
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
                         s61)
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
                         s61)
      in
      strIndex_rechelper
        0
        c6
        s60
in
let strLastIndex =
  lam c4.
    lam s58.
      recursive
        let strLastIndex_rechelper =
          lam i10.
            lam acc58.
              lam c5.
                lam s59.
                  match
                    null
                      s59
                  with
                    true
                  then
                    match
                      eqi
                        acc58
                        (negi
                           1)
                    with
                      true
                    then
                      None
                        {}
                    else
                      Some
                        acc58
                  else
                    match
                      eqChar
                        c5
                        (head
                           s59)
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
                           s59)
                    else
                      strLastIndex_rechelper
                        (addi
                           i10
                           1)
                        acc58
                        c5
                        (tail
                           s59)
      in
      strLastIndex_rechelper
        0
        (negi
           1)
        c4
        s58
in
let strSplit =
  lam delim2.
    lam s57.
      let n6 =
        length
          s57
      in
      let m16 =
        length
          delim2
      in
      recursive
        let work6 =
          lam acc57.
            lam lastMatch1.
              lam i9.
                match
                  lti
                    (subi
                       n6
                       m16)
                    i9
                with
                  true
                then
                  snoc
                    acc57
                    (subsequence
                       s57
                       lastMatch1
                       n6)
                else
                  match
                    eqStringSlice
                      delim2
                      s57
                      i9
                      m16
                  with
                    true
                  then
                    let nexti1 =
                      addi
                        i9
                        m16
                    in
                    work6
                      (snoc
                         acc57
                         (subsequence
                            s57
                            lastMatch1
                            (subi
                               i9
                               lastMatch1)))
                      nexti1
                      nexti1
                  else
                    work6
                      acc57
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
        [ s57 ]
      else
        work6
          ""
          0
          0
in
let strTrim =
  lam s55.
    recursive
      let strTrim_init1 =
        lam s56.
          match
            eqString
              s56
              ""
          with
            true
          then
            s56
          else
            match
              isWhitespace
                (head
                   s56)
            with
              true
            then
              strTrim_init1
                (tail
                   s56)
            else
              s56
    in
    reverse
      (strTrim_init1
         (reverse
            (strTrim_init1
               s55)))
in
let stringIsInt1 =
  lam s53.
    match
      null
        s53
    with
      true
    then
      false
    else
      let s54 =
        match
          eqChar
            (get
               s53
               0)
            '-'
        with
          true
        then
          tail
            s53
        else
          s53
      in
      forAll
        isDigit
        s54
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
  lam s52.
    match
      fileExists
        s52
    with
      true
    then
      deleteFile
        s52
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
    lam s51.
      writeString
        c3
        s51
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
      (s50, false)
    then
      Some
        s50
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
  lam #var"86".
    match
      neqi
        (length
           argv)
        2
    with
      true
    then
      let #var"87" =
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
            let #var"82" =
              writeString1
                ch
                (strJoin
                   ","
                   names4)
            in
            let #var"83" =
              writeString1
                ch
                "\n"
            in
            let #var"84" =
              iter
                (lam lst.
                   let #var"85" =
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
              lam s49.
                lam n5.
                  match
                    geqi
                      n5
                      (length
                         s49)
                  with
                    true
                  then
                    let #var"80" =
                      print
                        s49
                    in
                    print
                      (create
                         (subi
                            n5
                            (length
                               s49))
                         (lam #var"81".
                            ' '))
                  else
                    print
                      s49
            in
            let #var"68" =
              padPrint
                "Variable"
                14
            in
            let #var"69" =
              padPrint
                "Expected Value"
                pad
            in
            let #var"70" =
              padPrint
                "Variance"
                pad
            in
            let #var"71" =
              padPrint
                "Standard Deviation"
                pad
            in
            let #var"72" =
              print
                "\n"
            in
            recursive
              let work5 =
                lam names3.
                  lam ev.
                    lam vv.
                      match
                        (names3, ev, vv)
                      with
                        ([ n4 ] ++ ns4 ++ "", [ e1 ] ++ es1 ++ "", [ v10 ] ++ vs ++ "")
                      then
                        match
                          isPrefix
                            eqChar
                            "#"
                            n4
                        with
                          true
                        then
                          work5
                            ns4
                            ev
                            vv
                        else
                          let #var"75" =
                            padPrint
                              n4
                              14
                          in
                          let #var"76" =
                            padPrint
                              (float2string
                                 e1)
                              pad
                          in
                          let #var"77" =
                            padPrint
                              (float2string
                                 v10)
                              pad
                          in
                          let #var"78" =
                            padPrint
                              (float2string
                                 (sqrt
                                    v10))
                              pad
                          in
                          let #var"79" =
                            print
                              "\n"
                          in
                          work5
                            ns4
                            es1
                            vs
                      else
                        {}
            in
            let #var"73" =
              work5
                names2
                expVals2
                varianceVals1
            in
            let #var"74" =
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
  lam res8.
    let max1 =
      foldl
        (lam acc56.
           lam x55.
             match
               geqf
                 x55
                 acc56
             with
               true
             then
               x55
             else
               acc56)
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
          (lam acc55.
             lam x54.
               addf
                 (exp
                    (subf
                       x54
                       max1))
                 acc55)
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
        (lam acc53.
           lam t827.
             let w6 =
               exp
                 (subf
                    (head
                       t827)
                    normConst2)
             in
             let ys =
               tail
                 t827
             in
             recursive
               let work4 =
                 lam acc54.
                   lam xs3.
                     match
                       (acc54, xs3)
                     with
                       ([ a63 ] ++ as1 ++ "", [ x53 ] ++ xs4 ++ "")
                     then
                       cons
                         (addf
                            (mulf
                               x53
                               w6)
                            a63)
                         (work4
                            as1
                            xs4)
                     else
                       ""
             in
             work4
               acc53
               ys)
        (create
           (subi
              (length
                 (head
                    res7))
              1)
           (lam #var"67".
              0.))
        res7
in
let variance =
  lam res6.
    lam expVals1.
      let sum =
        foldl
          (lam acc51.
             lam t826.
               recursive
                 let work3 =
                   lam acc52.
                     lam xs1.
                       lam expv.
                         match
                           (acc52, xs1, expv)
                         with
                           ([ a62 ] ++ as ++ "", [ x52 ] ++ xs2 ++ "", [ e ] ++ es ++ "")
                         then
                           let v9 =
                             subf
                               x52
                               e
                           in
                           cons
                             (addf
                                a62
                                (mulf
                                   v9
                                   v9))
                             (work3
                                as
                                xs2
                                es)
                         else
                           ""
               in
               work3
                 acc51
                 (tail
                    t826)
                 expVals1)
          (create
             (subi
                (length
                   (head
                      res6))
                1)
             (lam #var"66".
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
      (lam t825.
         match
           t825
         with
           [ x50 ] ++ xs ++ ""
         in
         cons
             (exp
                x50)
             xs)
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
      let nc1 =
        normConstant
          (map
             head
             res4)
      in
      let expVals =
        expectedValues
          res4
          nc1
      in
      let varianceVals =
        variance
          res4
          expVals
      in
      let #var"65" =
        printStatistics
          res4
          names1
          nc1
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
      lam samples11.
        recursive
          let rec16: [Float] -> [a61] -> () =
            lam weights5.
              lam samples12.
                match
                  null
                    weights5
                with
                  true
                then
                  {}
                else
                  let w5 =
                    head
                      weights5
                  in
                  let weights6 =
                    tail
                      weights5
                  in
                  let s48 =
                    head
                      samples12
                  in
                  let samples13 =
                    tail
                      samples12
                  in
                  let #var"61" =
                    print
                      (printFun1
                         s48)
                  in
                  let #var"62" =
                    print
                      " "
                  in
                  let #var"63" =
                    print
                      (float2string
                         w5)
                  in
                  let #var"64" =
                    print
                      "\n"
                  in
                  rec16
                    weights6
                    samples13
        in
        match
          compileOptions.printSamples
        with
          true
        then
          rec16
            weights4
            samples11
        else
          {}
in
let printSamplesOption: all a60. (a60 -> [Char]) -> [Float] -> [Option a60] -> () =
  lam printFun.
    lam weights1.
      lam samples8.
        recursive
          let rec15: [Float] -> [Option a60] -> () =
            lam weights2.
              lam samples9.
                match
                  null
                    weights2
                with
                  true
                then
                  {}
                else
                  let w4 =
                    head
                      weights2
                  in
                  let weights3 =
                    tail
                      weights2
                  in
                  let s46 =
                    head
                      samples9
                  in
                  let samples10 =
                    tail
                      samples9
                  in
                  let #var"57" =
                    match
                      s46
                    with
                      Some s47
                    then
                      print
                        (printFun
                           s47)
                    else
                      print
                        "."
                  in
                  let #var"58" =
                    print
                      " "
                  in
                  let #var"59" =
                    print
                      (float2string
                         w4)
                  in
                  let #var"60" =
                    print
                      "\n"
                  in
                  rec15
                    weights3
                    samples10
        in
        match
          compileOptions.printSamples
        with
          true
        then
          rec15
            weights1
            samples8
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
  lam n3.
    let #var"56" =
      modref
        _mcmcSamples
        n3
    in
    modref
      _mcmcAccepts
      0
in
let mcmcAccept =
  lam #var"55".
    modref
      _mcmcAccepts
      (addi
         (deref
            _mcmcAccepts)
         1)
in
let mcmcAcceptRate =
  lam #var"54".
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
        RuntimeDistElementary_DistGamma t803
      then
        unsafeCoerce
          (gammaSample
             (t803.shape)
             (t803.scale))
      else
        match
          __sem_target18
        with
          RuntimeDistElementary_DistExponential t804
        then
          unsafeCoerce
            (exponentialSample
               (t804.rate))
        else
          match
            __sem_target18
          with
            RuntimeDistElementary_DistPoisson t805
          then
            unsafeCoerce
              (poissonSample
                 (t805.lambda))
          else
            match
              __sem_target18
            with
              RuntimeDistElementary_DistBinomial t806
            then
              unsafeCoerce
                (binomialSample
                   (t806.p)
                   (t806.n))
            else
              match
                __sem_target18
              with
                RuntimeDistElementary_DistBernoulli t807
              then
                unsafeCoerce
                  (bernoulliSample
                     (t807.p))
              else
                match
                  __sem_target18
                with
                  RuntimeDistElementary_DistBeta t808
                then
                  unsafeCoerce
                    (betaSample
                       (t808.a)
                       (t808.b))
                else
                  match
                    __sem_target18
                  with
                    RuntimeDistElementary_DistGaussian t809
                  then
                    unsafeCoerce
                      (gaussianSample
                         (t809.mu)
                         (t809.sigma))
                  else
                    match
                      __sem_target18
                    with
                      RuntimeDistElementary_DistMultinomial t810
                    then
                      unsafeCoerce
                        (multinomialSample
                           (t810.p)
                           (t810.n))
                    else
                      match
                        __sem_target18
                      with
                        RuntimeDistElementary_DistCategorical t811
                      then
                        unsafeCoerce
                          (categoricalSample
                             (t811.p))
                      else
                        match
                          __sem_target18
                        with
                          RuntimeDistElementary_DistDirichlet t812
                        then
                          unsafeCoerce
                            (dirichletSample
                               (t812.a))
                        else
                          match
                            __sem_target18
                          with
                            RuntimeDistElementary_DistUniform t813
                          then
                            unsafeCoerce
                              (uniformContinuousSample
                                 (t813.a)
                                 (t813.b))
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
        RuntimeDistElementary_DistGamma t814
      then
        unsafeCoerce
          (gammaLogPdf
             (t814.shape)
             (t814.scale))
      else
        match
          __sem_target19
        with
          RuntimeDistElementary_DistExponential t815
        then
          unsafeCoerce
            (exponentialLogPdf
               (t815.rate))
        else
          match
            __sem_target19
          with
            RuntimeDistElementary_DistPoisson t816
          then
            unsafeCoerce
              (poissonLogPmf
                 (t816.lambda))
          else
            match
              __sem_target19
            with
              RuntimeDistElementary_DistBinomial t817
            then
              unsafeCoerce
                (binomialLogPmf
                   (t817.p)
                   (t817.n))
            else
              match
                __sem_target19
              with
                RuntimeDistElementary_DistBernoulli t818
              then
                unsafeCoerce
                  (bernoulliLogPmf
                     (t818.p))
              else
                match
                  __sem_target19
                with
                  RuntimeDistElementary_DistBeta t819
                then
                  unsafeCoerce
                    (betaLogPdf
                       (t819.a)
                       (t819.b))
                else
                  match
                    __sem_target19
                  with
                    RuntimeDistElementary_DistGaussian t820
                  then
                    unsafeCoerce
                      (gaussianLogPdf
                         (t820.mu)
                         (t820.sigma))
                  else
                    match
                      __sem_target19
                    with
                      RuntimeDistElementary_DistMultinomial t821
                    then
                      unsafeCoerce
                        (lam o1.
                           match
                             eqi
                               (t821.n)
                               (foldl1
                                  addi
                                  o1)
                           with
                             true
                           then
                             multinomialLogPmf
                               (t821.p)
                               o1
                           else
                             negf
                               inf)
                    else
                      match
                        __sem_target19
                      with
                        RuntimeDistElementary_DistCategorical t822
                      then
                        unsafeCoerce
                          (categoricalLogPmf
                             (t822.p))
                      else
                        match
                          __sem_target19
                        with
                          RuntimeDistElementary_DistDirichlet t823
                        then
                          unsafeCoerce
                            (dirichletLogPdf
                               (t823.a))
                        else
                          match
                            __sem_target19
                          with
                            RuntimeDistElementary_DistUniform t824
                          then
                            unsafeCoerce
                              (uniformContinuousLogPdf
                                 (t824.a)
                                 (t824.b))
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
        RuntimeDistEmpirical_DistEmpirical t797
      then
        let x48 =
          uniformContinuousSample
            0.
            (last
               (t797.cumulativeWeights))
        in
        let cmp1 =
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
            cmp1
            (t797.cumulativeWeights)
        with
          Some idx1
        then
          unsafeCoerce
            (get
               (t797.samples)
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
        RuntimeDistEmpirical_DistEmpirical t798
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
        RuntimeDistEmpirical_DistEmpirical t799
      then
        (t799.samples, t799.logWeights)
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
        RuntimeDistEmpirical_DistEmpirical t800
      then
        match
          t800.extra
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
        RuntimeDistEmpirical_DistEmpirical t801
      then
        match
          t801.extra
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
        RuntimeDistEmpirical_DistEmpirical t802
      then
        t802.degenerate
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
    lam samples5.
      lam logWeights3.
        lam __sem_target16.
          match
            __sem_target16
          with
            extra2
          then
            let maxLogWeight1 =
              foldl
                (lam acc50.
                   lam lw6.
                     match
                       geqf
                         lw6
                         acc50
                     with
                       true
                     then
                       lw6
                     else
                       acc50)
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
                      (lam acc49.
                         lam lw5.
                           addf
                             acc49
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
              lam acc47.
                lam x49.
                  let acc48 =
                    addf
                      acc47
                      (exp
                         x49)
                  in
                  (acc48, acc48)
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
                    samples5,
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
        samples6
      then
        match
          unzip
            samples6
        with
          (logWeights5, samples7)
        in
        let extra3 =
            RuntimeDistEmpirical_EmpNorm
              { normConst =
                  0. }
          in
          #var"RuntimeDistEmpirical_constructDistEmpirical"
            samples7
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
        RuntimeDistCombined_DistCombinedIndependent t795
      then
        unsafeCoerce
          (map
             #var"RuntimeDistCombined_sample"
             (t795.combined))
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
        RuntimeDistCombined_DistCombinedIndependent t796
      then
        unsafeCoerce
          (map
             #var"RuntimeDistCombined_logObserve"
             (t796.combined))
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
        RuntimeDistCombined_DistCombinedIndependent t765
      then
        unsafeCoerce
          (map
             #var"RuntimeDist_sample"
             (t765.combined))
      else
        match
          __sem_target
        with
          RuntimeDistEmpirical_DistEmpirical t766
        then
          let x46 =
            uniformContinuousSample
              0.
              (last
                 (t766.cumulativeWeights))
          in
          let cmp =
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
              cmp
              (t766.cumulativeWeights)
          with
            Some idx
          then
            unsafeCoerce
              (get
                 (t766.samples)
                 idx)
          else
            error
              "Sampling from empirical distribution failed"
        else
          match
            __sem_target
          with
            RuntimeDistElementary_DistGamma t767
          then
            unsafeCoerce
              (gammaSample
                 (t767.shape)
                 (t767.scale))
          else
            match
              __sem_target
            with
              RuntimeDistElementary_DistExponential t768
            then
              unsafeCoerce
                (exponentialSample
                   (t768.rate))
            else
              match
                __sem_target
              with
                RuntimeDistElementary_DistPoisson t769
              then
                unsafeCoerce
                  (poissonSample
                     (t769.lambda))
              else
                match
                  __sem_target
                with
                  RuntimeDistElementary_DistBinomial t770
                then
                  unsafeCoerce
                    (binomialSample
                       (t770.p)
                       (t770.n))
                else
                  match
                    __sem_target
                  with
                    RuntimeDistElementary_DistBernoulli t771
                  then
                    unsafeCoerce
                      (bernoulliSample
                         (t771.p))
                  else
                    match
                      __sem_target
                    with
                      RuntimeDistElementary_DistBeta t772
                    then
                      unsafeCoerce
                        (betaSample
                           (t772.a)
                           (t772.b))
                    else
                      match
                        __sem_target
                      with
                        RuntimeDistElementary_DistGaussian t773
                      then
                        unsafeCoerce
                          (gaussianSample
                             (t773.mu)
                             (t773.sigma))
                      else
                        match
                          __sem_target
                        with
                          RuntimeDistElementary_DistMultinomial t774
                        then
                          unsafeCoerce
                            (multinomialSample
                               (t774.p)
                               (t774.n))
                        else
                          match
                            __sem_target
                          with
                            RuntimeDistElementary_DistCategorical t775
                          then
                            unsafeCoerce
                              (categoricalSample
                                 (t775.p))
                          else
                            match
                              __sem_target
                            with
                              RuntimeDistElementary_DistDirichlet t776
                            then
                              unsafeCoerce
                                (dirichletSample
                                   (t776.a))
                            else
                              match
                                __sem_target
                              with
                                RuntimeDistElementary_DistUniform t777
                              then
                                unsafeCoerce
                                  (uniformContinuousSample
                                     (t777.a)
                                     (t777.b))
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
        RuntimeDistCombined_DistCombinedIndependent t778
      then
        unsafeCoerce
          (map
             #var"RuntimeDist_logObserve"
             (t778.combined))
      else
        match
          __sem_target1
        with
          RuntimeDistEmpirical_DistEmpirical t779
        then
          error
            "Log observe not supported for empirical distribution"
        else
          match
            __sem_target1
          with
            RuntimeDistElementary_DistGamma t780
          then
            unsafeCoerce
              (gammaLogPdf
                 (t780.shape)
                 (t780.scale))
          else
            match
              __sem_target1
            with
              RuntimeDistElementary_DistExponential t781
            then
              unsafeCoerce
                (exponentialLogPdf
                   (t781.rate))
            else
              match
                __sem_target1
              with
                RuntimeDistElementary_DistPoisson t782
              then
                unsafeCoerce
                  (poissonLogPmf
                     (t782.lambda))
              else
                match
                  __sem_target1
                with
                  RuntimeDistElementary_DistBinomial t783
                then
                  unsafeCoerce
                    (binomialLogPmf
                       (t783.p)
                       (t783.n))
                else
                  match
                    __sem_target1
                  with
                    RuntimeDistElementary_DistBernoulli t784
                  then
                    unsafeCoerce
                      (bernoulliLogPmf
                         (t784.p))
                  else
                    match
                      __sem_target1
                    with
                      RuntimeDistElementary_DistBeta t785
                    then
                      unsafeCoerce
                        (betaLogPdf
                           (t785.a)
                           (t785.b))
                    else
                      match
                        __sem_target1
                      with
                        RuntimeDistElementary_DistGaussian t786
                      then
                        unsafeCoerce
                          (gaussianLogPdf
                             (t786.mu)
                             (t786.sigma))
                      else
                        match
                          __sem_target1
                        with
                          RuntimeDistElementary_DistMultinomial t787
                        then
                          unsafeCoerce
                            (lam o.
                               match
                                 eqi
                                   (t787.n)
                                   (foldl1
                                      addi
                                      o)
                               with
                                 true
                               then
                                 multinomialLogPmf
                                   (t787.p)
                                   o
                               else
                                 negf
                                   inf)
                        else
                          match
                            __sem_target1
                          with
                            RuntimeDistElementary_DistCategorical t788
                          then
                            unsafeCoerce
                              (categoricalLogPmf
                                 (t788.p))
                          else
                            match
                              __sem_target1
                            with
                              RuntimeDistElementary_DistDirichlet t789
                            then
                              unsafeCoerce
                                (dirichletLogPdf
                                   (t789.a))
                            else
                              match
                                __sem_target1
                              with
                                RuntimeDistElementary_DistUniform t790
                              then
                                unsafeCoerce
                                  (uniformContinuousLogPdf
                                     (t790.a)
                                     (t790.b))
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
        RuntimeDistEmpirical_DistEmpirical t791
      then
        (t791.samples, t791.logWeights)
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
        RuntimeDistEmpirical_DistEmpirical t792
      then
        match
          t792.extra
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
        RuntimeDistEmpirical_DistEmpirical t793
      then
        match
          t793.extra
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
        RuntimeDistEmpirical_DistEmpirical t794
      then
        t794.degenerate
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
    lam samples2.
      lam logWeights.
        lam __sem_target6.
          match
            __sem_target6
          with
            extra
          then
            let maxLogWeight =
              foldl
                (lam acc46.
                   lam lw3.
                     match
                       geqf
                         lw3
                         acc46
                     with
                       true
                     then
                       lw3
                     else
                       acc46)
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
                      (lam acc45.
                         lam lw2.
                           addf
                             acc45
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
              lam acc43.
                lam x47.
                  let acc44 =
                    addf
                      acc43
                      (exp
                         x47)
                  in
                  (acc44, acc44)
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
                    samples2,
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
        samples3
      then
        match
          unzip
            samples3
        with
          (logWeights2, samples4)
        in
        let extra1 =
            RuntimeDistEmpirical_EmpNorm
              { normConst =
                  0. }
          in
          #var"RuntimeDist_constructDistEmpirical"
            samples4
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
    lam model1.
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
              lam #var"52".
                let #var"53" =
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
            let #var"51" =
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
                (lam p12.
                   match
                     p12.checkpoint
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
                   (lam p13.
                      (p13.weight, match
                        p13.checkpoint
                      with
                        End a44
                      in
                      a44))
                   particles1)
            else
              let maxWeight =
                foldl
                  (lam acc42.
                     lam p17.
                       match
                         geqf
                           (p17.weight)
                           acc42
                       with
                         true
                       then
                         p17.weight
                       else
                         acc42)
                  (negf
                     inf)
                  particles1
              in
              let expWeights =
                reverse
                  (mapReverse
                     (lam p16.
                        exp
                          (subf
                             (p16.weight)
                             maxWeight))
                     particles1)
              in
              let sums =
                foldl
                  (lam acc41.
                     lam w3.
                       (addf
                         (acc41.0)
                         w3, addf
                         (acc41.1)
                         (mulf
                            w3
                            w3)))
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
                    (lam p14.
                       propagate
                         p14
                         (p14.weight))
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
                    (lam p15.
                       propagate
                         p15
                         contWeight)
                    resampled
                in
                runRec
                  particles3
      in
      match
        model1
          state3
      with
        (d9, cont1)
      in
      let particles: [Stop a42] =
          match
            d9
          with
            Some d10
          then
            match
              d10
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
                foldl21
                  (lam acc40.
                     lam s45.
                       lam lw.
                         cons
                           (start
                              cont1
                              lw
                              (lam #var"47".
                                 s45)
                              0)
                           acc40)
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
                     (lam #var"48".
                        #var"RuntimeDist_sample"
                          d10))
            else
              createList
                particleCount
                (start
                   cont1
                   0.
                   (lam #var"49".
                      #var"RuntimeDist_sample"
                        d10))
          else
            createList
              particleCount
              (start
                 cont1
                 0.
                 (lam #var"50".
                    unsafeCoerce
                      {}))
        in
        match
          runRec
            particles
        with
          (weights, samples1)
        in
        #var"RuntimeDist_constructDistEmpirical"
            samples1
            weights
            (RuntimeDistEmpirical_EmpNorm
               { normConst =
                   normConstant
                     weights })
in
recursive
  let work2 =
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
              work2
                eq
                t1100
                t2100
            else
              false
          else
            true
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
    lam s44.
      lam acc39.
        match
          null
            s44
        with
          true
        then
          acc39
        else
          let fsd =
            subi
              (char2int
                 (head
                    s44))
              (char2int
                 '0')
          in
          string2int_rechelper
            (tail
               s44)
            (addi
               (muli
                  10
                  acc39)
               fsd)
in
recursive
  let work =
    lam delim.
      lam s43.
        lam n1.
          lam m15.
            lam acc38.
              lam lastMatch.
                lam i7.
                  match
                    lti
                      (subi
                         n1
                         m15)
                      i7
                  with
                    true
                  then
                    snoc
                      acc38
                      (subsequence
                         s43
                         lastMatch
                         n1)
                  else
                    match
                      eqStringSlice
                        delim
                        s43
                        i7
                        m15
                    with
                      true
                    then
                      let nexti =
                        addi
                          i7
                          m15
                      in
                      work
                        delim
                        s43
                        n1
                        m15
                        (snoc
                           acc38
                           (subsequence
                              s43
                              lastMatch
                              (subi
                                 i7
                                 lastMatch)))
                        nexti
                        nexti
                    else
                      work
                        delim
                        s43
                        n1
                        m15
                        acc38
                        lastMatch
                        (addi
                           i7
                           1)
in
recursive
  let strTrim_init =
    lam s42.
      match
        eqString
          s42
          ""
      with
        true
      then
        s42
      else
        match
          isWhitespace
            (head
               s42)
        with
          true
        then
          strTrim_init
            (tail
               s42)
        else
          s42
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
external getProcessCpuTime : () -> Timespec
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
external externalReadDistFloatPipe : Int -> [(Timespec, [(Float, Float)])]
in
external externalReadDistFloatRecordPipe : Int -> Int -> [(Timespec, [(Float, Opaque)])]
in
external externalWriteDistFloatRecordPipe : Int -> Int -> (Timespec, ([Opaque], [Float])) -> ()
in
external externalBatchedInference : (() -> Opaque) -> Timespec -> [Opaque]
in
let nanosPerSec =
  1000000000
in
let nanosToTimespec =
  lam nanosPerSec20.
    lam nanos.
      let s24 =
        divi
          nanos
          nanosPerSec20
      in
      let ns2 =
        modi
          nanos
          nanosPerSec20
      in
      (s24, ns2)
in
let timespecToNanos =
  lam nanosPerSec13.
    lam ts.
      match
        ts
      with
        (s12, ns)
      in
      addi
          (muli
             s12
             nanosPerSec13)
          ns
in
let addTimespec =
  lam nanosPerSec19.
    lam lhs1.
      lam rhs1.
        match
          (lhs1, rhs1)
        with
          ((ls1, lns1), (rs1, rns1))
        in
        let s27 =
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
              nanosPerSec19
          with
            true
          then
            (addi
              s27
              1, subi
              ns3
              nanosPerSec19)
          else
            (s27, ns3)
in
let diffTimespec =
  lam nanosPerSec14.
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
              nanosPerSec14)
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
  lam nanosPerSec27.
    lam monoLogicalTime6.
      lam wallLogicalTime23.
        lam delay1.
          let oldPriority =
            setMaxPriority
              {}
          in
          let intervalTime =
            nanosToTimespec
              nanosPerSec27
              delay1
          in
          let endTime =
            getMonotonicTime
              {}
          in
          let elapsedTime =
            diffTimespec
              nanosPerSec27
              endTime
              (deref
                 monoLogicalTime6)
          in
          let waitTime =
            addTimespec
              nanosPerSec27
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
              let #var"46" =
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
                    nanosPerSec27
                    endTime
                    waitTime
                in
                timespecToNanos
                  nanosPerSec27
                  elapsedTime1
              else
                0
          in
          let #var"43" =
            modref
              monoLogicalTime6
              waitTime
          in
          let #var"44" =
            modref
              wallLogicalTime23
              (addTimespec
                 nanosPerSec27
                 (deref
                    wallLogicalTime23)
                 intervalTime)
          in
          let #var"45" =
            setPriority
              oldPriority
          in
          overrun1
in
type TSV a21 =
  (Timespec, a21)
in
let timestamp =
  lam nanosPerSec12.
    lam wallLogicalTime12.
      lam tsv2.
        let lt =
          deref
            wallLogicalTime12
        in
        timespecToNanos
          nanosPerSec12
          (diffTimespec
             nanosPerSec12
             (tsv2.0)
             lt)
in
let value: TSV Unknown -> Unknown =
  lam tsv3.
    tsv3.1
in
let tsv =
  lam nanosPerSec18.
    lam wallLogicalTime16.
      lam offset.
        lam value1.
          let lt1 =
            deref
              wallLogicalTime16
          in
          (addTimespec
            nanosPerSec18
            lt1
            (nanosToTimespec
               nanosPerSec18
               offset), value1)
in
let sdelay =
  lam nanosPerSec26.
    lam monoLogicalTime5.
      lam wallLogicalTime22.
        lam flushOutputs1.
          lam updateInputs1.
            lam delay.
              let #var"41" =
                flushOutputs1
                  {}
              in
              let overrun =
                delayBy
                  nanosPerSec26
                  monoLogicalTime5
                  wallLogicalTime22
                  delay
              in
              let #var"42" =
                updateInputs1
                  {}
              in
              overrun
in
let t =
  lam s41.
    lam w2.
      lam i6.
        (get
          w2
          i6, get
          s41
          i6)
in
let model =
  lam inferModel2.
    lam distToSamples2.
      lam distNormConst2.
        lam #var"40".
          let d8 =
            inferModel2
              {}
          in
          match
            distToSamples2
              d8
          with
            (s40, w)
          in
          let nc =
              distNormConst2
                d8
            in
            let w1 =
              map
                (addf
                   nc)
                w
            in
            let n =
              length
                s40
            in
            create
              n
              (t
                 s40
                 w1)
in
let rtpplInferRunner =
  lam nanosPerSec25.
    lam inferModel1.
      lam distToSamples1.
        lam samplesToDist1.
          lam distNormConst1.
            lam deadline.
              let t0 =
                getProcessCpuTime
                  {}
              in
              let deadlineTs =
                addTimespec
                  nanosPerSec25
                  t0
                  (nanosToTimespec
                     nanosPerSec25
                     deadline)
              in
              let samples =
                externalBatchedInference
                  (unsafeCoerce
                     (model
                        inferModel1
                        distToSamples1
                        distNormConst1))
                  deadlineTs
              in
              samplesToDist1
                (join
                   (unsafeCoerce
                      samples))
in
let openFileDescriptor: [Char] -> Int =
  lam file.
    externalOpenFileNonblocking
      file
in
let closeFileDescriptor: Int -> () =
  lam fd5.
    externalCloseFileDescriptor
      fd5
in
let rtpplReadFloatPort =
  lam fd4.
    externalReadFloatPipe
      fd4
in
let rtpplReadDistFloatPort =
  lam fd3.
    externalReadDistFloatPipe
      fd3
in
let rtpplReadDistFloatRecordPort =
  lam fd2.
    lam nfields2.
      externalReadDistFloatRecordPipe
        fd2
        nfields2
in
let t1 =
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
          (t1
             fd
             nfields)
          msgs
in
let t2 =
  lam closeFileDescriptors2.
    lam #var"38".
      let #var"39" =
        closeFileDescriptors2
          {}
      in
      exit
        0
in
let rtpplRuntimeInit =
  lam monoLogicalTime4.
    lam wallLogicalTime21.
      lam updateInputSequences.
        lam closeFileDescriptors1.
          lam cont.
            let #var"33" =
              setSigintHandler
                (t2
                   closeFileDescriptors1)
            in
            let #var"34" =
              modref
                monoLogicalTime4
                (getMonotonicTime
                   {})
            in
            let #var"35" =
              modref
                wallLogicalTime21
                (getWallClockTime
                   {})
            in
            let #var"36" =
              updateInputSequences
                {}
            in
            let #var"37" =
              cont
                {}
            in
            {}
in
let fileDescriptors =
  { speed =
      openFileDescriptor
        "pos-speed",
    posEst =
      openFileDescriptor
        "pos-posEst",
    frontLeft =
      openFileDescriptor
        "pos-frontLeft",
    frontRight =
      openFileDescriptor
        "pos-frontRight",
    rearLeft =
      openFileDescriptor
        "pos-rearLeft",
    rearRight =
      openFileDescriptor
        "pos-rearRight",
    left =
      openFileDescriptor
        "pos-left",
    right =
      openFileDescriptor
        "pos-right",
    steeringAngle =
      openFileDescriptor
        "pos-steeringAngle" }
in
let closeFileDescriptors =
  lam fileDescriptors6.
    lam #var"32".
      let close_frontLeft =
        closeFileDescriptor
          (fileDescriptors6.frontLeft)
      in
      let close_frontRight =
        closeFileDescriptor
          (fileDescriptors6.frontRight)
      in
      let close_rearLeft =
        closeFileDescriptor
          (fileDescriptors6.rearLeft)
      in
      let close_rearRight =
        closeFileDescriptor
          (fileDescriptors6.rearRight)
      in
      let close_left =
        closeFileDescriptor
          (fileDescriptors6.left)
      in
      let close_right =
        closeFileDescriptor
          (fileDescriptors6.right)
      in
      let close_speed =
        closeFileDescriptor
          (fileDescriptors6.speed)
      in
      let close_steeringAngle =
        closeFileDescriptor
          (fileDescriptors6.steeringAngle)
      in
      let close_posEst =
        closeFileDescriptor
          (fileDescriptors6.posEst)
      in
      {}
in
let inputSeqs =
  ref
    { speed =
        "",
      frontLeft =
        "",
      frontRight =
        "",
      rearLeft =
        "",
      rearRight =
        "",
      left =
        "",
      right =
        "",
      steeringAngle =
        "" }
in
let outputSeqs =
  ref
    { posEst =
        "" }
in
let t3 =
  lam tsv13.
    match
      tsv13
    with
      (ts11, v8)
    in
    (ts11, #var"RuntimeDistEmpirical_constructDistEmpiricalHelper"
        v8)
in
let t4 =
  lam tsv12.
    match
      tsv12
    with
      (ts10, v7)
    in
    (ts10, #var"RuntimeDistEmpirical_constructDistEmpiricalHelper"
        v7)
in
let t5 =
  lam tsv11.
    match
      tsv11
    with
      (ts9, v6)
    in
    (ts9, #var"RuntimeDistEmpirical_constructDistEmpiricalHelper"
        v6)
in
let t6 =
  lam tsv10.
    match
      tsv10
    with
      (ts8, v5)
    in
    (ts8, #var"RuntimeDistEmpirical_constructDistEmpiricalHelper"
        v5)
in
let t7 =
  lam tsv9.
    match
      tsv9
    with
      (ts7, v4)
    in
    (ts7, #var"RuntimeDistEmpirical_constructDistEmpiricalHelper"
        v4)
in
let t8 =
  lam tsv8.
    match
      tsv8
    with
      (ts6, v3)
    in
    (ts6, #var"RuntimeDistEmpirical_constructDistEmpiricalHelper"
        v3)
in
let t9 =
  lam tsv7.
    match
      tsv7
    with
      (ts5, v2)
    in
    (ts5, #var"RuntimeDistEmpirical_constructDistEmpiricalHelper"
        v2)
in
let updateInputs =
  lam fileDescriptors5.
    lam inputSeqs4.
      lam #var"31".
        modref
          inputSeqs4
          { speed =
              map
                t3
                (rtpplReadDistFloatRecordPort
                   (fileDescriptors5.speed)
                   2),
            frontLeft =
              map
                t4
                (rtpplReadDistFloatPort
                   (fileDescriptors5.frontLeft)),
            frontRight =
              map
                t5
                (rtpplReadDistFloatPort
                   (fileDescriptors5.frontRight)),
            rearLeft =
              map
                t6
                (rtpplReadDistFloatPort
                   (fileDescriptors5.rearLeft)),
            rearRight =
              map
                t7
                (rtpplReadDistFloatPort
                   (fileDescriptors5.rearRight)),
            left =
              map
                t8
                (rtpplReadDistFloatPort
                   (fileDescriptors5.left)),
            right =
              map
                t9
                (rtpplReadDistFloatPort
                   (fileDescriptors5.right)),
            steeringAngle =
              rtpplReadFloatPort
                (fileDescriptors5.steeringAngle) }
in
let t10 =
  lam tsv6.
    match
      tsv6
    with
      (ts4, v1)
    in
    (ts4, unsafeCoerce
        (distEmpiricalSamples
           v1))
in
let flushOutputs =
  lam fileDescriptors4.
    lam outputSeqs4.
      lam #var"29".
        let w_posEst =
          rtpplWriteDistFloatRecordPort
            (fileDescriptors4.posEst)
            3
            (map
               t10
               ((deref
                   outputSeqs4).posEst))
        in
        let #var"30" =
          modref
            outputSeqs4
            { posEst =
                "" }
        in
        {}
in
let subInt =
  subi
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
  lam s39.
    lam elem.
      snoc
        s39
        elem
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
let t11 =
  lam rows1.
    lam r.
      map
        convChar
        (get
           rows1
           r)
in
let readRoomMapRuntimeHelper =
  lam #var"28".
    let filename =
      get
        argv
        1
    in
    let s38 =
      strTrim
        (readFile
           filename)
    in
    match
      strSplit
        "\n"
        s38
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
          (t11
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
let usMaxRange =
  4.
in
let tofMaxRange =
  2.
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
  lam #var"27".
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
  lam floorToInt12.
    lam roomBlockWidth12.
      lam p7.
        { row =
            floorToInt12
              (divf
                 (p7.y)
                 roomBlockWidth12),
          col =
            floorToInt12
              (divf
                 (p7.x)
                 roomBlockWidth12) }
in
let positionAtOffset: Pos -> Offset -> Pos =
  lam p8.
    lam ofs2.
      let p9 =
        { p8
          with
          x =
            addf
              (p8.x)
              (mulf
                 (ofs2.distance)
                 (cos
                    (addf
                       (p8.direction)
                       (ofs2.angle)))) }
      in
      let p10 =
        { p9
          with
          y =
            addf
              (p9.y)
              (mulf
                 (ofs2.distance)
                 (sin
                    (addf
                       (p9.direction)
                       (ofs2.angle)))) }
      in
      let p11 =
        { p10
          with
          direction =
            addf
              (p10.direction)
              (ofs2.direction) }
      in
      p11
in
let withinRoomBounds =
  lam ltInt11.
    lam geqInt11.
      lam floorToInt11.
        lam roomBlockWidth11.
          lam m11.
            lam p6.
              let c =
                posToCoordinate
                  floorToInt11
                  roomBlockWidth11
                  p6
              in
              let res =
                true
              in
              let res1 =
                match
                  match
                    match
                      match
                        ltInt11
                          (c.row)
                          0
                      with
                        true
                      then
                        true
                      else
                        geqInt11
                          (c.row)
                          (m11.rows)
                    with
                      true
                    then
                      true
                    else
                      ltInt11
                        (c.col)
                        0
                  with
                    true
                  then
                    true
                  else
                    geqInt11
                      (c.col)
                      (m11.cols)
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
                            (m11.data)
                            (c.row))
                         (c.col))
                  in
                  res3
              in
              res1
in
let t12 =
  lam ltInt10.
    lam geqInt10.
      lam floorToInt10.
        lam roomBlockWidth10.
          lam m10.
            lam center1.
              lam acc20.
                lam ofs.
                  let acc21 =
                    match
                      acc20
                    with
                      true
                    then
                      let p =
                        positionAtOffset
                          center1
                          ofs
                      in
                      let acc22 =
                        withinRoomBounds
                          ltInt10
                          geqInt10
                          floorToInt10
                          roomBlockWidth10
                          m10
                          p
                      in
                      acc22
                    else
                      acc20
                  in
                  acc21
in
let carWithinRoomBounds =
  lam ltInt9.
    lam geqInt9.
      lam floorToInt9.
        lam roomBlockWidth9.
          lam frontLeftOffset8.
            lam frontRightOffset8.
              lam rearLeftOffset8.
                lam rearRightOffset8.
                  lam sideLeftOffset8.
                    lam sideRightOffset8.
                      lam m9.
                        lam center.
                          let sensorOffsets =
                            [ frontLeftOffset8,
                              frontRightOffset8,
                              rearLeftOffset8,
                              rearRightOffset8,
                              sideLeftOffset8,
                              sideRightOffset8 ]
                          in
                          let acc18 =
                            true
                          in
                          let acc19 =
                            foldl
                              (t12
                                 ltInt9
                                 geqInt9
                                 floorToInt9
                                 roomBlockWidth9
                                 m9
                                 center)
                              acc18
                              sensorOffsets
                          in
                          acc19
in
let t13 =
  lam m14.
    lam row1.
      lam blocks8.
        lam col.
          let blocks9 =
            match
              not
                (get
                   (get
                      (m14.data)
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
let t14 =
  lam m13.
    lam blocks6.
      lam row.
        let blocks7 =
          foldl
            (t13
               m13
               row)
            blocks6
            (range1
               0
               (m13.cols))
        in
        blocks7
in
let findFreeRoomCoordinates: RoomMap -> [Coordinate] =
  lam m12.
    let blocks4 =
      ""
    in
    let blocks5 =
      foldl
        (t14
           m12)
        blocks4
        (range1
           0
           (m12.rows))
    in
    blocks5
in
let timestampToSeconds =
  lam intToFloat10.
    lam ts1.
      divf
        (intToFloat10
           ts1)
        (intToFloat10
           1000000000)
in
let t15 =
  lam nanosPerSec11.
    lam wallLogicalTime11.
      lam intToFloat9.
        lam wheelCircumference6.
          lam m5.
            lam b1.
              lam sigma1.
                lam #var"13".
                  lam tsv1.
                    let ts2 =
                      timestampToSeconds
                        intToFloat9
                        (timestamp
                           nanosPerSec11
                           wallLogicalTime11
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
                    let #var"14": () =
                      error
                        "Cannot use observe outside of inferred model"
                    in
                    {}
in
let speedModel =
  lam nanosPerSec10.
    lam wallLogicalTime10.
      lam intToFloat8.
        lam maxSpeed5.
          lam wheelCircumference5.
            lam speedObs.
              let m4 =
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
              let #var"12" =
                foldl
                  (t15
                     nanosPerSec10
                     wallLogicalTime10
                     intToFloat8
                     wheelCircumference5
                     m4
                     b
                     sigma)
                  {}
                  speedObs
              in
              { slope =
                  m4,
                intercept =
                  b }
in
let t16 =
  lam nanosPerSec24.
    lam wallLogicalTime20.
      lam ltInt13.
        lam acc35.
          lam tsv5.
            let acc36 =
              match
                ltInt13
                  (timestamp
                     nanosPerSec24
                     wallLogicalTime20
                     acc35)
                  (timestamp
                     nanosPerSec24
                     wallLogicalTime20
                     tsv5)
              with
                true
              then
                let acc37 =
                  tsv5
                in
                acc37
              else
                acc35
            in
            acc36
in
let maxDistLineTimestamp =
  lam nanosPerSec23.
    lam wallLogicalTime19.
      lam ltInt12.
        lam acc33.
          lam tsvs.
            let acc34 =
              foldl
                (t16
                   nanosPerSec23
                   wallLogicalTime19
                   ltInt12)
                acc33
                tsvs
            in
            acc34
in
let initialPositionModel =
  lam pi8.
    lam blocks3.
      let d4 =
        error
          "Cannot use assume outside of inferred model"
      in
      { x =
          0.8,
        y =
          5.55,
        direction =
          d4 }
in
recursive
  let loopFn1 =
    lam inBounds3.
      lam eps1.
        lam acc25.
          match
            inBounds3
              (acc25.p)
          with
            true
          then
            let p3 =
              acc25.p
            in
            let p4 =
              { p3
                with
                x =
                  addf
                    (p3.x)
                    (mulf
                       eps1
                       (cos
                          (p3.direction))) }
            in
            let p5 =
              { p4
                with
                y =
                  addf
                    (p4.y)
                    (mulf
                       eps1
                       (sin
                          (p4.direction))) }
            in
            let acc26 =
              { acc25
                with
                d =
                  addf
                    (acc25.d)
                    eps1 }
            in
            let acc27 =
              { acc26
                with
                p =
                  p5 }
            in
            loopFn1
              inBounds3
              eps1
              acc27
          else
            acc25
in
let estimateDistance: Offset -> Pos -> (Pos -> Bool) -> Float =
  lam ofs1.
    lam p1.
      lam inBounds2.
        let eps =
          0.01
        in
        let p2 =
          positionAtOffset
            p1
            ofs1
        in
        let acc23 =
          { d =
              0.,
            p =
              p2 }
        in
        let acc24 =
          loopFn1
            inBounds2
            eps
            acc23
        in
        acc24.d
in
let areaUnderLine =
  lam subInt11.
    lam intToFloat16.
      lam f15.
        lam a34.
          lam b2.
            let x15 =
              f15
                a34
            in
            let y =
              f15
                b2
            in
            let delta =
              timestampToSeconds
                intToFloat16
                (subInt11
                   b2
                   a34)
            in
            divf
              (addf
                 (mulf
                    x15
                    delta)
                 (mulf
                    y
                    delta))
              2.
in
let f =
  lam nanosPerSec17.
    lam wallLogicalTime15.
      lam subInt8.
        lam intToFloat13.
          lam line.
            lam t431.
              let lineTs =
                timestamp
                  nanosPerSec17
                  wallLogicalTime15
                  line
              in
              let ts3 =
                timestampToSeconds
                  intToFloat13
                  (subInt8
                     t431
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
let t17 =
  lam nanosPerSec22.
    lam wallLogicalTime18.
      lam subInt10.
        lam intToFloat15.
          lam prior2.
            lam speedFn2.
              lam d7.
                lam bx2.
                  lam by2.
                    lam sigma4.
                      lam s26.
                        lam inBounds1.
                          lam #var"23".
                            lam tsv4.
                              let distTravelled =
                                areaUnderLine
                                  subInt10
                                  intToFloat15
                                  speedFn2
                                  (timestamp
                                     nanosPerSec22
                                     wallLogicalTime18
                                     prior2)
                                  (timestamp
                                     nanosPerSec22
                                     wallLogicalTime18
                                     tsv4)
                              in
                              let xt =
                                addf
                                  bx2
                                  (mulf
                                     distTravelled
                                     (cos
                                        d7))
                              in
                              let yt =
                                addf
                                  by2
                                  (mulf
                                     distTravelled
                                     (sin
                                        d7))
                              in
                              let pos1 =
                                { x =
                                    xt,
                                  y =
                                    yt,
                                  direction =
                                    d7 }
                              in
                              let expectedDist =
                                estimateDistance
                                  (s26.ofs)
                                  pos1
                                  inBounds1
                              in
                              let expectedDist1 =
                                match
                                  gtf
                                    expectedDist
                                    (s26.maxRange)
                                with
                                  true
                                then
                                  let expectedDist2 =
                                    s26.maxRange
                                  in
                                  expectedDist2
                                else
                                  expectedDist
                              in
                              let estDist =
                                error
                                  "Cannot use assume outside of inferred model"
                              in
                              let #var"24": () =
                                error
                                  "Cannot use observe outside of inferred model"
                              in
                              {}
in
let t18 =
  lam nanosPerSec21.
    lam wallLogicalTime17.
      lam subInt9.
        lam ltInt8.
          lam geqInt8.
            lam floorToInt8.
              lam intToFloat14.
                lam roomBlockWidth8.
                  lam m8.
                    lam prior1.
                      lam speedFn1.
                        lam d6.
                          lam bx1.
                            lam by1.
                              lam sigma3.
                                lam #var"21".
                                  lam s25.
                                    let inBounds =
                                      withinRoomBounds
                                        ltInt8
                                        geqInt8
                                        floorToInt8
                                        roomBlockWidth8
                                        m8
                                    in
                                    let #var"22" =
                                      foldl
                                        (t17
                                           nanosPerSec21
                                           wallLogicalTime17
                                           subInt9
                                           intToFloat14
                                           prior1
                                           speedFn1
                                           d6
                                           bx1
                                           by1
                                           sigma3
                                           s25
                                           inBounds)
                                        {}
                                        (s25.obs)
                                    in
                                    {}
in
let positionModel =
  lam pi10.
    lam nanosPerSec16.
      lam wallLogicalTime14.
        lam subInt7.
          lam ltInt7.
            lam geqInt7.
              lam floorToInt7.
                lam intToFloat12.
                  lam roomBlockWidth7.
                    lam usMaxRange7.
                      lam tofMaxRange7.
                        lam frontLeftOffset7.
                          lam frontRightOffset7.
                            lam rearLeftOffset7.
                              lam rearRightOffset7.
                                lam sideLeftOffset7.
                                  lam sideRightOffset7.
                                    lam m7.
                                      lam prior.
                                        lam flEst5.
                                          lam frEst4.
                                            lam rlEst4.
                                              lam rrEst4.
                                                lam lEst5.
                                                  lam rEst4.
                                                    lam speedEst1.
                                                      lam saObs4.
                                                        let posPrev =
                                                          error
                                                            "Cannot use assume outside of inferred model"
                                                        in
                                                        let speedLine =
                                                          error
                                                            "Cannot use assume outside of inferred model"
                                                        in
                                                        let speedFn =
                                                          f
                                                            nanosPerSec16
                                                            wallLogicalTime14
                                                            subInt7
                                                            intToFloat12
                                                            (tsv
                                                               nanosPerSec16
                                                               wallLogicalTime14
                                                               (timestamp
                                                                  nanosPerSec16
                                                                  wallLogicalTime14
                                                                  speedEst1)
                                                               speedLine)
                                                        in
                                                        let v =
                                                          speedFn
                                                            (timestamp
                                                               nanosPerSec16
                                                               wallLogicalTime14
                                                               prior)
                                                        in
                                                        let d5 =
                                                          error
                                                            "Cannot use assume outside of inferred model"
                                                        in
                                                        let bx =
                                                          error
                                                            "Cannot use assume outside of inferred model"
                                                        in
                                                        let by =
                                                          error
                                                            "Cannot use assume outside of inferred model"
                                                        in
                                                        let sigma2 =
                                                          error
                                                            "Cannot use assume outside of inferred model"
                                                        in
                                                        let sensorData =
                                                          [ { ofs =
                                                                frontLeftOffset7,
                                                              obs =
                                                                flEst5,
                                                              maxRange =
                                                                usMaxRange7 },
                                                            { ofs =
                                                                frontRightOffset7,
                                                              obs =
                                                                frEst4,
                                                              maxRange =
                                                                usMaxRange7 },
                                                            { ofs =
                                                                rearLeftOffset7,
                                                              obs =
                                                                rlEst4,
                                                              maxRange =
                                                                usMaxRange7 },
                                                            { ofs =
                                                                rearRightOffset7,
                                                              obs =
                                                                rrEst4,
                                                              maxRange =
                                                                usMaxRange7 },
                                                            { ofs =
                                                                sideLeftOffset7,
                                                              obs =
                                                                lEst5,
                                                              maxRange =
                                                                tofMaxRange7 },
                                                            { ofs =
                                                                sideRightOffset7,
                                                              obs =
                                                                rEst4,
                                                              maxRange =
                                                                tofMaxRange7 } ]
                                                        in
                                                        let #var"18" =
                                                          foldl
                                                            (t18
                                                               nanosPerSec16
                                                               wallLogicalTime14
                                                               subInt7
                                                               ltInt7
                                                               geqInt7
                                                               floorToInt7
                                                               intToFloat12
                                                               roomBlockWidth7
                                                               m7
                                                               prior
                                                               speedFn
                                                               d5
                                                               bx
                                                               by
                                                               sigma2)
                                                            {}
                                                            sensorData
                                                        in
                                                        let dist =
                                                          areaUnderLine
                                                            subInt7
                                                            intToFloat12
                                                            speedFn
                                                            (timestamp
                                                               nanosPerSec16
                                                               wallLogicalTime14
                                                               prior)
                                                            0
                                                        in
                                                        let pos =
                                                          { x =
                                                              addf
                                                                bx
                                                                (mulf
                                                                   dist
                                                                   (cos
                                                                      d5)),
                                                            y =
                                                              addf
                                                                by
                                                                (mulf
                                                                   dist
                                                                   (sin
                                                                      d5)),
                                                            direction =
                                                              d5 }
                                                        in
                                                        let #var"19" =
                                                          match
                                                            not
                                                              (carWithinRoomBounds
                                                                 ltInt7
                                                                 geqInt7
                                                                 floorToInt7
                                                                 roomBlockWidth7
                                                                 frontLeftOffset7
                                                                 frontRightOffset7
                                                                 rearLeftOffset7
                                                                 rearRightOffset7
                                                                 sideLeftOffset7
                                                                 sideRightOffset7
                                                                 m7
                                                                 pos)
                                                          with
                                                            true
                                                          then
                                                            let #var"20" =
                                                              error
                                                                "Cannot use weight outside of inferred model"
                                                            in
                                                            {}
                                                          else
                                                            {}
                                                        in
                                                        pos
in
let t19 =
  lam nanosPerSec9.
    lam wallLogicalTime9.
      lam intToFloat7.
        lam maxSpeed4.
          lam wheelCircumference4.
            lam #var"11".
              speedModel
                nanosPerSec9
                wallLogicalTime9
                intToFloat7
                maxSpeed4
                wheelCircumference4
                ""
in
let t20 =
  lam pi7.
    lam blocks2.
      lam #var"7".
        initialPositionModel
          pi7
          blocks2
in
let t21 =
  lam pi9: Float.
    lam nanosPerSec15: Int.
      lam wallLogicalTime13: Ref (Timespec).
        lam subInt6: Int -> Int -> Int.
          lam ltInt6: Int -> Int -> Bool.
            lam geqInt6: Int -> Int -> Bool.
              lam floorToInt6: Float -> Int.
                lam intToFloat11: Int -> Float.
                  lam roomBlockWidth6: Float.
                    lam usMaxRange6: Float.
                      lam tofMaxRange6: Float.
                        lam frontLeftOffset6: {angle: Float, distance: Float, direction: Float}.
                          lam frontRightOffset6: {angle: Float, distance: Float, direction: Float}.
                            lam rearLeftOffset6: {angle: Float, distance: Float, direction: Float}.
                              lam rearRightOffset6: {angle: Float, distance: Float, direction: Float}.
                                lam sideLeftOffset6: {angle: Float, distance: Float, direction: Float}.
                                  lam sideRightOffset6: {angle: Float, distance: Float, direction: Float}.
                                    lam m6: RoomMap.
                                      lam frEst3: [((Int, Int), Dist Float)].
                                        lam rlEst3: [((Int, Int), Dist Float)].
                                          lam rrEst3: [((Int, Int), Dist Float)].
                                            lam rEst3: [((Int, Int), Dist Float)].
                                              lam saObs3: [((Int, Int), Float)].
                                                lam lEst4: [TSV (Dist Float)].
                                                  lam flEst4: [TSV (Dist Float)].
                                                    lam acc17: {d: TSV Unknown, speed: TSV Unknown}.
                                                      lam state2: State.
                                                        stopInit
                                                          (lam #var"17".
                                                             let map3 =
                                                               lam f22.
                                                                 let t751 =
                                                                   lam s36.
                                                                     recursive
                                                                       let rec14 =
                                                                         lam s37.
                                                                           let t753 =
                                                                             match
                                                                               s37
                                                                             with
                                                                               ""
                                                                             then
                                                                               let t754 =
                                                                                 ""
                                                                               in
                                                                               t754
                                                                             else
                                                                               let t755 =
                                                                                 match
                                                                                   s37
                                                                                 with
                                                                                   [ a40 ]
                                                                                 then
                                                                                   let t756 =
                                                                                     f22
                                                                                       a40
                                                                                   in
                                                                                   let t757 =
                                                                                     [ t756 ]
                                                                                   in
                                                                                   t757
                                                                                 else
                                                                                   let t758 =
                                                                                     match
                                                                                       s37
                                                                                     with
                                                                                       [ a41 ] ++ ss11 ++ ""
                                                                                     then
                                                                                       let t759 =
                                                                                         cons
                                                                                       in
                                                                                       let t760 =
                                                                                         f22
                                                                                           a41
                                                                                       in
                                                                                       let t761 =
                                                                                         t759
                                                                                           t760
                                                                                       in
                                                                                       let t762 =
                                                                                         rec14
                                                                                           ss11
                                                                                       in
                                                                                       let t763 =
                                                                                         t761
                                                                                           t762
                                                                                       in
                                                                                       t763
                                                                                     else
                                                                                       let t764 =
                                                                                         never
                                                                                       in
                                                                                       t764
                                                                                   in
                                                                                   t758
                                                                               in
                                                                               t755
                                                                           in
                                                                           t753
                                                                     in
                                                                     let t752 =
                                                                       rec14
                                                                         s36
                                                                     in
                                                                     t752
                                                                 in
                                                                 t751
                                                             in
                                                             let iter3 =
                                                               lam f21.
                                                                 let t748 =
                                                                   lam s35.
                                                                     let t749 =
                                                                       map3
                                                                         f21
                                                                     in
                                                                     let #var"26" =
                                                                       t749
                                                                         s35
                                                                     in
                                                                     let t750 =
                                                                       {}
                                                                     in
                                                                     t750
                                                                 in
                                                                 t748
                                                             in
                                                             let mapi2 =
                                                               lam f20.
                                                                 let t724 =
                                                                   lam s33.
                                                                     recursive
                                                                       let rec13 =
                                                                         lam i5.
                                                                           let t728 =
                                                                             lam s34.
                                                                               let t729 =
                                                                                 match
                                                                                   s34
                                                                                 with
                                                                                   ""
                                                                                 then
                                                                                   let t730 =
                                                                                     ""
                                                                                   in
                                                                                   t730
                                                                                 else
                                                                                   let t731 =
                                                                                     match
                                                                                       s34
                                                                                     with
                                                                                       [ a38 ]
                                                                                     then
                                                                                       let t732 =
                                                                                         f20
                                                                                           i5
                                                                                       in
                                                                                       let t733 =
                                                                                         t732
                                                                                           a38
                                                                                       in
                                                                                       let t734 =
                                                                                         [ t733 ]
                                                                                       in
                                                                                       t734
                                                                                     else
                                                                                       let t735 =
                                                                                         match
                                                                                           s34
                                                                                         with
                                                                                           [ a39 ] ++ ss10 ++ ""
                                                                                         then
                                                                                           let t736 =
                                                                                             cons
                                                                                           in
                                                                                           let t737 =
                                                                                             f20
                                                                                               i5
                                                                                           in
                                                                                           let t738 =
                                                                                             t737
                                                                                               a39
                                                                                           in
                                                                                           let t739 =
                                                                                             t736
                                                                                               t738
                                                                                           in
                                                                                           let t740 =
                                                                                             addi
                                                                                           in
                                                                                           let t741 =
                                                                                             t740
                                                                                               i5
                                                                                           in
                                                                                           let t742 =
                                                                                             1
                                                                                           in
                                                                                           let t743 =
                                                                                             t741
                                                                                               t742
                                                                                           in
                                                                                           let t744 =
                                                                                             rec13
                                                                                               t743
                                                                                           in
                                                                                           let t745 =
                                                                                             t744
                                                                                               ss10
                                                                                           in
                                                                                           let t746 =
                                                                                             t739
                                                                                               t745
                                                                                           in
                                                                                           t746
                                                                                         else
                                                                                           let t747 =
                                                                                             never
                                                                                           in
                                                                                           t747
                                                                                       in
                                                                                       t735
                                                                                   in
                                                                                   t731
                                                                               in
                                                                               t729
                                                                           in
                                                                           t728
                                                                     in
                                                                     let t725 =
                                                                       0
                                                                     in
                                                                     let t726 =
                                                                       rec13
                                                                         t725
                                                                     in
                                                                     let t727 =
                                                                       t726
                                                                         s33
                                                                     in
                                                                     t727
                                                                 in
                                                                 t724
                                                             in
                                                             let iteri2 =
                                                               lam f19.
                                                                 let t721 =
                                                                   lam s32.
                                                                     let t722 =
                                                                       mapi2
                                                                         f19
                                                                     in
                                                                     let #var"25" =
                                                                       t722
                                                                         s32
                                                                     in
                                                                     let t723 =
                                                                       {}
                                                                     in
                                                                     t723
                                                                 in
                                                                 t721
                                                             in
                                                             let foldl3 =
                                                               lam f18.
                                                                 let t709 =
                                                                   lam acc31.
                                                                     let t710 =
                                                                       lam s30.
                                                                         recursive
                                                                           let rec12 =
                                                                             lam acc32.
                                                                               let t713 =
                                                                                 lam s31.
                                                                                   let t714 =
                                                                                     match
                                                                                       s31
                                                                                     with
                                                                                       ""
                                                                                     then
                                                                                       acc32
                                                                                     else
                                                                                       let t715 =
                                                                                         match
                                                                                           s31
                                                                                         with
                                                                                           [ a37 ] ++ ss9 ++ ""
                                                                                         then
                                                                                           let t716 =
                                                                                             f18
                                                                                               acc32
                                                                                           in
                                                                                           let t717 =
                                                                                             t716
                                                                                               a37
                                                                                           in
                                                                                           let t718 =
                                                                                             rec12
                                                                                               t717
                                                                                           in
                                                                                           let t719 =
                                                                                             t718
                                                                                               ss9
                                                                                           in
                                                                                           t719
                                                                                         else
                                                                                           let t720 =
                                                                                             never
                                                                                           in
                                                                                           t720
                                                                                       in
                                                                                       t715
                                                                                   in
                                                                                   t714
                                                                               in
                                                                               t713
                                                                         in
                                                                         let t711 =
                                                                           rec12
                                                                             acc31
                                                                         in
                                                                         let t712 =
                                                                           t711
                                                                             s30
                                                                         in
                                                                         t712
                                                                     in
                                                                     t710
                                                                 in
                                                                 t709
                                                             in
                                                             let foldr3 =
                                                               lam f17.
                                                                 let t697 =
                                                                   lam acc29.
                                                                     let t698 =
                                                                       lam s28.
                                                                         recursive
                                                                           let rec11 =
                                                                             lam acc30.
                                                                               let t701 =
                                                                                 lam s29.
                                                                                   let t702 =
                                                                                     match
                                                                                       s29
                                                                                     with
                                                                                       ""
                                                                                     then
                                                                                       acc30
                                                                                     else
                                                                                       let t703 =
                                                                                         match
                                                                                           s29
                                                                                         with
                                                                                           [ a36 ] ++ ss8 ++ ""
                                                                                         then
                                                                                           let t704 =
                                                                                             f17
                                                                                               a36
                                                                                           in
                                                                                           let t705 =
                                                                                             rec11
                                                                                               acc30
                                                                                           in
                                                                                           let t706 =
                                                                                             t705
                                                                                               ss8
                                                                                           in
                                                                                           let t707 =
                                                                                             t704
                                                                                               t706
                                                                                           in
                                                                                           t707
                                                                                         else
                                                                                           let t708 =
                                                                                             never
                                                                                           in
                                                                                           t708
                                                                                       in
                                                                                       t703
                                                                                   in
                                                                                   t702
                                                                               in
                                                                               t701
                                                                         in
                                                                         let t699 =
                                                                           rec11
                                                                             acc29
                                                                         in
                                                                         let t700 =
                                                                           t699
                                                                             s28
                                                                         in
                                                                         t700
                                                                     in
                                                                     t698
                                                                 in
                                                                 t697
                                                             in
                                                             let create2 =
                                                               lam l3.
                                                                 let t673 =
                                                                   lam f16.
                                                                     recursive
                                                                       let rec10 =
                                                                         lam i4.
                                                                           let t681 =
                                                                             lam acc28.
                                                                               let t682 =
                                                                                 geqi
                                                                               in
                                                                               let t683 =
                                                                                 t682
                                                                                   i4
                                                                               in
                                                                               let t684 =
                                                                                 0
                                                                               in
                                                                               let t685 =
                                                                                 t683
                                                                                   t684
                                                                               in
                                                                               let t686 =
                                                                                 match
                                                                                   t685
                                                                                 with
                                                                                   true
                                                                                 then
                                                                                   let t687 =
                                                                                     subi
                                                                                   in
                                                                                   let t688 =
                                                                                     t687
                                                                                       i4
                                                                                   in
                                                                                   let t689 =
                                                                                     1
                                                                                   in
                                                                                   let t690 =
                                                                                     t688
                                                                                       t689
                                                                                   in
                                                                                   let t691 =
                                                                                     rec10
                                                                                       t690
                                                                                   in
                                                                                   let t692 =
                                                                                     cons
                                                                                   in
                                                                                   let t693 =
                                                                                     f16
                                                                                       i4
                                                                                   in
                                                                                   let t694 =
                                                                                     t692
                                                                                       t693
                                                                                   in
                                                                                   let t695 =
                                                                                     t694
                                                                                       acc28
                                                                                   in
                                                                                   let t696 =
                                                                                     t691
                                                                                       t695
                                                                                   in
                                                                                   t696
                                                                                 else
                                                                                   acc28
                                                                               in
                                                                               t686
                                                                           in
                                                                           t681
                                                                     in
                                                                     let t674 =
                                                                       subi
                                                                     in
                                                                     let t675 =
                                                                       t674
                                                                         l3
                                                                     in
                                                                     let t676 =
                                                                       1
                                                                     in
                                                                     let t677 =
                                                                       t675
                                                                         t676
                                                                     in
                                                                     let t678 =
                                                                       rec10
                                                                         t677
                                                                     in
                                                                     let t679 =
                                                                       ""
                                                                     in
                                                                     let t680 =
                                                                       t678
                                                                         t679
                                                                     in
                                                                     t680
                                                                 in
                                                                 t673
                                                             in
                                                             let t289 =
                                                               {}
                                                             in
                                                             let not =
                                                               lam a35.
                                                                 let t670 =
                                                                   match
                                                                     a35
                                                                   with
                                                                     true
                                                                   then
                                                                     let t671 =
                                                                       false
                                                                     in
                                                                     t671
                                                                   else
                                                                     let t672 =
                                                                       true
                                                                     in
                                                                     t672
                                                                 in
                                                                 t670
                                                             in
                                                             let externalSin =
                                                               lam a111.
                                                                 externalSin
                                                                   a111
                                                             in
                                                             let sin =
                                                               lam x45: Float.
                                                                 let t669 =
                                                                   externalSin
                                                                     x45
                                                                 in
                                                                 t669
                                                             in
                                                             let externalCos =
                                                               lam a110.
                                                                 externalCos
                                                                   a110
                                                             in
                                                             let cos =
                                                               lam x44: Float.
                                                                 let t668 =
                                                                   externalCos
                                                                     x44
                                                                 in
                                                                 t668
                                                             in
                                                             let timestamp : all a. Int -> Ref Timespec -> TSV a -> Int =
                                                               lam nanosPerSec12.
                                                                 let t635 =
                                                                   lam wallLogicalTime12.
                                                                     let t636 =
                                                                       lam tsv2.
                                                                         let t637 =
                                                                           deref
                                                                         in
                                                                         let lt =
                                                                           t637
                                                                             wallLogicalTime12
                                                                         in
                                                                         let nanosPerSec13 =
                                                                           nanosPerSec12
                                                                         in
                                                                         let nanosPerSec14 =
                                                                           nanosPerSec12
                                                                         in
                                                                         let lhs =
                                                                           match
                                                                             tsv2
                                                                           with
                                                                             (#var"X1",)
                                                                           then
                                                                             #var"X1"
                                                                           else
                                                                             let t667 =
                                                                               never
                                                                             in
                                                                             t667
                                                                         in
                                                                         let rhs =
                                                                           lt
                                                                         in
                                                                         let t638 =
                                                                           (lhs, rhs)
                                                                         in
                                                                         let t639 =
                                                                           match
                                                                             t638
                                                                           with
                                                                             ((ls, lns), (rs, rns))
                                                                           then
                                                                             let t648 =
                                                                               subi
                                                                             in
                                                                             let t649 =
                                                                               t648
                                                                                 ls
                                                                             in
                                                                             let s13 =
                                                                               t649
                                                                                 rs
                                                                             in
                                                                             let t650 =
                                                                               subi
                                                                             in
                                                                             let t651 =
                                                                               t650
                                                                                 lns
                                                                             in
                                                                             let ns1 =
                                                                               t651
                                                                                 rns
                                                                             in
                                                                             let t652 =
                                                                               lti
                                                                             in
                                                                             let t653 =
                                                                               t652
                                                                                 ns1
                                                                             in
                                                                             let t654 =
                                                                               0
                                                                             in
                                                                             let t655 =
                                                                               t653
                                                                                 t654
                                                                             in
                                                                             let t656 =
                                                                               match
                                                                                 t655
                                                                               with
                                                                                 true
                                                                               then
                                                                                 let t657 =
                                                                                   addi
                                                                                 in
                                                                                 let t658 =
                                                                                   t657
                                                                                     ns1
                                                                                 in
                                                                                 let t659 =
                                                                                   t658
                                                                                     nanosPerSec14
                                                                                 in
                                                                                 let t660 =
                                                                                   subi
                                                                                 in
                                                                                 let t661 =
                                                                                   t660
                                                                                     s13
                                                                                 in
                                                                                 let t662 =
                                                                                   1
                                                                                 in
                                                                                 let t663 =
                                                                                   t661
                                                                                     t662
                                                                                 in
                                                                                 let t664 =
                                                                                   (t663, t659)
                                                                                 in
                                                                                 t664
                                                                               else
                                                                                 let t665 =
                                                                                   (s13, ns1)
                                                                                 in
                                                                                 t665
                                                                             in
                                                                             t656
                                                                           else
                                                                             let t666 =
                                                                               never
                                                                             in
                                                                             t666
                                                                         in
                                                                         let ts =
                                                                           t639
                                                                         in
                                                                         let t640 =
                                                                           match
                                                                             ts
                                                                           with
                                                                             (s12, ns)
                                                                           then
                                                                             let t641 =
                                                                               addi
                                                                             in
                                                                             let t642 =
                                                                               muli
                                                                             in
                                                                             let t643 =
                                                                               t642
                                                                                 s12
                                                                             in
                                                                             let t644 =
                                                                               t643
                                                                                 nanosPerSec13
                                                                             in
                                                                             let t645 =
                                                                               t641
                                                                                 t644
                                                                             in
                                                                             let t646 =
                                                                               t645
                                                                                 ns
                                                                             in
                                                                             t646
                                                                           else
                                                                             let t647 =
                                                                               never
                                                                             in
                                                                             t647
                                                                         in
                                                                         t640
                                                                     in
                                                                     t636
                                                                 in
                                                                 t635
                                                             in
                                                             let value : all a. TSV a -> a =
                                                               lam tsv3.
                                                                 let t633 =
                                                                   match
                                                                     tsv3
                                                                   with
                                                                     {#label"1" = #var"X"}
                                                                   then
                                                                     #var"X"
                                                                   else
                                                                     let t634 =
                                                                       never
                                                                     in
                                                                     t634
                                                                 in
                                                                 t633
                                                             in
                                                             let positionAtOffset =
                                                               lam p8.
                                                                 let t589 =
                                                                   lam ofs2.
                                                                     let t590 =
                                                                       addf
                                                                     in
                                                                     let t591 =
                                                                       match
                                                                         p8
                                                                       with
                                                                         {x = x43}
                                                                       then
                                                                         x43
                                                                       else
                                                                         let t632 =
                                                                           never
                                                                         in
                                                                         t632
                                                                     in
                                                                     let t592 =
                                                                       t590
                                                                         t591
                                                                     in
                                                                     let t593 =
                                                                       mulf
                                                                     in
                                                                     let t594 =
                                                                       match
                                                                         ofs2
                                                                       with
                                                                         {distance = x42}
                                                                       then
                                                                         x42
                                                                       else
                                                                         let t631 =
                                                                           never
                                                                         in
                                                                         t631
                                                                     in
                                                                     let t595 =
                                                                       t593
                                                                         t594
                                                                     in
                                                                     let t596 =
                                                                       addf
                                                                     in
                                                                     let t597 =
                                                                       match
                                                                         p8
                                                                       with
                                                                         {direction = x41}
                                                                       then
                                                                         x41
                                                                       else
                                                                         let t630 =
                                                                           never
                                                                         in
                                                                         t630
                                                                     in
                                                                     let t598 =
                                                                       t596
                                                                         t597
                                                                     in
                                                                     let t599 =
                                                                       match
                                                                         ofs2
                                                                       with
                                                                         {angle = x40}
                                                                       then
                                                                         x40
                                                                       else
                                                                         let t629 =
                                                                           never
                                                                         in
                                                                         t629
                                                                     in
                                                                     let t600 =
                                                                       t598
                                                                         t599
                                                                     in
                                                                     let t601 =
                                                                       cos
                                                                         t600
                                                                     in
                                                                     let t602 =
                                                                       t595
                                                                         t601
                                                                     in
                                                                     let t603 =
                                                                       t592
                                                                         t602
                                                                     in
                                                                     let p9 =
                                                                       { p8
                                                                         with
                                                                         x =
                                                                           t603 }
                                                                     in
                                                                     let t604 =
                                                                       addf
                                                                     in
                                                                     let t605 =
                                                                       match
                                                                         p9
                                                                       with
                                                                         {y = x39}
                                                                       then
                                                                         x39
                                                                       else
                                                                         let t628 =
                                                                           never
                                                                         in
                                                                         t628
                                                                     in
                                                                     let t606 =
                                                                       t604
                                                                         t605
                                                                     in
                                                                     let t607 =
                                                                       mulf
                                                                     in
                                                                     let t608 =
                                                                       match
                                                                         ofs2
                                                                       with
                                                                         {distance = x38}
                                                                       then
                                                                         x38
                                                                       else
                                                                         let t627 =
                                                                           never
                                                                         in
                                                                         t627
                                                                     in
                                                                     let t609 =
                                                                       t607
                                                                         t608
                                                                     in
                                                                     let t610 =
                                                                       addf
                                                                     in
                                                                     let t611 =
                                                                       match
                                                                         p9
                                                                       with
                                                                         {direction = x37}
                                                                       then
                                                                         x37
                                                                       else
                                                                         let t626 =
                                                                           never
                                                                         in
                                                                         t626
                                                                     in
                                                                     let t612 =
                                                                       t610
                                                                         t611
                                                                     in
                                                                     let t613 =
                                                                       match
                                                                         ofs2
                                                                       with
                                                                         {angle = x36}
                                                                       then
                                                                         x36
                                                                       else
                                                                         let t625 =
                                                                           never
                                                                         in
                                                                         t625
                                                                     in
                                                                     let t614 =
                                                                       t612
                                                                         t613
                                                                     in
                                                                     let t615 =
                                                                       sin
                                                                         t614
                                                                     in
                                                                     let t616 =
                                                                       t609
                                                                         t615
                                                                     in
                                                                     let t617 =
                                                                       t606
                                                                         t616
                                                                     in
                                                                     let p10 =
                                                                       { p9
                                                                         with
                                                                         y =
                                                                           t617 }
                                                                     in
                                                                     let t618 =
                                                                       addf
                                                                     in
                                                                     let t619 =
                                                                       match
                                                                         p10
                                                                       with
                                                                         {direction = x35}
                                                                       then
                                                                         x35
                                                                       else
                                                                         let t624 =
                                                                           never
                                                                         in
                                                                         t624
                                                                     in
                                                                     let t620 =
                                                                       t618
                                                                         t619
                                                                     in
                                                                     let t621 =
                                                                       match
                                                                         ofs2
                                                                       with
                                                                         {direction = x34}
                                                                       then
                                                                         x34
                                                                       else
                                                                         let t623 =
                                                                           never
                                                                         in
                                                                         t623
                                                                     in
                                                                     let t622 =
                                                                       t620
                                                                         t621
                                                                     in
                                                                     let p11 =
                                                                       { p10
                                                                         with
                                                                         direction =
                                                                           t622 }
                                                                     in
                                                                     p11
                                                                 in
                                                                 t589
                                                             in
                                                             let withinRoomBounds =
                                                               lam ltInt11.
                                                                 let t531 =
                                                                   lam geqInt11.
                                                                     let t532 =
                                                                       lam floorToInt11.
                                                                         let t533 =
                                                                           lam roomBlockWidth11.
                                                                             let t534 =
                                                                               lam m11.
                                                                                 let t535 =
                                                                                   lam p6.
                                                                                     let floorToInt12 =
                                                                                       floorToInt11
                                                                                     in
                                                                                     let roomBlockWidth12 =
                                                                                       roomBlockWidth11
                                                                                     in
                                                                                     let p7 =
                                                                                       p6
                                                                                     in
                                                                                     let t536 =
                                                                                       divf
                                                                                     in
                                                                                     let t537 =
                                                                                       match
                                                                                         p7
                                                                                       with
                                                                                         {x = x33}
                                                                                       then
                                                                                         x33
                                                                                       else
                                                                                         let t588 =
                                                                                           never
                                                                                         in
                                                                                         t588
                                                                                     in
                                                                                     let t538 =
                                                                                       t536
                                                                                         t537
                                                                                     in
                                                                                     let t539 =
                                                                                       t538
                                                                                         roomBlockWidth12
                                                                                     in
                                                                                     let t540 =
                                                                                       floorToInt12
                                                                                         t539
                                                                                     in
                                                                                     let t541 =
                                                                                       divf
                                                                                     in
                                                                                     let t542 =
                                                                                       match
                                                                                         p7
                                                                                       with
                                                                                         {y = x32}
                                                                                       then
                                                                                         x32
                                                                                       else
                                                                                         let t587 =
                                                                                           never
                                                                                         in
                                                                                         t587
                                                                                     in
                                                                                     let t543 =
                                                                                       t541
                                                                                         t542
                                                                                     in
                                                                                     let t544 =
                                                                                       t543
                                                                                         roomBlockWidth12
                                                                                     in
                                                                                     let t545 =
                                                                                       floorToInt12
                                                                                         t544
                                                                                     in
                                                                                     let t546 =
                                                                                       { row =
                                                                                           t545,
                                                                                         col =
                                                                                           t540 }
                                                                                     in
                                                                                     let c =
                                                                                       t546
                                                                                     in
                                                                                     let res =
                                                                                       true
                                                                                     in
                                                                                     let t547 =
                                                                                       match
                                                                                         c
                                                                                       with
                                                                                         {row = x31}
                                                                                       then
                                                                                         x31
                                                                                       else
                                                                                         let t586 =
                                                                                           never
                                                                                         in
                                                                                         t586
                                                                                     in
                                                                                     let t548 =
                                                                                       ltInt11
                                                                                         t547
                                                                                     in
                                                                                     let t549 =
                                                                                       0
                                                                                     in
                                                                                     let t550 =
                                                                                       t548
                                                                                         t549
                                                                                     in
                                                                                     let t551 =
                                                                                       match
                                                                                         t550
                                                                                       with
                                                                                         true
                                                                                       then
                                                                                         let t579 =
                                                                                           true
                                                                                         in
                                                                                         t579
                                                                                       else
                                                                                         let t580 =
                                                                                           match
                                                                                             c
                                                                                           with
                                                                                             {row = x30}
                                                                                           then
                                                                                             x30
                                                                                           else
                                                                                             let t585 =
                                                                                               never
                                                                                             in
                                                                                             t585
                                                                                         in
                                                                                         let t581 =
                                                                                           geqInt11
                                                                                             t580
                                                                                         in
                                                                                         let t582 =
                                                                                           match
                                                                                             m11
                                                                                           with
                                                                                             {rows = x29}
                                                                                           then
                                                                                             x29
                                                                                           else
                                                                                             let t584 =
                                                                                               never
                                                                                             in
                                                                                             t584
                                                                                         in
                                                                                         let t583 =
                                                                                           t581
                                                                                             t582
                                                                                         in
                                                                                         t583
                                                                                     in
                                                                                     let t552 =
                                                                                       match
                                                                                         t551
                                                                                       with
                                                                                         true
                                                                                       then
                                                                                         let t573 =
                                                                                           true
                                                                                         in
                                                                                         t573
                                                                                       else
                                                                                         let t574 =
                                                                                           match
                                                                                             c
                                                                                           with
                                                                                             {col = x28}
                                                                                           then
                                                                                             x28
                                                                                           else
                                                                                             let t578 =
                                                                                               never
                                                                                             in
                                                                                             t578
                                                                                         in
                                                                                         let t575 =
                                                                                           ltInt11
                                                                                             t574
                                                                                         in
                                                                                         let t576 =
                                                                                           0
                                                                                         in
                                                                                         let t577 =
                                                                                           t575
                                                                                             t576
                                                                                         in
                                                                                         t577
                                                                                     in
                                                                                     let t553 =
                                                                                       match
                                                                                         t552
                                                                                       with
                                                                                         true
                                                                                       then
                                                                                         let t566 =
                                                                                           true
                                                                                         in
                                                                                         t566
                                                                                       else
                                                                                         let t567 =
                                                                                           match
                                                                                             c
                                                                                           with
                                                                                             {col = x27}
                                                                                           then
                                                                                             x27
                                                                                           else
                                                                                             let t572 =
                                                                                               never
                                                                                             in
                                                                                             t572
                                                                                         in
                                                                                         let t568 =
                                                                                           geqInt11
                                                                                             t567
                                                                                         in
                                                                                         let t569 =
                                                                                           match
                                                                                             m11
                                                                                           with
                                                                                             {cols = x26}
                                                                                           then
                                                                                             x26
                                                                                           else
                                                                                             let t571 =
                                                                                               never
                                                                                             in
                                                                                             t571
                                                                                         in
                                                                                         let t570 =
                                                                                           t568
                                                                                             t569
                                                                                         in
                                                                                         t570
                                                                                     in
                                                                                     let res1 =
                                                                                       match
                                                                                         t553
                                                                                       with
                                                                                         true
                                                                                       then
                                                                                         let res2 =
                                                                                           false
                                                                                         in
                                                                                         res2
                                                                                       else
                                                                                         let t554 =
                                                                                           get
                                                                                         in
                                                                                         let t555 =
                                                                                           get
                                                                                         in
                                                                                         let t556 =
                                                                                           match
                                                                                             m11
                                                                                           with
                                                                                             {data = x25}
                                                                                           then
                                                                                             x25
                                                                                           else
                                                                                             let t565 =
                                                                                               never
                                                                                             in
                                                                                             t565
                                                                                         in
                                                                                         let t557 =
                                                                                           t555
                                                                                             t556
                                                                                         in
                                                                                         let t558 =
                                                                                           match
                                                                                             c
                                                                                           with
                                                                                             {row = x24}
                                                                                           then
                                                                                             x24
                                                                                           else
                                                                                             let t564 =
                                                                                               never
                                                                                             in
                                                                                             t564
                                                                                         in
                                                                                         let t559 =
                                                                                           t557
                                                                                             t558
                                                                                         in
                                                                                         let t560 =
                                                                                           t554
                                                                                             t559
                                                                                         in
                                                                                         let t561 =
                                                                                           match
                                                                                             c
                                                                                           with
                                                                                             {col = x23}
                                                                                           then
                                                                                             x23
                                                                                           else
                                                                                             let t563 =
                                                                                               never
                                                                                             in
                                                                                             t563
                                                                                         in
                                                                                         let t562 =
                                                                                           t560
                                                                                             t561
                                                                                         in
                                                                                         let res3 =
                                                                                           not
                                                                                             t562
                                                                                         in
                                                                                         res3
                                                                                     in
                                                                                     res1
                                                                                 in
                                                                                 t535
                                                                             in
                                                                             t534
                                                                         in
                                                                         t533
                                                                     in
                                                                     t532
                                                                 in
                                                                 t531
                                                             in
                                                             let timestampToSeconds =
                                                               lam intToFloat10.
                                                                 let t524 =
                                                                   lam ts1.
                                                                     let t525 =
                                                                       divf
                                                                     in
                                                                     let t526 =
                                                                       intToFloat10
                                                                         ts1
                                                                     in
                                                                     let t527 =
                                                                       t525
                                                                         t526
                                                                     in
                                                                     let t528 =
                                                                       1000000000
                                                                     in
                                                                     let t529 =
                                                                       intToFloat10
                                                                         t528
                                                                     in
                                                                     let t530 =
                                                                       t527
                                                                         t529
                                                                     in
                                                                     t530
                                                                 in
                                                                 t524
                                                             in
                                                             recursive
                                                               let loopFn1 =
                                                                 lam inBounds3.
                                                                   let t487 =
                                                                     lam eps1.
                                                                       let t488 =
                                                                         lam acc25.
                                                                           let t489 =
                                                                             match
                                                                               acc25
                                                                             with
                                                                               {p = x22}
                                                                             then
                                                                               x22
                                                                             else
                                                                               let t523 =
                                                                                 never
                                                                               in
                                                                               t523
                                                                           in
                                                                           let t490 =
                                                                             inBounds3
                                                                               t489
                                                                           in
                                                                           let t491 =
                                                                             match
                                                                               t490
                                                                             with
                                                                               true
                                                                             then
                                                                               let p3 =
                                                                                 match
                                                                                   acc25
                                                                                 with
                                                                                   {p = x21}
                                                                                 then
                                                                                   x21
                                                                                 else
                                                                                   let t522 =
                                                                                     never
                                                                                   in
                                                                                   t522
                                                                               in
                                                                               let t492 =
                                                                                 addf
                                                                               in
                                                                               let t493 =
                                                                                 match
                                                                                   p3
                                                                                 with
                                                                                   {x = x20}
                                                                                 then
                                                                                   x20
                                                                                 else
                                                                                   let t521 =
                                                                                     never
                                                                                   in
                                                                                   t521
                                                                               in
                                                                               let t494 =
                                                                                 t492
                                                                                   t493
                                                                               in
                                                                               let t495 =
                                                                                 mulf
                                                                               in
                                                                               let t496 =
                                                                                 t495
                                                                                   eps1
                                                                               in
                                                                               let t497 =
                                                                                 match
                                                                                   p3
                                                                                 with
                                                                                   {direction = x19}
                                                                                 then
                                                                                   x19
                                                                                 else
                                                                                   let t520 =
                                                                                     never
                                                                                   in
                                                                                   t520
                                                                               in
                                                                               let t498 =
                                                                                 cos
                                                                                   t497
                                                                               in
                                                                               let t499 =
                                                                                 t496
                                                                                   t498
                                                                               in
                                                                               let t500 =
                                                                                 t494
                                                                                   t499
                                                                               in
                                                                               let p4 =
                                                                                 { p3
                                                                                   with
                                                                                   x =
                                                                                     t500 }
                                                                               in
                                                                               let t501 =
                                                                                 addf
                                                                               in
                                                                               let t502 =
                                                                                 match
                                                                                   p4
                                                                                 with
                                                                                   {y = x18}
                                                                                 then
                                                                                   x18
                                                                                 else
                                                                                   let t519 =
                                                                                     never
                                                                                   in
                                                                                   t519
                                                                               in
                                                                               let t503 =
                                                                                 t501
                                                                                   t502
                                                                               in
                                                                               let t504 =
                                                                                 mulf
                                                                               in
                                                                               let t505 =
                                                                                 t504
                                                                                   eps1
                                                                               in
                                                                               let t506 =
                                                                                 match
                                                                                   p4
                                                                                 with
                                                                                   {direction = x17}
                                                                                 then
                                                                                   x17
                                                                                 else
                                                                                   let t518 =
                                                                                     never
                                                                                   in
                                                                                   t518
                                                                               in
                                                                               let t507 =
                                                                                 sin
                                                                                   t506
                                                                               in
                                                                               let t508 =
                                                                                 t505
                                                                                   t507
                                                                               in
                                                                               let t509 =
                                                                                 t503
                                                                                   t508
                                                                               in
                                                                               let p5 =
                                                                                 { p4
                                                                                   with
                                                                                   y =
                                                                                     t509 }
                                                                               in
                                                                               let t510 =
                                                                                 addf
                                                                               in
                                                                               let t511 =
                                                                                 match
                                                                                   acc25
                                                                                 with
                                                                                   {d = x16}
                                                                                 then
                                                                                   x16
                                                                                 else
                                                                                   let t517 =
                                                                                     never
                                                                                   in
                                                                                   t517
                                                                               in
                                                                               let t512 =
                                                                                 t510
                                                                                   t511
                                                                               in
                                                                               let t513 =
                                                                                 t512
                                                                                   eps1
                                                                               in
                                                                               let acc26 =
                                                                                 { acc25
                                                                                   with
                                                                                   d =
                                                                                     t513 }
                                                                               in
                                                                               let acc27 =
                                                                                 { acc26
                                                                                   with
                                                                                   p =
                                                                                     p5 }
                                                                               in
                                                                               let t514 =
                                                                                 loopFn1
                                                                                   inBounds3
                                                                               in
                                                                               let t515 =
                                                                                 t514
                                                                                   eps1
                                                                               in
                                                                               let t516 =
                                                                                 t515
                                                                                   acc27
                                                                               in
                                                                               t516
                                                                             else
                                                                               acc25
                                                                           in
                                                                           t491
                                                                       in
                                                                       t488
                                                                   in
                                                                   t487
                                                             in
                                                             let areaUnderLine =
                                                               lam subInt11.
                                                                 let t467 =
                                                                   lam intToFloat16.
                                                                     let t468 =
                                                                       lam f15.
                                                                         let t469 =
                                                                           lam a34.
                                                                             let t470 =
                                                                               lam b2.
                                                                                 let x15 =
                                                                                   f15
                                                                                     a34
                                                                                 in
                                                                                 let y =
                                                                                   f15
                                                                                     b2
                                                                                 in
                                                                                 let t471 =
                                                                                   timestampToSeconds
                                                                                     intToFloat16
                                                                                 in
                                                                                 let t472 =
                                                                                   subInt11
                                                                                     b2
                                                                                 in
                                                                                 let t473 =
                                                                                   t472
                                                                                     a34
                                                                                 in
                                                                                 let delta =
                                                                                   t471
                                                                                     t473
                                                                                 in
                                                                                 let t474 =
                                                                                   divf
                                                                                 in
                                                                                 let t475 =
                                                                                   addf
                                                                                 in
                                                                                 let t476 =
                                                                                   mulf
                                                                                 in
                                                                                 let t477 =
                                                                                   t476
                                                                                     x15
                                                                                 in
                                                                                 let t478 =
                                                                                   t477
                                                                                     delta
                                                                                 in
                                                                                 let t479 =
                                                                                   t475
                                                                                     t478
                                                                                 in
                                                                                 let t480 =
                                                                                   mulf
                                                                                 in
                                                                                 let t481 =
                                                                                   t480
                                                                                     y
                                                                                 in
                                                                                 let t482 =
                                                                                   t481
                                                                                     delta
                                                                                 in
                                                                                 let t483 =
                                                                                   t479
                                                                                     t482
                                                                                 in
                                                                                 let t484 =
                                                                                   t474
                                                                                     t483
                                                                                 in
                                                                                 let t485 =
                                                                                   2.
                                                                                 in
                                                                                 let t486 =
                                                                                   t484
                                                                                     t485
                                                                                 in
                                                                                 t486
                                                                             in
                                                                             t470
                                                                         in
                                                                         t469
                                                                     in
                                                                     t468
                                                                 in
                                                                 t467
                                                             in
                                                             let pi3 =
                                                               pi9
                                                             in
                                                             let nanosPerSec4 =
                                                               nanosPerSec15
                                                             in
                                                             let wallLogicalTime4 =
                                                               wallLogicalTime13
                                                             in
                                                             let subInt3 =
                                                               subInt6
                                                             in
                                                             let ltInt3 =
                                                               ltInt6
                                                             in
                                                             let geqInt3 =
                                                               geqInt6
                                                             in
                                                             let floorToInt3 =
                                                               floorToInt6
                                                             in
                                                             let intToFloat3 =
                                                               intToFloat11
                                                             in
                                                             let roomBlockWidth3 =
                                                               roomBlockWidth6
                                                             in
                                                             let usMaxRange3 =
                                                               usMaxRange6
                                                             in
                                                             let tofMaxRange3 =
                                                               tofMaxRange6
                                                             in
                                                             let frontLeftOffset3 =
                                                               frontLeftOffset6
                                                             in
                                                             let frontRightOffset3 =
                                                               frontRightOffset6
                                                             in
                                                             let rearLeftOffset3 =
                                                               rearLeftOffset6
                                                             in
                                                             let rearRightOffset3 =
                                                               rearRightOffset6
                                                             in
                                                             let sideLeftOffset3 =
                                                               sideLeftOffset6
                                                             in
                                                             let sideRightOffset3 =
                                                               sideRightOffset6
                                                             in
                                                             let m1 =
                                                               m6
                                                             in
                                                             let frEst =
                                                               frEst3
                                                             in
                                                             let rlEst =
                                                               rlEst3
                                                             in
                                                             let rrEst =
                                                               rrEst3
                                                             in
                                                             let rEst =
                                                               rEst3
                                                             in
                                                             let saObs =
                                                               saObs3
                                                             in
                                                             let lEst =
                                                               lEst4
                                                             in
                                                             let flEst =
                                                               flEst4
                                                             in
                                                             let acc2 =
                                                               acc17
                                                             in
                                                             let #var"2" =
                                                               {}
                                                             in
                                                             let pi10 =
                                                               pi3
                                                             in
                                                             let nanosPerSec16 =
                                                               nanosPerSec4
                                                             in
                                                             let wallLogicalTime14 =
                                                               wallLogicalTime4
                                                             in
                                                             let subInt7 =
                                                               subInt3
                                                             in
                                                             let ltInt7 =
                                                               ltInt3
                                                             in
                                                             let geqInt7 =
                                                               geqInt3
                                                             in
                                                             let floorToInt7 =
                                                               floorToInt3
                                                             in
                                                             let intToFloat12 =
                                                               intToFloat3
                                                             in
                                                             let roomBlockWidth7 =
                                                               roomBlockWidth3
                                                             in
                                                             let usMaxRange7 =
                                                               usMaxRange3
                                                             in
                                                             let tofMaxRange7 =
                                                               tofMaxRange3
                                                             in
                                                             let frontLeftOffset7 =
                                                               frontLeftOffset3
                                                             in
                                                             let frontRightOffset7 =
                                                               frontRightOffset3
                                                             in
                                                             let rearLeftOffset7 =
                                                               rearLeftOffset3
                                                             in
                                                             let rearRightOffset7 =
                                                               rearRightOffset3
                                                             in
                                                             let sideLeftOffset7 =
                                                               sideLeftOffset3
                                                             in
                                                             let sideRightOffset7 =
                                                               sideRightOffset3
                                                             in
                                                             let m7 =
                                                               m1
                                                             in
                                                             let prior =
                                                               match
                                                                 acc2
                                                               with
                                                                 {d = x14}
                                                               then
                                                                 x14
                                                               else
                                                                 let t466 =
                                                                   never
                                                                 in
                                                                 t466
                                                             in
                                                             let flEst5 =
                                                               flEst
                                                             in
                                                             let frEst4 =
                                                               frEst
                                                             in
                                                             let rlEst4 =
                                                               rlEst
                                                             in
                                                             let rrEst4 =
                                                               rrEst
                                                             in
                                                             let lEst5 =
                                                               lEst
                                                             in
                                                             let rEst4 =
                                                               rEst
                                                             in
                                                             let speedEst1 =
                                                               match
                                                                 acc2
                                                               with
                                                                 {speed = x13}
                                                               then
                                                                 x13
                                                               else
                                                                 let t465 =
                                                                   never
                                                                 in
                                                                 t465
                                                             in
                                                             let saObs4 =
                                                               saObs
                                                             in
                                                             let t290 =
                                                               value
                                                                 prior
                                                             in
                                                             let posPrev =
                                                               sample
                                                                 t290
                                                             in
                                                             let t291 =
                                                               value
                                                                 speedEst1
                                                             in
                                                             let speedLine =
                                                               sample
                                                                 t291
                                                             in
                                                             let nanosPerSec17 =
                                                               nanosPerSec16
                                                             in
                                                             let wallLogicalTime15 =
                                                               wallLogicalTime14
                                                             in
                                                             let subInt8 =
                                                               subInt7
                                                             in
                                                             let intToFloat13 =
                                                               intToFloat12
                                                             in
                                                             let nanosPerSec18 =
                                                               nanosPerSec16
                                                             in
                                                             let wallLogicalTime16 =
                                                               wallLogicalTime14
                                                             in
                                                             let t292 =
                                                               timestamp
                                                                 nanosPerSec16
                                                             in
                                                             let t293 =
                                                               t292
                                                                 wallLogicalTime14
                                                             in
                                                             let offset =
                                                               t293
                                                                 speedEst1
                                                             in
                                                             let value1 =
                                                               speedLine
                                                             in
                                                             let t294 =
                                                               deref
                                                             in
                                                             let lt1 =
                                                               t294
                                                                 wallLogicalTime16
                                                             in
                                                             let nanosPerSec19 =
                                                               nanosPerSec18
                                                             in
                                                             let lhs1 =
                                                               lt1
                                                             in
                                                             let nanosPerSec20 =
                                                               nanosPerSec18
                                                             in
                                                             let nanos =
                                                               offset
                                                             in
                                                             let t295 =
                                                               divi
                                                             in
                                                             let t296 =
                                                               t295
                                                                 nanos
                                                             in
                                                             let s24 =
                                                               t296
                                                                 nanosPerSec20
                                                             in
                                                             let t297 =
                                                               modi
                                                             in
                                                             let t298 =
                                                               t297
                                                                 nanos
                                                             in
                                                             let ns2 =
                                                               t298
                                                                 nanosPerSec20
                                                             in
                                                             let t299 =
                                                               (s24, ns2)
                                                             in
                                                             let rhs1 =
                                                               t299
                                                             in
                                                             let t300 =
                                                               (lhs1, rhs1)
                                                             in
                                                             let t301 =
                                                               match
                                                                 t300
                                                               with
                                                                 ((ls1, lns1), (rs1, rns1))
                                                               then
                                                                 let t447 =
                                                                   addi
                                                                 in
                                                                 let t448 =
                                                                   t447
                                                                     ls1
                                                                 in
                                                                 let s27 =
                                                                   t448
                                                                     rs1
                                                                 in
                                                                 let t449 =
                                                                   addi
                                                                 in
                                                                 let t450 =
                                                                   t449
                                                                     lns1
                                                                 in
                                                                 let ns3 =
                                                                   t450
                                                                     rns1
                                                                 in
                                                                 let t451 =
                                                                   geqi
                                                                 in
                                                                 let t452 =
                                                                   t451
                                                                     ns3
                                                                 in
                                                                 let t453 =
                                                                   t452
                                                                     nanosPerSec19
                                                                 in
                                                                 let t454 =
                                                                   match
                                                                     t453
                                                                   with
                                                                     true
                                                                   then
                                                                     let t455 =
                                                                       subi
                                                                     in
                                                                     let t456 =
                                                                       t455
                                                                         ns3
                                                                     in
                                                                     let t457 =
                                                                       t456
                                                                         nanosPerSec19
                                                                     in
                                                                     let t458 =
                                                                       addi
                                                                     in
                                                                     let t459 =
                                                                       t458
                                                                         s27
                                                                     in
                                                                     let t460 =
                                                                       1
                                                                     in
                                                                     let t461 =
                                                                       t459
                                                                         t460
                                                                     in
                                                                     let t462 =
                                                                       (t461, t457)
                                                                     in
                                                                     t462
                                                                   else
                                                                     let t463 =
                                                                       (s27, ns3)
                                                                     in
                                                                     t463
                                                                 in
                                                                 t454
                                                               else
                                                                 let t464 =
                                                                   never
                                                                 in
                                                                 t464
                                                             in
                                                             let t302 =
                                                               (t301, value1)
                                                             in
                                                             let line =
                                                               t302
                                                             in
                                                             let t303 =
                                                               lam t431.
                                                                 let t432 =
                                                                   timestamp
                                                                     nanosPerSec17
                                                                 in
                                                                 let t433 =
                                                                   t432
                                                                     wallLogicalTime15
                                                                 in
                                                                 let lineTs =
                                                                   t433
                                                                     line
                                                                 in
                                                                 let t434 =
                                                                   timestampToSeconds
                                                                     intToFloat13
                                                                 in
                                                                 let t435 =
                                                                   subInt8
                                                                     t431
                                                                 in
                                                                 let t436 =
                                                                   t435
                                                                     lineTs
                                                                 in
                                                                 let ts3 =
                                                                   t434
                                                                     t436
                                                                 in
                                                                 let l2 =
                                                                   value
                                                                     line
                                                                 in
                                                                 let t437 =
                                                                   addf
                                                                 in
                                                                 let t438 =
                                                                   mulf
                                                                 in
                                                                 let t439 =
                                                                   match
                                                                     l2
                                                                   with
                                                                     {slope = x12}
                                                                   then
                                                                     x12
                                                                   else
                                                                     let t446 =
                                                                       never
                                                                     in
                                                                     t446
                                                                 in
                                                                 let t440 =
                                                                   t438
                                                                     t439
                                                                 in
                                                                 let t441 =
                                                                   t440
                                                                     ts3
                                                                 in
                                                                 let t442 =
                                                                   t437
                                                                     t441
                                                                 in
                                                                 let t443 =
                                                                   match
                                                                     l2
                                                                   with
                                                                     {intercept = x11}
                                                                   then
                                                                     x11
                                                                   else
                                                                     let t445 =
                                                                       never
                                                                     in
                                                                     t445
                                                                 in
                                                                 let t444 =
                                                                   t442
                                                                     t443
                                                                 in
                                                                 t444
                                                             in
                                                             let speedFn =
                                                               t303
                                                             in
                                                             let t304 =
                                                               timestamp
                                                                 nanosPerSec16
                                                             in
                                                             let t305 =
                                                               t304
                                                                 wallLogicalTime14
                                                             in
                                                             let t306 =
                                                               t305
                                                                 prior
                                                             in
                                                             let v =
                                                               speedFn
                                                                 t306
                                                             in
                                                             let t307 =
                                                               match
                                                                 posPrev
                                                               with
                                                                 {direction = x10}
                                                               then
                                                                 x10
                                                               else
                                                                 let t430 =
                                                                   never
                                                                 in
                                                                 t430
                                                             in
                                                             let t308 =
                                                               divf
                                                             in
                                                             let t309 =
                                                               t308
                                                                 pi10
                                                             in
                                                             let t310 =
                                                               8.
                                                             in
                                                             let t311 =
                                                               t309
                                                                 t310
                                                             in
                                                             let t312 =
                                                               RuntimeDistElementary_DistGaussian
                                                                 { mu =
                                                                     t307,
                                                                   sigma =
                                                                     t311 }
                                                             in
                                                             let d5 =
                                                               sample
                                                                 t312
                                                             in
                                                             let t313 =
                                                               match
                                                                 posPrev
                                                               with
                                                                 {x = x9}
                                                               then
                                                                 x9
                                                               else
                                                                 let t429 =
                                                                   never
                                                                 in
                                                                 t429
                                                             in
                                                             let t314 =
                                                               0.1
                                                             in
                                                             let t315 =
                                                               RuntimeDistElementary_DistGaussian
                                                                 { mu =
                                                                     t313,
                                                                   sigma =
                                                                     t314 }
                                                             in
                                                             let bx =
                                                               sample
                                                                 t315
                                                             in
                                                             let t316 =
                                                               match
                                                                 posPrev
                                                               with
                                                                 {y = x8}
                                                               then
                                                                 x8
                                                               else
                                                                 let t428 =
                                                                   never
                                                                 in
                                                                 t428
                                                             in
                                                             let t317 =
                                                               0.1
                                                             in
                                                             let t318 =
                                                               RuntimeDistElementary_DistGaussian
                                                                 { mu =
                                                                     t316,
                                                                   sigma =
                                                                     t317 }
                                                             in
                                                             let by =
                                                               sample
                                                                 t318
                                                             in
                                                             let t319 =
                                                               0.5
                                                             in
                                                             let t320 =
                                                               1.
                                                             in
                                                             let t321 =
                                                               RuntimeDistElementary_DistGamma
                                                                 { scale =
                                                                     t320,
                                                                   shape =
                                                                     t319 }
                                                             in
                                                             let sigma2 =
                                                               sample
                                                                 t321
                                                             in
                                                             let t322 =
                                                               { ofs =
                                                                   sideRightOffset7,
                                                                 obs =
                                                                   rEst4,
                                                                 maxRange =
                                                                   tofMaxRange7 }
                                                             in
                                                             let t323 =
                                                               { ofs =
                                                                   sideLeftOffset7,
                                                                 obs =
                                                                   lEst5,
                                                                 maxRange =
                                                                   tofMaxRange7 }
                                                             in
                                                             let t324 =
                                                               { ofs =
                                                                   rearRightOffset7,
                                                                 obs =
                                                                   rrEst4,
                                                                 maxRange =
                                                                   usMaxRange7 }
                                                             in
                                                             let t325 =
                                                               { ofs =
                                                                   rearLeftOffset7,
                                                                 obs =
                                                                   rlEst4,
                                                                 maxRange =
                                                                   usMaxRange7 }
                                                             in
                                                             let t326 =
                                                               { ofs =
                                                                   frontRightOffset7,
                                                                 obs =
                                                                   frEst4,
                                                                 maxRange =
                                                                   usMaxRange7 }
                                                             in
                                                             let t327 =
                                                               { ofs =
                                                                   frontLeftOffset7,
                                                                 obs =
                                                                   flEst5,
                                                                 maxRange =
                                                                   usMaxRange7 }
                                                             in
                                                             let sensorData =
                                                               [ t327,
                                                                 t326,
                                                                 t325,
                                                                 t324,
                                                                 t323,
                                                                 t322 ]
                                                             in
                                                             let t328 =
                                                               foldl3
                                                             in
                                                             let nanosPerSec21 =
                                                               nanosPerSec16
                                                             in
                                                             let wallLogicalTime17 =
                                                               wallLogicalTime14
                                                             in
                                                             let subInt9 =
                                                               subInt7
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
                                                             let intToFloat14 =
                                                               intToFloat12
                                                             in
                                                             let roomBlockWidth8 =
                                                               roomBlockWidth7
                                                             in
                                                             let m8 =
                                                               m7
                                                             in
                                                             let prior1 =
                                                               prior
                                                             in
                                                             let speedFn1 =
                                                               speedFn
                                                             in
                                                             let d6 =
                                                               d5
                                                             in
                                                             let bx1 =
                                                               bx
                                                             in
                                                             let by1 =
                                                               by
                                                             in
                                                             let sigma3 =
                                                               sigma2
                                                             in
                                                             let t329 =
                                                               lam #var"21".
                                                                 let t376 =
                                                                   lam s25.
                                                                     let t377 =
                                                                       withinRoomBounds
                                                                         ltInt8
                                                                     in
                                                                     let t378 =
                                                                       t377
                                                                         geqInt8
                                                                     in
                                                                     let t379 =
                                                                       t378
                                                                         floorToInt8
                                                                     in
                                                                     let t380 =
                                                                       t379
                                                                         roomBlockWidth8
                                                                     in
                                                                     let inBounds =
                                                                       t380
                                                                         m8
                                                                     in
                                                                     let t381 =
                                                                       foldl3
                                                                     in
                                                                     let nanosPerSec22 =
                                                                       nanosPerSec21
                                                                     in
                                                                     let wallLogicalTime18 =
                                                                       wallLogicalTime17
                                                                     in
                                                                     let subInt10 =
                                                                       subInt9
                                                                     in
                                                                     let intToFloat15 =
                                                                       intToFloat14
                                                                     in
                                                                     let prior2 =
                                                                       prior1
                                                                     in
                                                                     let speedFn2 =
                                                                       speedFn1
                                                                     in
                                                                     let d7 =
                                                                       d6
                                                                     in
                                                                     let bx2 =
                                                                       bx1
                                                                     in
                                                                     let by2 =
                                                                       by1
                                                                     in
                                                                     let sigma4 =
                                                                       sigma3
                                                                     in
                                                                     let s26 =
                                                                       s25
                                                                     in
                                                                     let inBounds1 =
                                                                       inBounds
                                                                     in
                                                                     let t382 =
                                                                       lam #var"23".
                                                                         let t389 =
                                                                           lam tsv4.
                                                                             let t390 =
                                                                               areaUnderLine
                                                                                 subInt10
                                                                             in
                                                                             let t391 =
                                                                               t390
                                                                                 intToFloat15
                                                                             in
                                                                             let t392 =
                                                                               t391
                                                                                 speedFn2
                                                                             in
                                                                             let t393 =
                                                                               timestamp
                                                                                 nanosPerSec22
                                                                             in
                                                                             let t394 =
                                                                               t393
                                                                                 wallLogicalTime18
                                                                             in
                                                                             let t395 =
                                                                               t394
                                                                                 prior2
                                                                             in
                                                                             let t396 =
                                                                               t392
                                                                                 t395
                                                                             in
                                                                             let t397 =
                                                                               timestamp
                                                                                 nanosPerSec22
                                                                             in
                                                                             let t398 =
                                                                               t397
                                                                                 wallLogicalTime18
                                                                             in
                                                                             let t399 =
                                                                               t398
                                                                                 tsv4
                                                                             in
                                                                             let distTravelled =
                                                                               t396
                                                                                 t399
                                                                             in
                                                                             let t400 =
                                                                               addf
                                                                             in
                                                                             let t401 =
                                                                               t400
                                                                                 bx2
                                                                             in
                                                                             let t402 =
                                                                               mulf
                                                                             in
                                                                             let t403 =
                                                                               t402
                                                                                 distTravelled
                                                                             in
                                                                             let t404 =
                                                                               cos
                                                                                 d7
                                                                             in
                                                                             let t405 =
                                                                               t403
                                                                                 t404
                                                                             in
                                                                             let xt =
                                                                               t401
                                                                                 t405
                                                                             in
                                                                             let t406 =
                                                                               addf
                                                                             in
                                                                             let t407 =
                                                                               t406
                                                                                 by2
                                                                             in
                                                                             let t408 =
                                                                               mulf
                                                                             in
                                                                             let t409 =
                                                                               t408
                                                                                 distTravelled
                                                                             in
                                                                             let t410 =
                                                                               sin
                                                                                 d7
                                                                             in
                                                                             let t411 =
                                                                               t409
                                                                                 t410
                                                                             in
                                                                             let yt =
                                                                               t407
                                                                                 t411
                                                                             in
                                                                             let pos1 =
                                                                               { x =
                                                                                   xt,
                                                                                 y =
                                                                                   yt,
                                                                                 direction =
                                                                                   d7 }
                                                                             in
                                                                             let ofs1 =
                                                                               match
                                                                                 s26
                                                                               with
                                                                                 {ofs = x7}
                                                                               then
                                                                                 x7
                                                                               else
                                                                                 let t427 =
                                                                                   never
                                                                                 in
                                                                                 t427
                                                                             in
                                                                             let p1 =
                                                                               pos1
                                                                             in
                                                                             let inBounds2 =
                                                                               inBounds1
                                                                             in
                                                                             let eps =
                                                                               0.01
                                                                             in
                                                                             let t412 =
                                                                               positionAtOffset
                                                                                 p1
                                                                             in
                                                                             let p2 =
                                                                               t412
                                                                                 ofs1
                                                                             in
                                                                             let t413 =
                                                                               0.
                                                                             in
                                                                             let acc23 =
                                                                               { d =
                                                                                   t413,
                                                                                 p =
                                                                                   p2 }
                                                                             in
                                                                             let t414 =
                                                                               loopFn1
                                                                                 inBounds2
                                                                             in
                                                                             let t415 =
                                                                               t414
                                                                                 eps
                                                                             in
                                                                             let acc24 =
                                                                               t415
                                                                                 acc23
                                                                             in
                                                                             let t416 =
                                                                               match
                                                                                 acc24
                                                                               with
                                                                                 {d = x6}
                                                                               then
                                                                                 x6
                                                                               else
                                                                                 let t426 =
                                                                                   never
                                                                                 in
                                                                                 t426
                                                                             in
                                                                             let expectedDist =
                                                                               t416
                                                                             in
                                                                             let t417 =
                                                                               gtf
                                                                             in
                                                                             let t418 =
                                                                               t417
                                                                                 expectedDist
                                                                             in
                                                                             let t419 =
                                                                               match
                                                                                 s26
                                                                               with
                                                                                 {maxRange = x5}
                                                                               then
                                                                                 x5
                                                                               else
                                                                                 let t425 =
                                                                                   never
                                                                                 in
                                                                                 t425
                                                                             in
                                                                             let t420 =
                                                                               t418
                                                                                 t419
                                                                             in
                                                                             let expectedDist1 =
                                                                               match
                                                                                 t420
                                                                               with
                                                                                 true
                                                                               then
                                                                                 let expectedDist2 =
                                                                                   match
                                                                                     s26
                                                                                   with
                                                                                     {maxRange = x4}
                                                                                   then
                                                                                     x4
                                                                                   else
                                                                                     let t424 =
                                                                                       never
                                                                                     in
                                                                                     t424
                                                                                 in
                                                                                 expectedDist2
                                                                               else
                                                                                 expectedDist
                                                                             in
                                                                             let t421 =
                                                                               value
                                                                                 tsv4
                                                                             in
                                                                             let estDist =
                                                                               sample
                                                                                 t421
                                                                             in
                                                                             let t422 =
                                                                               RuntimeDistElementary_DistGaussian
                                                                                 { mu =
                                                                                     expectedDist1,
                                                                                   sigma =
                                                                                     sigma4 }
                                                                             in
                                                                             let #var"24": () =
                                                                               updateWeight
                                                                                 (logObserve
                                                                                    t422
                                                                                    estDist)
                                                                                 state2
                                                                             in
                                                                             let t423 =
                                                                               {}
                                                                             in
                                                                             t423
                                                                         in
                                                                         t389
                                                                     in
                                                                     let t383 =
                                                                       t381
                                                                         t382
                                                                     in
                                                                     let t384 =
                                                                       {}
                                                                     in
                                                                     let t385 =
                                                                       t383
                                                                         t384
                                                                     in
                                                                     let t386 =
                                                                       match
                                                                         s25
                                                                       with
                                                                         {obs = x3}
                                                                       then
                                                                         x3
                                                                       else
                                                                         let t388 =
                                                                           never
                                                                         in
                                                                         t388
                                                                     in
                                                                     let #var"22" =
                                                                       t385
                                                                         t386
                                                                     in
                                                                     let t387 =
                                                                       {}
                                                                     in
                                                                     t387
                                                                 in
                                                                 t376
                                                             in
                                                             let t330 =
                                                               t328
                                                                 t329
                                                             in
                                                             let t331 =
                                                               {}
                                                             in
                                                             let t332 =
                                                               t330
                                                                 t331
                                                             in
                                                             let #var"18" =
                                                               t332
                                                                 sensorData
                                                             in
                                                             let t333 =
                                                               areaUnderLine
                                                                 subInt7
                                                             in
                                                             let t334 =
                                                               t333
                                                                 intToFloat12
                                                             in
                                                             let t335 =
                                                               t334
                                                                 speedFn
                                                             in
                                                             let t336 =
                                                               timestamp
                                                                 nanosPerSec16
                                                             in
                                                             let t337 =
                                                               t336
                                                                 wallLogicalTime14
                                                             in
                                                             let t338 =
                                                               t337
                                                                 prior
                                                             in
                                                             let t339 =
                                                               t335
                                                                 t338
                                                             in
                                                             let t340 =
                                                               0
                                                             in
                                                             let dist =
                                                               t339
                                                                 t340
                                                             in
                                                             let t341 =
                                                               addf
                                                             in
                                                             let t342 =
                                                               t341
                                                                 by
                                                             in
                                                             let t343 =
                                                               mulf
                                                             in
                                                             let t344 =
                                                               t343
                                                                 dist
                                                             in
                                                             let t345 =
                                                               sin
                                                                 d5
                                                             in
                                                             let t346 =
                                                               t344
                                                                 t345
                                                             in
                                                             let t347 =
                                                               t342
                                                                 t346
                                                             in
                                                             let t348 =
                                                               addf
                                                             in
                                                             let t349 =
                                                               t348
                                                                 bx
                                                             in
                                                             let t350 =
                                                               mulf
                                                             in
                                                             let t351 =
                                                               t350
                                                                 dist
                                                             in
                                                             let t352 =
                                                               cos
                                                                 d5
                                                             in
                                                             let t353 =
                                                               t351
                                                                 t352
                                                             in
                                                             let t354 =
                                                               t349
                                                                 t353
                                                             in
                                                             let pos =
                                                               { x =
                                                                   t354,
                                                                 y =
                                                                   t347,
                                                                 direction =
                                                                   d5 }
                                                             in
                                                             let ltInt9 =
                                                               ltInt7
                                                             in
                                                             let geqInt9 =
                                                               geqInt7
                                                             in
                                                             let floorToInt9 =
                                                               floorToInt7
                                                             in
                                                             let roomBlockWidth9 =
                                                               roomBlockWidth7
                                                             in
                                                             let frontLeftOffset8 =
                                                               frontLeftOffset7
                                                             in
                                                             let frontRightOffset8 =
                                                               frontRightOffset7
                                                             in
                                                             let rearLeftOffset8 =
                                                               rearLeftOffset7
                                                             in
                                                             let rearRightOffset8 =
                                                               rearRightOffset7
                                                             in
                                                             let sideLeftOffset8 =
                                                               sideLeftOffset7
                                                             in
                                                             let sideRightOffset8 =
                                                               sideRightOffset7
                                                             in
                                                             let m9 =
                                                               m7
                                                             in
                                                             let center =
                                                               pos
                                                             in
                                                             let sensorOffsets =
                                                               [ frontLeftOffset8,
                                                                 frontRightOffset8,
                                                                 rearLeftOffset8,
                                                                 rearRightOffset8,
                                                                 sideLeftOffset8,
                                                                 sideRightOffset8 ]
                                                             in
                                                             let acc18 =
                                                               true
                                                             in
                                                             let t355 =
                                                               foldl3
                                                             in
                                                             let ltInt10 =
                                                               ltInt9
                                                             in
                                                             let geqInt10 =
                                                               geqInt9
                                                             in
                                                             let floorToInt10 =
                                                               floorToInt9
                                                             in
                                                             let roomBlockWidth10 =
                                                               roomBlockWidth9
                                                             in
                                                             let m10 =
                                                               m9
                                                             in
                                                             let center1 =
                                                               center
                                                             in
                                                             let t356 =
                                                               lam acc20.
                                                                 let t369 =
                                                                   lam ofs.
                                                                     let acc21 =
                                                                       match
                                                                         acc20
                                                                       with
                                                                         true
                                                                       then
                                                                         let t370 =
                                                                           positionAtOffset
                                                                             center1
                                                                         in
                                                                         let p =
                                                                           t370
                                                                             ofs
                                                                         in
                                                                         let t371 =
                                                                           withinRoomBounds
                                                                             ltInt10
                                                                         in
                                                                         let t372 =
                                                                           t371
                                                                             geqInt10
                                                                         in
                                                                         let t373 =
                                                                           t372
                                                                             floorToInt10
                                                                         in
                                                                         let t374 =
                                                                           t373
                                                                             roomBlockWidth10
                                                                         in
                                                                         let t375 =
                                                                           t374
                                                                             m10
                                                                         in
                                                                         let acc22 =
                                                                           t375
                                                                             p
                                                                         in
                                                                         acc22
                                                                       else
                                                                         acc20
                                                                     in
                                                                     acc21
                                                                 in
                                                                 t369
                                                             in
                                                             let t357 =
                                                               t355
                                                                 t356
                                                             in
                                                             let t358 =
                                                               t357
                                                                 acc18
                                                             in
                                                             let acc19 =
                                                               t358
                                                                 sensorOffsets
                                                             in
                                                             let t359 =
                                                               not
                                                                 acc19
                                                             in
                                                             let #var"19" =
                                                               match
                                                                 t359
                                                               with
                                                                 true
                                                               then
                                                                 let t360 =
                                                                   divf
                                                                 in
                                                                 let t361 =
                                                                   negf
                                                                 in
                                                                 let t362 =
                                                                   1.
                                                                 in
                                                                 let t363 =
                                                                   t361
                                                                     t362
                                                                 in
                                                                 let t364 =
                                                                   t360
                                                                     t363
                                                                 in
                                                                 let t365 =
                                                                   0.
                                                                 in
                                                                 let t366 =
                                                                   t364
                                                                     t365
                                                                 in
                                                                 let #var"20" =
                                                                   updateWeight
                                                                     t366
                                                                     state2
                                                                 in
                                                                 let t367 =
                                                                   {}
                                                                 in
                                                                 t367
                                                               else
                                                                 let t368 =
                                                                   {}
                                                                 in
                                                                 t368
                                                             in
                                                             (lam x2.
                                                                End
                                                                  x2)
                                                               pos)
in
let t22 =
  lam nanosPerSec8: Int.
    lam wallLogicalTime8: Ref (Timespec).
      lam intToFloat6: Int -> Float.
        lam maxSpeed3: Float.
          lam wheelCircumference3: Float.
            lam state1: State.
              stopInit
                (lam #var"10".
                   let map1 =
                     lam f14.
                       let t275 =
                         lam s22.
                           recursive
                             let rec9 =
                               lam s23.
                                 let t277 =
                                   match
                                     s23
                                   with
                                     ""
                                   then
                                     let t278 =
                                       ""
                                     in
                                     t278
                                   else
                                     let t279 =
                                       match
                                         s23
                                       with
                                         [ a32 ]
                                       then
                                         let t280 =
                                           f14
                                             a32
                                         in
                                         let t281 =
                                           [ t280 ]
                                         in
                                         t281
                                       else
                                         let t282 =
                                           match
                                             s23
                                           with
                                             [ a33 ] ++ ss7 ++ ""
                                           then
                                             let t283 =
                                               cons
                                             in
                                             let t284 =
                                               f14
                                                 a33
                                             in
                                             let t285 =
                                               t283
                                                 t284
                                             in
                                             let t286 =
                                               rec9
                                                 ss7
                                             in
                                             let t287 =
                                               t285
                                                 t286
                                             in
                                             t287
                                           else
                                             let t288 =
                                               never
                                             in
                                             t288
                                         in
                                         t282
                                     in
                                     t279
                                 in
                                 t277
                           in
                           let t276 =
                             rec9
                               s22
                           in
                           t276
                       in
                       t275
                   in
                   let iter1 =
                     lam f13.
                       let t272 =
                         lam s21.
                           let t273 =
                             map1
                               f13
                           in
                           let #var"16" =
                             t273
                               s21
                           in
                           let t274 =
                             {}
                           in
                           t274
                       in
                       t272
                   in
                   let mapi1 =
                     lam f12.
                       let t248 =
                         lam s19.
                           recursive
                             let rec8 =
                               lam i3.
                                 let t252 =
                                   lam s20.
                                     let t253 =
                                       match
                                         s20
                                       with
                                         ""
                                       then
                                         let t254 =
                                           ""
                                         in
                                         t254
                                       else
                                         let t255 =
                                           match
                                             s20
                                           with
                                             [ a30 ]
                                           then
                                             let t256 =
                                               f12
                                                 i3
                                             in
                                             let t257 =
                                               t256
                                                 a30
                                             in
                                             let t258 =
                                               [ t257 ]
                                             in
                                             t258
                                           else
                                             let t259 =
                                               match
                                                 s20
                                               with
                                                 [ a31 ] ++ ss6 ++ ""
                                               then
                                                 let t260 =
                                                   cons
                                                 in
                                                 let t261 =
                                                   f12
                                                     i3
                                                 in
                                                 let t262 =
                                                   t261
                                                     a31
                                                 in
                                                 let t263 =
                                                   t260
                                                     t262
                                                 in
                                                 let t264 =
                                                   addi
                                                 in
                                                 let t265 =
                                                   t264
                                                     i3
                                                 in
                                                 let t266 =
                                                   1
                                                 in
                                                 let t267 =
                                                   t265
                                                     t266
                                                 in
                                                 let t268 =
                                                   rec8
                                                     t267
                                                 in
                                                 let t269 =
                                                   t268
                                                     ss6
                                                 in
                                                 let t270 =
                                                   t263
                                                     t269
                                                 in
                                                 t270
                                               else
                                                 let t271 =
                                                   never
                                                 in
                                                 t271
                                             in
                                             t259
                                         in
                                         t255
                                     in
                                     t253
                                 in
                                 t252
                           in
                           let t249 =
                             0
                           in
                           let t250 =
                             rec8
                               t249
                           in
                           let t251 =
                             t250
                               s19
                           in
                           t251
                       in
                       t248
                   in
                   let iteri1 =
                     lam f11.
                       let t245 =
                         lam s18.
                           let t246 =
                             mapi1
                               f11
                           in
                           let #var"15" =
                             t246
                               s18
                           in
                           let t247 =
                             {}
                           in
                           t247
                       in
                       t245
                   in
                   let foldl2 =
                     lam f10.
                       let t233 =
                         lam acc15.
                           let t234 =
                             lam s16.
                               recursive
                                 let rec7 =
                                   lam acc16.
                                     let t237 =
                                       lam s17.
                                         let t238 =
                                           match
                                             s17
                                           with
                                             ""
                                           then
                                             acc16
                                           else
                                             let t239 =
                                               match
                                                 s17
                                               with
                                                 [ a29 ] ++ ss5 ++ ""
                                               then
                                                 let t240 =
                                                   f10
                                                     acc16
                                                 in
                                                 let t241 =
                                                   t240
                                                     a29
                                                 in
                                                 let t242 =
                                                   rec7
                                                     t241
                                                 in
                                                 let t243 =
                                                   t242
                                                     ss5
                                                 in
                                                 t243
                                               else
                                                 let t244 =
                                                   never
                                                 in
                                                 t244
                                             in
                                             t239
                                         in
                                         t238
                                     in
                                     t237
                               in
                               let t235 =
                                 rec7
                                   acc15
                               in
                               let t236 =
                                 t235
                                   s16
                               in
                               t236
                           in
                           t234
                       in
                       t233
                   in
                   let foldr2 =
                     lam f9.
                       let t221 =
                         lam acc13.
                           let t222 =
                             lam s14.
                               recursive
                                 let rec6 =
                                   lam acc14.
                                     let t225 =
                                       lam s15.
                                         let t226 =
                                           match
                                             s15
                                           with
                                             ""
                                           then
                                             acc14
                                           else
                                             let t227 =
                                               match
                                                 s15
                                               with
                                                 [ a28 ] ++ ss4 ++ ""
                                               then
                                                 let t228 =
                                                   f9
                                                     a28
                                                 in
                                                 let t229 =
                                                   rec6
                                                     acc14
                                                 in
                                                 let t230 =
                                                   t229
                                                     ss4
                                                 in
                                                 let t231 =
                                                   t228
                                                     t230
                                                 in
                                                 t231
                                               else
                                                 let t232 =
                                                   never
                                                 in
                                                 t232
                                             in
                                             t227
                                         in
                                         t226
                                     in
                                     t225
                               in
                               let t223 =
                                 rec6
                                   acc13
                               in
                               let t224 =
                                 t223
                                   s14
                               in
                               t224
                           in
                           t222
                       in
                       t221
                   in
                   let create1 =
                     lam l1.
                       let t197 =
                         lam f8.
                           recursive
                             let rec5 =
                               lam i2.
                                 let t205 =
                                   lam acc12.
                                     let t206 =
                                       geqi
                                     in
                                     let t207 =
                                       t206
                                         i2
                                     in
                                     let t208 =
                                       0
                                     in
                                     let t209 =
                                       t207
                                         t208
                                     in
                                     let t210 =
                                       match
                                         t209
                                       with
                                         true
                                       then
                                         let t211 =
                                           subi
                                         in
                                         let t212 =
                                           t211
                                             i2
                                         in
                                         let t213 =
                                           1
                                         in
                                         let t214 =
                                           t212
                                             t213
                                         in
                                         let t215 =
                                           rec5
                                             t214
                                         in
                                         let t216 =
                                           cons
                                         in
                                         let t217 =
                                           f8
                                             i2
                                         in
                                         let t218 =
                                           t216
                                             t217
                                         in
                                         let t219 =
                                           t218
                                             acc12
                                         in
                                         let t220 =
                                           t215
                                             t219
                                         in
                                         t220
                                       else
                                         acc12
                                     in
                                     t210
                                 in
                                 t205
                           in
                           let t198 =
                             subi
                           in
                           let t199 =
                             t198
                               l1
                           in
                           let t200 =
                             1
                           in
                           let t201 =
                             t199
                               t200
                           in
                           let t202 =
                             rec5
                               t201
                           in
                           let t203 =
                             ""
                           in
                           let t204 =
                             t202
                               t203
                           in
                           t204
                       in
                       t197
                   in
                   let t128 =
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
                   let maxSpeed4 =
                     maxSpeed3
                   in
                   let wheelCircumference4 =
                     wheelCircumference3
                   in
                   let #var"11" =
                     {}
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
                   let maxSpeed5 =
                     maxSpeed4
                   in
                   let wheelCircumference5 =
                     wheelCircumference4
                   in
                   let speedObs =
                     ""
                   in
                   let t129 =
                     0.
                   in
                   let t130 =
                     0.1
                   in
                   let t131 =
                     RuntimeDistElementary_DistGaussian
                       { mu =
                           t129,
                         sigma =
                           t130 }
                   in
                   let m4 =
                     sample
                       t131
                   in
                   let t132 =
                     0.
                   in
                   let t133 =
                     RuntimeDistElementary_DistUniform
                       { a =
                           t132,
                         b =
                           maxSpeed5 }
                   in
                   let b =
                     sample
                       t133
                   in
                   let t134 =
                     1.
                   in
                   let t135 =
                     1.
                   in
                   let t136 =
                     RuntimeDistElementary_DistGamma
                       { scale =
                           t135,
                         shape =
                           t134 }
                   in
                   let sigma =
                     sample
                       t136
                   in
                   let t137 =
                     foldl2
                   in
                   let nanosPerSec11 =
                     nanosPerSec10
                   in
                   let wallLogicalTime11 =
                     wallLogicalTime10
                   in
                   let intToFloat9 =
                     intToFloat8
                   in
                   let wheelCircumference6 =
                     wheelCircumference5
                   in
                   let m5 =
                     m4
                   in
                   let b1 =
                     b
                   in
                   let sigma1 =
                     sigma
                   in
                   let t138 =
                     lam #var"13".
                       let t143 =
                         lam tsv1.
                           let intToFloat10 =
                             intToFloat9
                           in
                           let nanosPerSec12 =
                             nanosPerSec11
                           in
                           let wallLogicalTime12 =
                             wallLogicalTime11
                           in
                           let tsv2 =
                             tsv1
                           in
                           let t144 =
                             deref
                           in
                           let lt =
                             t144
                               wallLogicalTime12
                           in
                           let nanosPerSec13 =
                             nanosPerSec12
                           in
                           let nanosPerSec14 =
                             nanosPerSec12
                           in
                           let lhs =
                             match
                               tsv2
                             with
                               (#var"X1",)
                             then
                               #var"X1"
                             else
                               let t196 =
                                 never
                               in
                               t196
                           in
                           let rhs =
                             lt
                           in
                           let t145 =
                             (lhs, rhs)
                           in
                           let t146 =
                             match
                               t145
                             with
                               ((ls, lns), (rs, rns))
                             then
                               let t177 =
                                 subi
                               in
                               let t178 =
                                 t177
                                   ls
                               in
                               let s13 =
                                 t178
                                   rs
                               in
                               let t179 =
                                 subi
                               in
                               let t180 =
                                 t179
                                   lns
                               in
                               let ns1 =
                                 t180
                                   rns
                               in
                               let t181 =
                                 lti
                               in
                               let t182 =
                                 t181
                                   ns1
                               in
                               let t183 =
                                 0
                               in
                               let t184 =
                                 t182
                                   t183
                               in
                               let t185 =
                                 match
                                   t184
                                 with
                                   true
                                 then
                                   let t186 =
                                     addi
                                   in
                                   let t187 =
                                     t186
                                       ns1
                                   in
                                   let t188 =
                                     t187
                                       nanosPerSec14
                                   in
                                   let t189 =
                                     subi
                                   in
                                   let t190 =
                                     t189
                                       s13
                                   in
                                   let t191 =
                                     1
                                   in
                                   let t192 =
                                     t190
                                       t191
                                   in
                                   let t193 =
                                     (t192, t188)
                                   in
                                   t193
                                 else
                                   let t194 =
                                     (s13, ns1)
                                   in
                                   t194
                               in
                               t185
                             else
                               let t195 =
                                 never
                               in
                               t195
                           in
                           let ts =
                             t146
                           in
                           let t147 =
                             match
                               ts
                             with
                               (s12, ns)
                             then
                               let t170 =
                                 addi
                               in
                               let t171 =
                                 muli
                               in
                               let t172 =
                                 t171
                                   s12
                               in
                               let t173 =
                                 t172
                                   nanosPerSec13
                               in
                               let t174 =
                                 t170
                                   t173
                               in
                               let t175 =
                                 t174
                                   ns
                               in
                               t175
                             else
                               let t176 =
                                 never
                               in
                               t176
                           in
                           let ts1 =
                             t147
                           in
                           let t148 =
                             divf
                           in
                           let t149 =
                             intToFloat10
                               ts1
                           in
                           let t150 =
                             t148
                               t149
                           in
                           let t151 =
                             1000000000
                           in
                           let t152 =
                             intToFloat10
                               t151
                           in
                           let t153 =
                             t150
                               t152
                           in
                           let ts2 =
                             t153
                           in
                           let tsv3 =
                             tsv1
                           in
                           let t154 =
                             match
                               tsv3
                             with
                               {#label"1" = #var"X"}
                             then
                               #var"X"
                             else
                               let t169 =
                                 never
                               in
                               t169
                           in
                           let rpm =
                             t154
                           in
                           let t155 =
                             divf
                           in
                           let t156 =
                             mulf
                           in
                           let t157 =
                             t156
                               rpm
                           in
                           let t158 =
                             t157
                               wheelCircumference6
                           in
                           let t159 =
                             t155
                               t158
                           in
                           let t160 =
                             60.
                           in
                           let mps =
                             t159
                               t160
                           in
                           let t161 =
                             addf
                           in
                           let t162 =
                             mulf
                           in
                           let t163 =
                             t162
                               m5
                           in
                           let t164 =
                             t163
                               ts2
                           in
                           let t165 =
                             t161
                               t164
                           in
                           let t166 =
                             t165
                               b1
                           in
                           let t167 =
                             RuntimeDistElementary_DistGaussian
                               { mu =
                                   t166,
                                 sigma =
                                   sigma1 }
                           in
                           let #var"14": () =
                             updateWeight
                               (logObserve
                                  t167
                                  mps)
                               state1
                           in
                           let t168 =
                             {}
                           in
                           t168
                       in
                       t143
                   in
                   let t139 =
                     t137
                       t138
                   in
                   let t140 =
                     {}
                   in
                   let t141 =
                     t139
                       t140
                   in
                   let #var"12" =
                     t141
                       speedObs
                   in
                   let t142 =
                     { slope =
                         m4,
                       intercept =
                         b }
                   in
                   (lam x1.
                      End
                        x1)
                     t142)
in
let t23 =
  lam blocks1: [Coordinate].
    lam pi6: Float.
      lam state: State.
        stopInit
          (lam #var"6".
             let map =
               lam f7.
                 let t114 =
                   lam s10.
                     recursive
                       let rec4 =
                         lam s11.
                           let t116 =
                             match
                               s11
                             with
                               ""
                             then
                               let t117 =
                                 ""
                               in
                               t117
                             else
                               let t118 =
                                 match
                                   s11
                                 with
                                   [ a26 ]
                                 then
                                   let t119 =
                                     f7
                                       a26
                                   in
                                   let t120 =
                                     [ t119 ]
                                   in
                                   t120
                                 else
                                   let t121 =
                                     match
                                       s11
                                     with
                                       [ a27 ] ++ ss3 ++ ""
                                     then
                                       let t122 =
                                         cons
                                       in
                                       let t123 =
                                         f7
                                           a27
                                       in
                                       let t124 =
                                         t122
                                           t123
                                       in
                                       let t125 =
                                         rec4
                                           ss3
                                       in
                                       let t126 =
                                         t124
                                           t125
                                       in
                                       t126
                                     else
                                       let t127 =
                                         never
                                       in
                                       t127
                                   in
                                   t121
                               in
                               t118
                           in
                           t116
                     in
                     let t115 =
                       rec4
                         s10
                     in
                     t115
                 in
                 t114
             in
             let iter =
               lam f6.
                 let t111 =
                   lam s9.
                     let t112 =
                       map
                         f6
                     in
                     let #var"9" =
                       t112
                         s9
                     in
                     let t113 =
                       {}
                     in
                     t113
                 in
                 t111
             in
             let mapi =
               lam f5.
                 let t87 =
                   lam s7.
                     recursive
                       let rec3 =
                         lam i1.
                           let t91 =
                             lam s8.
                               let t92 =
                                 match
                                   s8
                                 with
                                   ""
                                 then
                                   let t93 =
                                     ""
                                   in
                                   t93
                                 else
                                   let t94 =
                                     match
                                       s8
                                     with
                                       [ a24 ]
                                     then
                                       let t95 =
                                         f5
                                           i1
                                       in
                                       let t96 =
                                         t95
                                           a24
                                       in
                                       let t97 =
                                         [ t96 ]
                                       in
                                       t97
                                     else
                                       let t98 =
                                         match
                                           s8
                                         with
                                           [ a25 ] ++ ss2 ++ ""
                                         then
                                           let t99 =
                                             cons
                                           in
                                           let t100 =
                                             f5
                                               i1
                                           in
                                           let t101 =
                                             t100
                                               a25
                                           in
                                           let t102 =
                                             t99
                                               t101
                                           in
                                           let t103 =
                                             addi
                                           in
                                           let t104 =
                                             t103
                                               i1
                                           in
                                           let t105 =
                                             1
                                           in
                                           let t106 =
                                             t104
                                               t105
                                           in
                                           let t107 =
                                             rec3
                                               t106
                                           in
                                           let t108 =
                                             t107
                                               ss2
                                           in
                                           let t109 =
                                             t102
                                               t108
                                           in
                                           t109
                                         else
                                           let t110 =
                                             never
                                           in
                                           t110
                                       in
                                       t98
                                   in
                                   t94
                               in
                               t92
                           in
                           t91
                     in
                     let t88 =
                       0
                     in
                     let t89 =
                       rec3
                         t88
                     in
                     let t90 =
                       t89
                         s7
                     in
                     t90
                 in
                 t87
             in
             let iteri =
               lam f4.
                 let t84 =
                   lam s6.
                     let t85 =
                       mapi
                         f4
                     in
                     let #var"8" =
                       t85
                         s6
                     in
                     let t86 =
                       {}
                     in
                     t86
                 in
                 t84
             in
             let foldl =
               lam f3.
                 let t72 =
                   lam acc10.
                     let t73 =
                       lam s4.
                         recursive
                           let rec2 =
                             lam acc11.
                               let t76 =
                                 lam s5.
                                   let t77 =
                                     match
                                       s5
                                     with
                                       ""
                                     then
                                       acc11
                                     else
                                       let t78 =
                                         match
                                           s5
                                         with
                                           [ a23 ] ++ ss1 ++ ""
                                         then
                                           let t79 =
                                             f3
                                               acc11
                                           in
                                           let t80 =
                                             t79
                                               a23
                                           in
                                           let t81 =
                                             rec2
                                               t80
                                           in
                                           let t82 =
                                             t81
                                               ss1
                                           in
                                           t82
                                         else
                                           let t83 =
                                             never
                                           in
                                           t83
                                       in
                                       t78
                                   in
                                   t77
                               in
                               t76
                         in
                         let t74 =
                           rec2
                             acc10
                         in
                         let t75 =
                           t74
                             s4
                         in
                         t75
                     in
                     t73
                 in
                 t72
             in
             let foldr =
               lam f2.
                 let t60 =
                   lam acc8.
                     let t61 =
                       lam s2.
                         recursive
                           let rec1 =
                             lam acc9.
                               let t64 =
                                 lam s3.
                                   let t65 =
                                     match
                                       s3
                                     with
                                       ""
                                     then
                                       acc9
                                     else
                                       let t66 =
                                         match
                                           s3
                                         with
                                           [ a22 ] ++ ss ++ ""
                                         then
                                           let t67 =
                                             f2
                                               a22
                                           in
                                           let t68 =
                                             rec1
                                               acc9
                                           in
                                           let t69 =
                                             t68
                                               ss
                                           in
                                           let t70 =
                                             t67
                                               t69
                                           in
                                           t70
                                         else
                                           let t71 =
                                             never
                                           in
                                           t71
                                       in
                                       t66
                                   in
                                   t65
                               in
                               t64
                         in
                         let t62 =
                           rec1
                             acc8
                         in
                         let t63 =
                           t62
                             s2
                         in
                         t63
                     in
                     t61
                 in
                 t60
             in
             let create =
               lam l.
                 let t36 =
                   lam f1.
                     recursive
                       let rec =
                         lam i.
                           let t44 =
                             lam acc7.
                               let t45 =
                                 geqi
                               in
                               let t46 =
                                 t45
                                   i
                               in
                               let t47 =
                                 0
                               in
                               let t48 =
                                 t46
                                   t47
                               in
                               let t49 =
                                 match
                                   t48
                                 with
                                   true
                                 then
                                   let t50 =
                                     subi
                                   in
                                   let t51 =
                                     t50
                                       i
                                   in
                                   let t52 =
                                     1
                                   in
                                   let t53 =
                                     t51
                                       t52
                                   in
                                   let t54 =
                                     rec
                                       t53
                                   in
                                   let t55 =
                                     cons
                                   in
                                   let t56 =
                                     f1
                                       i
                                   in
                                   let t57 =
                                     t55
                                       t56
                                   in
                                   let t58 =
                                     t57
                                       acc7
                                   in
                                   let t59 =
                                     t54
                                       t58
                                   in
                                   t59
                                 else
                                   acc7
                               in
                               t49
                           in
                           t44
                     in
                     let t37 =
                       subi
                     in
                     let t38 =
                       t37
                         l
                     in
                     let t39 =
                       1
                     in
                     let t40 =
                       t38
                         t39
                     in
                     let t41 =
                       rec
                         t40
                     in
                     let t42 =
                       ""
                     in
                     let t43 =
                       t41
                         t42
                     in
                     t43
                 in
                 t36
             in
             let t26 =
               {}
             in
             let pi7 =
               pi6
             in
             let blocks2 =
               blocks1
             in
             let #var"7" =
               {}
             in
             let pi8 =
               pi7
             in
             let blocks3 =
               blocks2
             in
             let t27 =
               0.
             in
             let t28 =
               mulf
             in
             let t29 =
               2.
             in
             let t30 =
               t28
                 t29
             in
             let t31 =
               t30
                 pi8
             in
             let t32 =
               RuntimeDistElementary_DistUniform
                 { a =
                     t27,
                   b =
                     t31 }
             in
             let d4 =
               sample
                 t32
             in
             let t33 =
               5.55
             in
             let t34 =
               0.8
             in
             let t35 =
               { x =
                   t34,
                 y =
                   t33,
                 direction =
                   d4 }
             in
             (lam x.
                End
                  x)
               t35)
in
recursive
  let t25 =
    lam pi3.
      lam nanosPerSec4.
        lam wallLogicalTime4.
          lam subInt3.
            lam ltInt3.
              lam geqInt3.
                lam floorToInt3.
                  lam intToFloat3.
                    lam roomBlockWidth3.
                      lam usMaxRange3.
                        lam tofMaxRange3.
                          lam frontLeftOffset3.
                            lam frontRightOffset3.
                              lam rearLeftOffset3.
                                lam rearRightOffset3.
                                  lam sideLeftOffset3.
                                    lam sideRightOffset3.
                                      lam m1.
                                        lam frEst.
                                          lam rlEst.
                                            lam rrEst.
                                              lam rEst.
                                                lam saObs.
                                                  lam lEst.
                                                    lam flEst.
                                                      lam acc2.
                                                        lam #var"2".
                                                          positionModel
                                                            pi3
                                                            nanosPerSec4
                                                            wallLogicalTime4
                                                            subInt3
                                                            ltInt3
                                                            geqInt3
                                                            floorToInt3
                                                            intToFloat3
                                                            roomBlockWidth3
                                                            usMaxRange3
                                                            tofMaxRange3
                                                            frontLeftOffset3
                                                            frontRightOffset3
                                                            rearLeftOffset3
                                                            rearRightOffset3
                                                            sideLeftOffset3
                                                            sideRightOffset3
                                                            m1
                                                            (acc2.d)
                                                            flEst
                                                            frEst
                                                            rlEst
                                                            rrEst
                                                            lEst
                                                            rEst
                                                            (acc2.speed)
                                                            saObs
  let inferModel =
    lam pi4.
      lam nanosPerSec5.
        lam wallLogicalTime5.
          lam subInt4.
            lam ltInt4.
              lam geqInt4.
                lam floorToInt4.
                  lam intToFloat4.
                    lam roomBlockWidth4.
                      lam usMaxRange4.
                        lam tofMaxRange4.
                          lam frontLeftOffset4.
                            lam frontRightOffset4.
                              lam rearLeftOffset4.
                                lam rearRightOffset4.
                                  lam sideLeftOffset4.
                                    lam sideRightOffset4.
                                      lam m2.
                                        lam frEst1.
                                          lam rlEst1.
                                            lam rrEst1.
                                              lam rEst1.
                                                lam saObs1.
                                                  lam lEst1.
                                                    lam flEst1.
                                                      lam acc3.
                                                        lam #var"3".
                                                          run
                                                            { particles =
                                                                100 }
                                                            (t21
                                                               pi4
                                                               nanosPerSec5
                                                               wallLogicalTime5
                                                               subInt4
                                                               ltInt4
                                                               geqInt4
                                                               floorToInt4
                                                               intToFloat4
                                                               roomBlockWidth4
                                                               usMaxRange4
                                                               tofMaxRange4
                                                               frontLeftOffset4
                                                               frontRightOffset4
                                                               rearLeftOffset4
                                                               rearRightOffset4
                                                               sideLeftOffset4
                                                               sideRightOffset4
                                                               m2
                                                               frEst1
                                                               rlEst1
                                                               rrEst1
                                                               rEst1
                                                               saObs1
                                                               lEst1
                                                               flEst1
                                                               acc3)
  let distToSamples =
    lam d1.
      distEmpiricalSamples
        d1
  let samplesToDist =
    lam s1.
      #var"RuntimeDistEmpirical_constructDistEmpiricalHelper"
        s1
  let distNormConst =
    lam d2.
      distEmpiricalNormConst
        d2
  let loopFn =
    lam fileDescriptors3.
      lam inputSeqs3.
        lam outputSeqs3.
          lam nanosPerSec6.
            lam monoLogicalTime3.
              lam wallLogicalTime6.
                lam pi5.
                  lam nanosPerSec7.
                    lam wallLogicalTime7.
                      lam subInt5.
                        lam ltInt5.
                          lam geqInt5.
                            lam floorToInt5.
                              lam intToFloat5.
                                lam roomBlockWidth5.
                                  lam usMaxRange5.
                                    lam tofMaxRange5.
                                      lam frontLeftOffset5.
                                        lam frontRightOffset5.
                                          lam rearLeftOffset5.
                                            lam rearRightOffset5.
                                              lam sideLeftOffset5.
                                                lam sideRightOffset5.
                                                  lam period1.
                                                    lam posBudget1.
                                                      lam m3.
                                                        lam acc4.
                                                          match
                                                            true
                                                          with
                                                            true
                                                          then
                                                            let #var"4" =
                                                              let out =
                                                                deref
                                                                  outputSeqs3
                                                              in
                                                              modref
                                                                outputSeqs3
                                                                { out
                                                                  with
                                                                  posEst =
                                                                    cons
                                                                      (tsv
                                                                         nanosPerSec6
                                                                         wallLogicalTime6
                                                                         0
                                                                         (value
                                                                            (acc4.d)))
                                                                      (out.posEst) }
                                                            in
                                                            let #var"5" =
                                                              sdelay
                                                                nanosPerSec6
                                                                monoLogicalTime3
                                                                wallLogicalTime6
                                                                (flushOutputs
                                                                   fileDescriptors3
                                                                   outputSeqs3)
                                                                (updateInputs
                                                                   fileDescriptors3
                                                                   inputSeqs3)
                                                                period1
                                                            in
                                                            let flEst2 =
                                                              unsafeCoerce
                                                                ((deref
                                                                    inputSeqs3).frontLeft)
                                                            in
                                                            let frEst2 =
                                                              unsafeCoerce
                                                                ((deref
                                                                    inputSeqs3).frontRight)
                                                            in
                                                            let rlEst2 =
                                                              unsafeCoerce
                                                                ((deref
                                                                    inputSeqs3).rearLeft)
                                                            in
                                                            let rrEst2 =
                                                              unsafeCoerce
                                                                ((deref
                                                                    inputSeqs3).rearRight)
                                                            in
                                                            let lEst2 =
                                                              unsafeCoerce
                                                                ((deref
                                                                    inputSeqs3).left)
                                                            in
                                                            let rEst2 =
                                                              unsafeCoerce
                                                                ((deref
                                                                    inputSeqs3).right)
                                                            in
                                                            let speedEst =
                                                              unsafeCoerce
                                                                ((deref
                                                                    inputSeqs3).speed)
                                                            in
                                                            let saObs2 =
                                                              unsafeCoerce
                                                                ((deref
                                                                    inputSeqs3).steeringAngle)
                                                            in
                                                            let lEst3 =
                                                              ""
                                                            in
                                                            let flEst3 =
                                                              ""
                                                            in
                                                            let acc5 =
                                                              { acc4
                                                                with
                                                                speed =
                                                                  maxDistLineTimestamp
                                                                    nanosPerSec7
                                                                    wallLogicalTime7
                                                                    ltInt5
                                                                    (acc4.speed)
                                                                    speedEst }
                                                            in
                                                            let d3 =
                                                              rtpplInferRunner
                                                                nanosPerSec7
                                                                (inferModel
                                                                   pi5
                                                                   nanosPerSec7
                                                                   wallLogicalTime7
                                                                   subInt5
                                                                   ltInt5
                                                                   geqInt5
                                                                   floorToInt5
                                                                   intToFloat5
                                                                   roomBlockWidth5
                                                                   usMaxRange5
                                                                   tofMaxRange5
                                                                   frontLeftOffset5
                                                                   frontRightOffset5
                                                                   rearLeftOffset5
                                                                   rearRightOffset5
                                                                   sideLeftOffset5
                                                                   sideRightOffset5
                                                                   m3
                                                                   frEst2
                                                                   rlEst2
                                                                   rrEst2
                                                                   rEst2
                                                                   saObs2
                                                                   lEst3
                                                                   flEst3
                                                                   acc5)
                                                                distToSamples
                                                                samplesToDist
                                                                distNormConst
                                                                posBudget1
                                                            in
printLn (join ["position estimate finished at t=", int2string (timespecToNanos nanosPerSec (deref wallLogicalTime7))]);
                                                            let acc6 =
                                                              { acc5
                                                                with
                                                                d =
                                                                  tsv
                                                                    nanosPerSec7
                                                                    wallLogicalTime7
                                                                    0
                                                                    d3 }
                                                            in
                                                            loopFn
                                                              fileDescriptors3
                                                              inputSeqs3
                                                              outputSeqs3
                                                              nanosPerSec6
                                                              monoLogicalTime3
                                                              wallLogicalTime6
                                                              pi5
                                                              nanosPerSec7
                                                              wallLogicalTime7
                                                              subInt5
                                                              ltInt5
                                                              geqInt5
                                                              floorToInt5
                                                              intToFloat5
                                                              roomBlockWidth5
                                                              usMaxRange5
                                                              tofMaxRange5
                                                              frontLeftOffset5
                                                              frontRightOffset5
                                                              rearLeftOffset5
                                                              rearRightOffset5
                                                              sideLeftOffset5
                                                              sideRightOffset5
                                                              period1
                                                              posBudget1
                                                              m3
                                                              acc6
                                                          else
                                                            acc4
in
let #var"RTPPL_positionEstimate" =
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
                      lam ltInt2.
                        lam geqInt2.
                          lam floorToInt2.
                            lam intToFloat2.
                              lam roomBlockWidth2.
                                lam maxSpeed2.
                                  lam wheelCircumference2.
                                    lam usMaxRange2.
                                      lam tofMaxRange2.
                                        lam frontLeftOffset2.
                                          lam frontRightOffset2.
                                            lam rearLeftOffset2.
                                              lam rearRightOffset2.
                                                lam sideLeftOffset2.
                                                  lam sideRightOffset2.
                                                    lam period.
                                                      let posBudget =
                                                        subInt2
                                                          period
                                                          100000000
                                                      in
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
                                                          (t22
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
                                                          (t23
                                                             blocks
                                                             pi2)
                                                      in
                                                      let acc =
                                                        { d =
                                                            tsv
                                                              nanosPerSec3
                                                              wallLogicalTime3
                                                              0
                                                              d,
                                                          speed =
                                                            tsv
                                                              nanosPerSec3
                                                              wallLogicalTime3
                                                              0
                                                              initSpeed }
                                                      in
                                                      let acc1 =
                                                        loopFn
                                                          fileDescriptors2
                                                          inputSeqs2
                                                          outputSeqs2
                                                          nanosPerSec2
                                                          monoLogicalTime2
                                                          wallLogicalTime2
                                                          pi2
                                                          nanosPerSec3
                                                          wallLogicalTime3
                                                          subInt2
                                                          ltInt2
                                                          geqInt2
                                                          floorToInt2
                                                          intToFloat2
                                                          roomBlockWidth2
                                                          usMaxRange2
                                                          tofMaxRange2
                                                          frontLeftOffset2
                                                          frontRightOffset2
                                                          rearLeftOffset2
                                                          rearRightOffset2
                                                          sideLeftOffset2
                                                          sideRightOffset2
                                                          period
                                                          posBudget
                                                          m
                                                          acc
                                                      in
                                                      {}
in
let t24 =
  lam fileDescriptors1.
    lam inputSeqs1.
      lam outputSeqs1.
        lam pi1.
          lam nanosPerSec1.
            lam monoLogicalTime1.
              lam wallLogicalTime1.
                lam subInt1.
                  lam ltInt1.
                    lam geqInt1.
                      lam floorToInt1.
                        lam intToFloat1.
                          lam roomBlockWidth1.
                            lam maxSpeed1.
                              lam wheelCircumference1.
                                lam usMaxRange1.
                                  lam tofMaxRange1.
                                    lam frontLeftOffset1.
                                      lam frontRightOffset1.
                                        lam rearLeftOffset1.
                                          lam rearRightOffset1.
                                            lam sideLeftOffset1.
                                              lam sideRightOffset1.
                                                lam #var"1".
                                                  #var"RTPPL_positionEstimate"
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
                                                    ltInt1
                                                    geqInt1
                                                    floorToInt1
                                                    intToFloat1
                                                    roomBlockWidth1
                                                    maxSpeed1
                                                    wheelCircumference1
                                                    usMaxRange1
                                                    tofMaxRange1
                                                    frontLeftOffset1
                                                    frontRightOffset1
                                                    rearLeftOffset1
                                                    rearRightOffset1
                                                    sideLeftOffset1
                                                    sideRightOffset1
                                                    1000000000
in
rtpplRuntimeInit
  monoLogicalTime
  wallLogicalTime
  (updateInputs
     fileDescriptors
     inputSeqs)
  (closeFileDescriptors
     fileDescriptors)
  (t24
     fileDescriptors
     inputSeqs
     outputSeqs
     pi
     nanosPerSec
     monoLogicalTime
     wallLogicalTime
     subInt
     ltInt
     geqInt
     floorToInt
     intToFloat
     roomBlockWidth
     maxSpeed
     wheelCircumference
     usMaxRange
     tofMaxRange
     frontLeftOffset
     frontRightOffset
     rearLeftOffset
     rearRightOffset
     sideLeftOffset
     sideRightOffset)
