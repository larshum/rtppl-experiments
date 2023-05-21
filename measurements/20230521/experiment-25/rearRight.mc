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
  lam a134.
    match
      a134
    with
      true
    then
      false
    else
      true
in
let and: Bool -> Bool -> Bool =
  lam a133.
    lam b53.
      match
        a133
      with
        true
      then
        b53
      else
        false
in
let or: Bool -> Bool -> Bool =
  lam a132.
    lam b52.
      match
        a132
      with
        true
      then
        true
      else
        b52
in
let xor: Bool -> Bool -> Bool =
  lam a131.
    lam b51.
      match
        a131
      with
        true
      then
        not
          b51
      else
        b51
in
let xnor: Bool -> Bool -> Bool =
  lam a130.
    lam b50.
      not
        (xor
           a130
           b50)
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
  lam b49.
    match
      b49
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
let optionEq: all a129. all b48. (a129 -> b48 -> Bool) -> Option a129 -> Option b48 -> Bool =
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
let optionMap: all a128. all b47. (a128 -> b47) -> Option a128 -> Option b47 =
  lam f53.
    lam o19.
      match
        o19
      with
        Some t460
      then
        Some
          (f53
             t460)
      else
        None
          {}
in
let optionMapAccum: all a126. all b45. all acc75. (acc75 -> a126 -> (acc75, b45)) -> acc75 -> Option a126 -> (acc75, Option b45) =
  lam f52.
    lam acc76.
      lam o18.
        match
          o18
        with
          Some a127
        then
          match
            f52
              acc76
              a127
          with
            (acc77, b46)
          in
          (acc77, Some
              b46)
        else
          (acc76, None
            {})
in
let optionJoin: all a125. Option (Option a125) -> Option a125 =
  lam o17.
    match
      o17
    with
      Some t459
    then
      t459
    else
      None
        {}
in
let optionBind: all a124. all b44. Option a124 -> (a124 -> Option b44) -> Option b44 =
  lam o16.
    lam f51.
      optionJoin
        (optionMap
           f51
           o16)
in
let optionCompose: all a123. all b43. all c40. (b43 -> Option c40) -> (a123 -> Option b43) -> a123 -> Option c40 =
  lam f50.
    lam g3.
      lam x72.
        optionBind
          (g3
             x72)
          f50
in
let optionZipWith: all a122. all b42. all c39. (a122 -> b42 -> c39) -> Option a122 -> Option b42 -> Option c39 =
  lam f49.
    lam o15.
      lam o25.
        match
          (o15, o25)
        with
          (Some v12, Some v22)
        then
          Some
            (f49
               v12
               v22)
        else
          None
            {}
in
let optionZipWithOrElse: all a121. all b41. all c38. (() -> c38) -> (a121 -> b41 -> c38) -> Option a121 -> Option b41 -> c38 =
  lam d10.
    lam f48.
      lam o14.
        lam o24.
          match
            (o14, o24)
          with
            (Some v11, Some v21)
          then
            f48
              v11
              v21
          else
            d10
              {}
in
let optionZipWithOr: all a120. all b40. all c37. c37 -> (a120 -> b40 -> c37) -> Option a120 -> Option b40 -> c37 =
  lam v7.
    optionZipWithOrElse
      (lam #var"90".
         v7)
in
let optionGetOrElse: all a119. (() -> a119) -> Option a119 -> a119 =
  lam d9.
    lam o10.
      match
        o10
      with
        Some t458
      then
        t458
      else
        d9
          {}
in
let optionGetOr: all a118. a118 -> Option a118 -> a118 =
  lam d8.
    optionGetOrElse
      (lam #var"89".
         d8)
in
let optionMapOrElse: all a117. all b39. (() -> b39) -> (a117 -> b39) -> Option a117 -> b39 =
  lam d7.
    lam f47.
      lam o9.
        optionGetOrElse
          d7
          (optionMap
             f47
             o9)
in
let optionMapOr: all a116. all b38. b38 -> (a116 -> b38) -> Option a116 -> b38 =
  lam d6.
    lam f46.
      lam o8.
        optionGetOr
          d6
          (optionMap
             f46
             o8)
in
let optionMapM: all a115. all b37. (a115 -> Option b37) -> [a115] -> Option [b37] =
  lam f45.
    lam l20.
      recursive
        let g2 =
          lam l21.
            lam acc74.
              match
                l21
              with
                [ hd ] ++ rest1 ++ ""
              then
                match
                  f45
                    hd
                with
                  Some x71
                then
                  g2
                    rest1
                    (snoc
                       acc74
                       x71)
                else
                  None
                    {}
              else
                Some
                  acc74
      in
      g2
        l20
        ""
in
let optionFoldlM: all a112. all b35. (a112 -> b35 -> Option a112) -> a112 -> [b35] -> Option a112 =
  lam f44.
    recursive
      let recur =
        lam a113.
          lam bs1.
            match
              bs1
            with
              [ b36 ] ++ bs2 ++ ""
            then
              let res21 =
                f44
                  a113
                  b36
              in
              match
                res21
              with
                Some a114
              then
                recur
                  a114
                  bs2
              else
                match
                  res21
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
                  a113
    in
    recur
in
let optionContains: all a111. Option a111 -> (a111 -> Bool) -> Bool =
  lam o7.
    lam p20.
      optionMapOr
        false
        p20
        o7
in
let optionIsSome: all a109. Option a109 -> Bool =
  lam o6.
    optionContains
      o6
      (lam #var"88".
         true)
in
let optionIsNone: all a108. Option a108 -> Bool =
  lam o5.
    not
      (optionIsSome
         o5)
in
let optionAnd: all a107. Option a107 -> Option a107 -> Option a107 =
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
let optionFilter: all a106. (a106 -> Bool) -> Option a106 -> Option a106 =
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
let optionOrElse: all a105. (() -> Option a105) -> Option a105 -> Option a105 =
  lam f43.
    lam o3.
      optionGetOrElse
        f43
        (optionMap
           (lam x70.
              Some
                x70)
           o3)
in
let optionOr: all a104. Option a104 -> Option a104 -> Option a104 =
  lam o12.
    lam o22.
      optionOrElse
        (lam #var"87".
           o22)
        o12
in
let optionXor: all a103. Option a103 -> Option a103 -> Option a103 =
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
    lam v6.
      create
        n20
        (lam #var"86".
           v6)
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
let eqSeq: all a102. all b34. (a102 -> b34 -> Bool) -> [a102] -> [b34] -> Bool =
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
                  ([ h12 ] ++ t1101 ++ "", [ h22 ] ++ t2101 ++ "")
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
                      t2101
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
      (lam i19.
         get
           seq34
           i19)
in
let toList =
  lam seq33.
    createList
      (length
         seq33)
      (lam i18.
         get
           seq33
           i18)
in
let mapOption: all a100. all b32. (a100 -> Option b32) -> [a100] -> [b32] =
  lam f42.
    recursive
      let work14 =
        lam as3.
          match
            as3
          with
            [ a101 ] ++ as4 ++ ""
          then
            match
              f42
                a101
            with
              Some b33
            then
              cons
                b33
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
let for_: all a99. [a99] -> (a99 -> ()) -> () =
  lam xs9.
    lam f41.
      iter
        f41
        xs9
in
let mapReverse =
  lam f40.
    lam lst1.
      foldl
        (lam acc73.
           lam x69.
             cons
               (f40
                  x69)
               acc73)
        (toList
           "")
        lst1
in
let mapK: all a98. all b31. all c36. (a98 -> (b31 -> c36) -> c36) -> [a98] -> ([b31] -> c36) -> c36 =
  lam f39.
    lam seq32.
      lam k2.
        foldl
          (lam k3.
             lam x67.
               lam xs8.
                 f39
                   x67
                   (lam x68.
                      k3
                        (cons
                           x68
                           xs8)))
          k2
          seq32
          ""
in
let foldl1 =
  lam f38.
    lam l19.
      foldl
        f38
        (head
           l19)
        (tail
           l19)
in
let foldr1 =
  lam f37.
    lam seq31.
      foldr
        f37
        (last
           seq31)
        (init
           seq31)
in
recursive
  let unfoldr: all a97. all c35. (a97 -> Option (c35, a97)) -> a97 -> [c35] =
    lam f36.
      lam b0.
        let fb =
          f36
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
            Some (x66, b110)
          in
          cons
              x66
              (unfoldr
                 f36
                 b110)
in
let range =
  lam s60.
    lam e6.
      lam by.
        unfoldr
          (lam b30.
             match
               leqi
                 e6
                 b30
             with
               true
             then
               None
                 {}
             else
               Some
                 (b30, addi
                   b30
                   by))
          s60
in
recursive
  let foldl21: all a96. all b29. all c34. (a96 -> b29 -> c34 -> a96) -> a96 -> [b29] -> [c34] -> a96 =
    lam f35.
      lam acc68.
        lam seq112.
          lam seq210.
            let g1 =
              lam acc71: (a96, [b29]).
                lam x213.
                  match
                    acc71
                  with
                    (acc72, [ x113 ] ++ xs11 ++ "")
                  in
                  (f35
                      acc72
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
                  (acc68, seq112)
                  seq210
              with
                (acc69, _)
              in
              acc69
            else
              foldl21
                (lam acc70.
                   lam x112.
                     lam x212.
                       f35
                         acc70
                         x212
                         x112)
                acc68
                seq210
                seq112
in
let foldli: all a95. all b28. (a95 -> Int -> b28 -> a95) -> a95 -> [b28] -> a95 =
  lam fn.
    lam initAcc.
      lam seq30.
        recursive
          let work13 =
            lam acc67.
              lam i17.
                lam s59.
                  match
                    s59
                  with
                    [ e5 ] ++ rest ++ ""
                  then
                    work13
                      (fn
                         acc67
                         i17
                         e5)
                      (addi
                         i17
                         1)
                      rest
                  else
                    acc67
        in
        work13
          initAcc
          0
          seq30
in
let zipWith: all a94. all b27. all c33. (a94 -> b27 -> c33) -> [a94] -> [b27] -> [c33] =
  lam f34.
    foldl21
      (lam acc66.
         lam x111.
           lam x211.
             snoc
               acc66
               (f34
                  x111
                  x211))
      ""
in
let zipWithIndex: all a93. all b26. all c32. (Int -> a93 -> b26 -> c32) -> [a93] -> [b26] -> [c32] =
  lam f33.
    lam a110.
      lam a210.
        recursive
          let work12 =
            lam acc65.
              lam i16.
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
                             (f33
                                i16
                                e11
                                e21)
                             acc65)
                          (addi
                             i16
                             1)
                          seq1tail
                          seq2tail
                      else
                        reverse
                          acc65
                    else
                      reverse
                        acc65
        in
        work12
          (toList
             "")
          0
          a110
          a210
in
let zip: all a92. all b25. [a92] -> [b25] -> [(a92, b25)] =
  zipWith
    (lam x65.
       lam y10.
         (x65, y10))
in
let mapAccumL: all a91. all b24. all c31. (a91 -> b24 -> (a91, c31)) -> a91 -> [b24] -> (a91, [c31]) =
  lam f32: a91 -> b24 -> (a91, c31).
    lam acc63.
      lam seq28.
        foldl
          (lam tacc1: (a91, [c31]).
             lam x64.
               match
                 f32
                   (tacc1.0)
                   x64
               with
                 (acc64, y9)
               in
               (acc64, snoc
                   (tacc1.1)
                   y9))
          (acc63, "")
          seq28
in
let mapAccumR: all a90. all b23. all c30. (a90 -> b23 -> (a90, c30)) -> a90 -> [b23] -> (a90, [c30]) =
  lam f31: a90 -> b23 -> (a90, c30).
    lam acc61.
      lam seq27.
        foldr
          (lam x63.
             lam tacc: (a90, [c30]).
               match
                 f31
                   (tacc.0)
                   x63
               with
                 (acc62, y8)
               in
               (acc62, cons
                   y8
                   (tacc.1)))
          (acc61, "")
          seq27
in
let unzip: all a89. all b22. [(a89, b22)] -> ([a89], [b22]) =
  mapAccumL
    (lam l18.
       lam p18: (a89, b22).
         (snoc
           l18
           (p18.0), p18.1))
    ""
in
let iter2: all a88. all b21. (a88 -> b21 -> ()) -> [a88] -> [b21] -> () =
  lam f29.
    lam seq110.
      lam seq26.
        let f30 =
          lam x62: (a88, b21).
            match
              x62
            with
              (x110, x210)
            in
            f29
                x110
                x210
        in
        iter
          f30
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
let seqLiftA2: all a86. all b20. all c29. (a86 -> b20 -> c29) -> [a86] -> [b20] -> [c29] =
  lam f28.
    lam as2.
      lam bs.
        join
          (map
             (lam a87.
                map
                  (f28
                     a87)
                  bs)
             as2)
in
let seqMapM: all a84. all b19. (a84 -> [b19]) -> [a84] -> [[b19]] =
  lam f27.
    foldr
      (lam a85.
         lam acc60.
           seqLiftA2
             cons
             (f27
                a85)
             acc60)
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
  let filterOption: all a83. [Option a83] -> [a83] =
    lam optSeq.
      match
        optSeq
      with
        [ Some x61 ] ++ optSeq1 ++ ""
      then
        cons
          x61
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
  let findMap: all a82. all b18. (a82 -> Option b18) -> [a82] -> Option b18 =
    lam f26.
      lam seq20.
        match
          seq20
        with
          [ h5 ] ++ t457 ++ ""
        then
          match
            f26
              h5
          with
            Some x60
          then
            Some
              x60
          else
            findMap
              f26
              t457
        else
          None
            {}
in
let lowerBoundBinarySearch: all a81. (a81 -> Int) -> [a81] -> Option Int =
  lam f25.
    lam s58.
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
                    (f25
                       (get
                          s58
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
             s58)
      in
      match
        eqi
          idx2
          (length
             s58)
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
          lam l17.
            lam r15.
              lam seq18.
                match
                  seq18
                with
                  ""
                then
                  (l17, r15)
                else
                  match
                    seq18
                  with
                    [ s57 ] ++ seq19 ++ ""
                  in
                  match
                      p13
                        s57
                    with
                      true
                    then
                      work10
                        (cons
                           s57
                           l17)
                        r15
                        seq19
                    else
                      work10
                        l17
                        (cons
                           s57
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
                [ h4 ] ++ t456 ++ ""
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
                    t456
                    seq21
                else
                  cons
                    h4
                    (work9
                       t456
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
    lam s55.
      recursive
        let work8 =
          lam acc59.
            lam s56.
              match
                s56
              with
                [ h11 ] ++ t455 ++ ""
              then
                match
                  acc59
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
                      acc59
                      t455
                  else
                    work8
                      (cons
                         h11
                         acc59)
                      t455
                else
                  work8
                    [ h11 ]
                    t455
              else
                acc59
      in
      reverse
        (work8
           ""
           s55)
in
recursive
  let quickSort: all a80. (a80 -> a80 -> Int) -> [a80] -> [a80] =
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
          let t454 =
            tail
              seq14
          in
          let lr1 =
            partition
              (lam x59.
                 lti
                   (cmp12
                      x59
                      h3)
                   0)
              t454
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
      lam l16.
        lam r14.
          match
            l16
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
              l16
            else
              match
                (l16, r14)
              with
                ([ x58 ] ++ xs7 ++ "", [ y7 ] ++ ys1 ++ "")
              in
              match
                  leqi
                    (cmp11
                       x58
                       y7)
                    0
                with
                  true
                then
                  cons
                    x58
                    (merge
                       cmp11
                       xs7
                       r14)
                else
                  cons
                    y7
                    (merge
                       cmp11
                       l16
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
            [ x57 ]
          then
            [ x57 ]
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
let minIdx: all a79. (a79 -> a79 -> Int) -> [a79] -> Option (Int, a79) =
  lam cmp9: a79 -> a79 -> Int.
    lam seq12: [a79].
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
            (lam acc58: (Int, Int, a79).
               lam e4: a79.
                 match
                   acc58
                 with
                   (curi, mini1, m6)
                 in
                 match
                     lti
                       (cmp9
                          m6
                          e4)
                       0
                   with
                     true
                   then
                     (addi
                       curi
                       1, mini1, m6)
                   else
                     (addi
                       curi
                       1, curi, e4))
            (1, 0, head
              seq12)
            (tail
               seq12)
        with
          (_, i15, m7)
        in
        Some
            (i15, m7)
in
let min: all a78. (a78 -> a78 -> Int) -> [a78] -> Option a78 =
  lam cmp8.
    lam seq11.
      optionMap
        (lam r13.
           match
             r13
           with
             (_, m5)
           in
           m5)
        (minIdx
           cmp8
           seq11)
in
let max =
  lam cmp7.
    min
      (lam l15.
         lam r12.
           cmp7
             r12
             l15)
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
        (lam l14.
           lam r11.
             cmp5
               r11
               l14)
in
let index =
  lam pred3.
    lam seq8.
      recursive
        let index_rechelper =
          lam i14.
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
                      i14
                  else
                    index_rechelper
                      (addi
                         i14
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
          lam i13.
            lam acc57.
              lam pred2.
                lam seq7.
                  match
                    null
                      seq7
                  with
                    true
                  then
                    acc57
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
                           i13
                           1)
                        (Some
                           i13)
                        pred2
                        (tail
                           seq7)
                    else
                      lastIndex_rechelper
                        (addi
                           i13
                           1)
                        acc57
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
let seqCmp: all a77. (a77 -> a77 -> Int) -> [a77] -> [a77] -> Int =
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
                  ([ h1 ] ++ t1100 ++ "", [ h2 ] ++ t2100 ++ "")
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
                      t2100
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
let randIndex: all a76. [a76] -> Option Int =
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
let randElem: all a75. [a75] -> Option a75 =
  lam seq4.
    optionMap
      (get
         seq4)
      (randIndex
         seq4)
in
let permute: all a74. [a74] -> [Int] -> [a74] =
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
            (lam x56: (a74, Int).
               lam y6: (a74, Int).
                 subi
                   (x56.1)
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
  lam x55.
    x55
in
let const =
  lam x54.
    lam #var"85".
      x54
in
let apply =
  lam f24.
    lam x53.
      f24
        x53
in
let compose =
  lam f23.
    lam g.
      lam x52.
        f23
          (g
             x52)
in
let curry =
  lam f22.
    lam x51.
      lam y5.
        f22
          (x51, y5)
in
let uncurry: all a73. all b17. all c20. (a73 -> b17 -> c20) -> (a73, b17) -> c20 =
  lam f21.
    lam t453: (a73, b17).
      f21
        (t453.0)
        (t453.1)
in
let flip =
  lam f20.
    lam x50.
      lam y4.
        f20
          y4
          x50
in
let printLn =
  lam s54.
    let #var"84" =
      print
        (concat
           s54
           "\n")
    in
    flushStdout
      {}
in
let printSeq =
  lam s53.
    print
      (join
         s53)
in
let printSeqLn =
  lam s52.
    let #var"82" =
      printSeq
        s52
    in
    let #var"83" =
      print
        "\n"
    in
    flushStdout
      {}
in
let dprintLn =
  lam x49.
    let #var"81" =
      dprint
        x49
    in
    printLn
      ""
in
recursive
  let fix: all a72. all b16. ((a72 -> b16) -> a72 -> b16) -> a72 -> b16 =
    lam f19.
      lam e3.
        f19
          (fix
             f19)
          e3
in
let repeat: (() -> ()) -> Int -> () =
  lam f18.
    lam n17.
      recursive
        let rec14 =
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
              let #var"80" =
                f18
                  {}
              in
              rec14
                (subi
                   n18
                   1)
      in
      rec14
        n17
in
let repeati: (Int -> ()) -> Int -> () =
  lam f17.
    lam n16.
      recursive
        let rec13 =
          lam i12.
            match
              geqi
                i12
                n16
            with
              true
            then
              {}
            else
              let #var"79" =
                f17
                  i12
              in
              rec13
                (addi
                   i12
                   1)
      in
      rec13
        0
in
let fixMutual: all a71. all b15. [[a71 -> b15] -> a71 -> b15] -> [a71 -> b15] =
  lam l11.
    let l12 =
      map
        (lam li1.
           (li1,))
        l11
    in
    fix
      (lam self.
         lam l13.
           map
             (lam li: ([a71 -> b15] -> a71 -> b15,).
                lam x48.
                  (li.0)
                    (self
                       l13)
                    x48)
             l13)
      l12
in
let maxf: Float -> Float -> Float =
  lam r10.
    lam l10.
      match
        gtf
          r10
          l10
      with
        true
      then
        r10
      else
        l10
in
let absf: Float -> Float =
  lam f16.
    maxf
      f16
      (negf
         f16)
in
let eqfApprox =
  lam epsilon1.
    lam r9.
      lam l9.
        match
          leqf
            (absf
               (subf
                  r9
                  l9))
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
  lam x47: Float.
    externalExp
      x47
in
external externalLog : Float -> Float
in
let log =
  lam x46: Float.
    externalLog
      x46
in
external externalAtan : Float -> Float
in
let atan =
  lam x45: Float.
    externalAtan
      x45
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
  lam x44: Float.
    externalSin
      x44
in
external externalCos : Float -> Float
in
let cos =
  lam x43: Float.
    externalCos
      x43
in
external externalAtan2 : Float -> Float -> Float
in
let atan2 =
  lam x42: Float.
    lam y3: Float.
      externalAtan2
        x42
        y3
in
external externalPow : Float -> Float -> Float
in
let pow =
  lam x41: Float.
    lam y2: Float.
      externalPow
        x41
        y2
in
external externalSqrt : Float -> Float
in
let sqrt: Float -> Float =
  lam x40.
    externalSqrt
      x40
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
    lam l8.
      match
        ltf
          r8
          l8
      with
        true
      then
        r8
      else
        l8
in
let cmpfApprox: Float -> Float -> Float -> Int =
  lam epsilon.
    lam l7.
      lam r7.
        match
          eqfApprox
            epsilon
            l7
            r7
        with
          true
        then
          0
        else
          match
            ltf
              l7
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
        lam acc56.
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
                   acc56)
                (subi
                   n15
                   1)
            else
              acc56
    in
    work6
      0.
      n14
in
let maxi =
  lam r6.
    lam l6.
      match
        gti
          r6
          l6
      with
        true
      then
        r6
      else
        l6
in
let mini =
  lam r5.
    lam l5.
      match
        lti
          r5
          l5
      with
        true
      then
        r5
      else
        l5
in
let absi =
  lam i11.
    maxi
      i11
      (negi
         i11)
in
let succ =
  lam x39.
    addi
      x39
      1
in
let pred =
  lam x38.
    subi
      x38
      1
in
external externalGammaLogPdf : Float -> Float -> Float -> Float
in
external externalGammaSample! : Float -> Float -> Float
in
let gammaPdf =
  lam shape2: Float.
    lam scale2: Float.
      lam x37: Float.
        exp
          (externalGammaLogPdf
             x37
             shape2
             scale2)
in
let gammaLogPdf =
  lam shape1: Float.
    lam scale1: Float.
      lam x36: Float.
        externalGammaLogPdf
          x36
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
      lam x35: Int.
        exp
          (externalBinomialLogPmf
             x35
             p12
             n13)
in
let binomialLogPmf =
  lam p11: Float.
    lam n12: Int.
      lam x34: Int.
        externalBinomialLogPmf
          x34
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
    lam x33: Bool.
      match
        x33
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
    lam x32: Bool.
      log
        (bernoulliPmf
           p8
           x32)
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
  lam a70: Float.
    lam b14: Float.
      lam x31: Float.
        exp
          (externalBetaLogPdf
             x31
             a70
             b14)
in
let betaLogPdf =
  lam a69: Float.
    lam b13: Float.
      lam x30: Float.
        externalBetaLogPdf
          x30
          a69
          b13
in
let betaSample =
  lam a68: Float.
    lam b12: Float.
      externalBetaSample
        a68
        b12
in
external externalGaussianLogPdf : Float -> Float -> Float -> Float
in
external externalGaussianSample! : Float -> Float -> Float
in
let gaussianPdf =
  lam mu2: Float.
    lam sigma6: Float.
      lam x29: Float.
        exp
          (externalGaussianLogPdf
             x29
             mu2
             sigma6)
in
let gaussianLogPdf =
  lam mu1: Float.
    lam sigma5: Float.
      lam x28: Float.
        externalGaussianLogPdf
          x28
          mu1
          sigma5
in
let gaussianSample =
  lam mu: Float.
    lam sigma4: Float.
      externalGaussianSample
        mu
        sigma4
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
    lam x27.
      log
        (get
           ps3
           x27)
in
let categoricalPmf: [Float] -> Int -> Float =
  lam ps2.
    lam x26.
      get
        ps2
        x26
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
  lam a67.
    lam b11.
      externalUniformContinuousSample
        a67
        b11
in
let uniformContinuousLogPdf =
  lam a66.
    lam b10.
      lam x25.
        match
          geqf
            x25
            a66
        with
          true
        then
          match
            leqf
              x25
              b10
          with
            true
          then
            subf
              (log
                 1.)
              (log
                 (subf
                    b10
                    a66))
          else
            0.
        else
          0.
in
let uniformContinuousPdf =
  lam a65.
    lam b9.
      lam x24.
        match
          geqf
            x24
            a65
        with
          true
        then
          match
            leqf
              x24
              b9
          with
            true
          then
            divf
              1.
              (subf
                 b9
                 a65)
          else
            0.
        else
          0.
in
let uniformSample: () -> Float =
  lam #var"78".
    uniformContinuousSample
      0.
      1.
in
external externalUniformDiscreteSample! : Int -> Int -> Int
in
let uniformDiscreteSample =
  lam a64: Int.
    lam b8: Int.
      externalUniformDiscreteSample
        a64
        b8
in
let uniformDiscreteLogPdf: Int -> Int -> Int -> Float =
  lam a63.
    lam b7.
      lam x23.
        match
          geqi
            x23
            a63
        with
          true
        then
          match
            leqi
              x23
              b7
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
                          b7
                          a63))))
          else
            0.
        else
          0.
in
let uniformDiscretePdf: Int -> Int -> Int -> Float =
  lam a62.
    lam b6.
      lam x22.
        match
          geqi
            x22
            a62
        with
          true
        then
          match
            leqi
              x22
              b6
          with
            true
          then
            divf
              1.
              (int2float
                 (addi
                    1
                    (subi
                       b6
                       a62)))
          else
            0.
        else
          0.
in
let poissonLogPmf =
  lam lambda5: Float.
    lam x21: Int.
      subf
        (subf
           (mulf
              (int2float
                 x21)
              (log
                 lambda5))
           lambda5)
        (logFactorial
           x21)
in
let poissonPmf =
  lam lambda4: Float.
    lam x20: Int.
      exp
        (poissonLogPmf
           lambda4
           x20)
in
let poissonSample =
  lam lambda3: Float.
    let enlam =
      exp
        (negf
           lambda3)
    in
    let x18 =
      0
    in
    let prod =
      1.
    in
    recursive
      let rec12 =
        lam x19.
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
              rec12
                (addi
                   x19
                   1)
                prod2
            else
              x19
    in
    rec12
      x18
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
    lam x17.
      subf
        (log
           lambda1)
        (mulf
           lambda1
           x17)
in
let exponentialPdf: Float -> Float -> Float =
  lam lambda.
    lam x16.
      exp
        (exponentialLogPdf
           lambda
           x16)
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
    let i10 =
      char2int
        c13
    in
    match
      leqi
        (char2int
           'a')
        i10
    with
      true
    then
      leqi
        i10
        (char2int
           'z')
    else
      false
in
let isUpperAlpha =
  lam c12.
    let i9 =
      char2int
        c12
    in
    match
      leqi
        (char2int
           'A')
        i9
    with
      true
    then
      leqi
        i9
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
    let i8 =
      char2int
        c8
    in
    match
      leqi
        (char2int
           '0')
        i8
    with
      true
    then
      leqi
        i8
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
  lam #var"77".
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
  lam s51.
    join
      (map
         escapeChar
         s51)
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
              lam i7.
                match
                  eqi
                    i7
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
                         i7)
                      (get
                         s212
                         (addi
                            o2
                            i7))
                  with
                    true
                  then
                    work5
                      (addi
                         i7
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
  lam s50.
    map
      char2upper
      s50
in
let str2lower =
  lam s49.
    map
      char2lower
      s49
in
let string2int =
  lam s47.
    recursive
      let string2int_rechelper =
        lam s48.
          lam acc55.
            match
              null
                s48
            with
              true
            then
              acc55
            else
              let fsd =
                subi
                  (char2int
                     (head
                        s48))
                  (char2int
                     '0')
              in
              string2int_rechelper
                (tail
                   s48)
                (addi
                   (muli
                      10
                      acc55)
                   fsd)
    in
    match
      s47
    with
      ""
    then
      0
    else
      match
        eqChar
          '-'
          (head
             s47)
      with
        true
      then
        negi
          (string2int_rechelper
             (tail
                s47)
             0)
      else
        string2int_rechelper
          s47
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
          lam acc54.
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
                acc54
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
                   acc54)
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
  lam s46.
    eqString
      s46
      (int2string
         (string2int
            s46))
in
let strIndex =
  lam c5.
    lam s44.
      recursive
        let strIndex_rechelper =
          lam i6.
            lam c6.
              lam s45.
                match
                  null
                    s45
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
                         s45)
                  with
                    true
                  then
                    Some
                      i6
                  else
                    strIndex_rechelper
                      (addi
                         i6
                         1)
                      c6
                      (tail
                         s45)
      in
      strIndex_rechelper
        0
        c5
        s44
in
let strLastIndex =
  lam c3.
    lam s42.
      recursive
        let strLastIndex_rechelper =
          lam i5.
            lam acc53.
              lam c4.
                lam s43.
                  match
                    null
                      s43
                  with
                    true
                  then
                    match
                      eqi
                        acc53
                        (negi
                           1)
                    with
                      true
                    then
                      None
                        {}
                    else
                      Some
                        acc53
                  else
                    match
                      eqChar
                        c4
                        (head
                           s43)
                    with
                      true
                    then
                      strLastIndex_rechelper
                        (addi
                           i5
                           1)
                        i5
                        c4
                        (tail
                           s43)
                    else
                      strLastIndex_rechelper
                        (addi
                           i5
                           1)
                        acc53
                        c4
                        (tail
                           s43)
      in
      strLastIndex_rechelper
        0
        (negi
           1)
        c3
        s42
in
let strSplit =
  lam delim1.
    lam s41.
      let n5 =
        length
          s41
      in
      let m4 =
        length
          delim1
      in
      recursive
        let work4 =
          lam acc52.
            lam lastMatch.
              lam i4.
                match
                  lti
                    (subi
                       n5
                       m4)
                    i4
                with
                  true
                then
                  snoc
                    acc52
                    (subsequence
                       s41
                       lastMatch
                       n5)
                else
                  match
                    eqStringSlice
                      delim1
                      s41
                      i4
                      m4
                  with
                    true
                  then
                    let nexti =
                      addi
                        i4
                        m4
                    in
                    work4
                      (snoc
                         acc52
                         (subsequence
                            s41
                            lastMatch
                            (subi
                               i4
                               lastMatch)))
                      nexti
                      nexti
                  else
                    work4
                      acc52
                      lastMatch
                      (addi
                         i4
                         1)
      in
      match
        null
          delim1
      with
        true
      then
        [ s41 ]
      else
        work4
          ""
          0
          0
in
let strTrim =
  lam s39.
    recursive
      let strTrim_init =
        lam s40.
          match
            eqString
              s40
              ""
          with
            true
          then
            s40
          else
            match
              isWhitespace
                (head
                   s40)
            with
              true
            then
              strTrim_init
                (tail
                   s40)
            else
              s40
    in
    reverse
      (strTrim_init
         (reverse
            (strTrim_init
               s39)))
in
let stringIsInt1 =
  lam s37.
    match
      null
        s37
    with
      true
    then
      false
    else
      let s38 =
        match
          eqChar
            (get
               s37
               0)
            '-'
        with
          true
        then
          tail
            s37
        else
          s37
      in
      forAll
        isDigit
        s38
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
  lam s36.
    match
      fileExists
        s36
    with
      true
    then
      deleteFile
        s36
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
    lam s35.
      writeString
        c2
        s35
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
      (s34, false)
    then
      Some
        s34
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
  lam #var"75".
    match
      neqi
        (length
           argv)
        2
    with
      true
    then
      let #var"76" =
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
  lam res20.
    lam names4.
      lam filename.
        lam expOnLogWeights1.
          match
            writeOpen1
              filename
          with
            Some ch
          then
            let #var"71" =
              writeString1
                ch
                (strJoin
                   ","
                   names4)
            in
            let #var"72" =
              writeString1
                ch
                "\n"
            in
            let #var"73" =
              iter
                (lam lst.
                   let #var"74" =
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
                   res20)
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
  lam res19.
    lam names2.
      lam normConst3.
        lam expVals2.
          lam varianceVals1.
            let pad =
              18
            in
            let padPrint =
              lam s33.
                lam n4.
                  match
                    geqi
                      n4
                      (length
                         s33)
                  with
                    true
                  then
                    let #var"69" =
                      print
                        s33
                    in
                    print
                      (create
                         (subi
                            n4
                            (length
                               s33))
                         (lam #var"70".
                            ' '))
                  else
                    print
                      s33
            in
            let #var"57" =
              padPrint
                "Variable"
                14
            in
            let #var"58" =
              padPrint
                "Expected Value"
                pad
            in
            let #var"59" =
              padPrint
                "Variance"
                pad
            in
            let #var"60" =
              padPrint
                "Standard Deviation"
                pad
            in
            let #var"61" =
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
                        ([ n3 ] ++ ns4 ++ "", [ e1 ] ++ es1 ++ "", [ v5 ] ++ vs ++ "")
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
                          let #var"64" =
                            padPrint
                              n3
                              14
                          in
                          let #var"65" =
                            padPrint
                              (float2string
                                 e1)
                              pad
                          in
                          let #var"66" =
                            padPrint
                              (float2string
                                 v5)
                              pad
                          in
                          let #var"67" =
                            padPrint
                              (float2string
                                 (sqrt
                                    v5))
                              pad
                          in
                          let #var"68" =
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
            let #var"62" =
              work3
                names2
                expVals2
                varianceVals1
            in
            let #var"63" =
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
let systematicSample: all a61. [a61] -> [Float] -> Float -> Int -> [a61] =
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
  lam res18.
    let max1 =
      foldl
        (lam acc51.
           lam x15.
             match
               geqf
                 x15
                 acc51
             with
               true
             then
               x15
             else
               acc51)
        negInf
        res18
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
          (lam acc50.
             lam x14.
               addf
                 (exp
                    (subf
                       x14
                       max1))
                 acc50)
          0.
          res18
      in
      subf
        (addf
           max1
           (log
              sum1))
        (log
           (int2float
              (length
                 res18)))
in
let expectedValues =
  lam res17: [[Float]].
    lam normConst2.
      foldl
        (lam acc48.
           lam t452.
             let w3 =
               exp
                 (subf
                    (head
                       t452)
                    normConst2)
             in
             let ys =
               tail
                 t452
             in
             recursive
               let work2 =
                 lam acc49.
                   lam xs3.
                     match
                       (acc49, xs3)
                     with
                       ([ a60 ] ++ as1 ++ "", [ x13 ] ++ xs4 ++ "")
                     then
                       cons
                         (addf
                            (mulf
                               x13
                               w3)
                            a60)
                         (work2
                            as1
                            xs4)
                     else
                       ""
             in
             work2
               acc48
               ys)
        (create
           (subi
              (length
                 (head
                    res17))
              1)
           (lam #var"56".
              0.))
        res17
in
let variance =
  lam res16.
    lam expVals1.
      let sum =
        foldl
          (lam acc46.
             lam t451.
               recursive
                 let work1 =
                   lam acc47.
                     lam xs1.
                       lam expv.
                         match
                           (acc47, xs1, expv)
                         with
                           ([ a59 ] ++ as ++ "", [ x12 ] ++ xs2 ++ "", [ e ] ++ es ++ "")
                         then
                           let v4 =
                             subf
                               x12
                               e
                           in
                           cons
                             (addf
                                a59
                                (mulf
                                   v4
                                   v4))
                             (work1
                                as
                                xs2
                                es)
                         else
                           ""
               in
               work1
                 acc46
                 (tail
                    t451)
                 expVals1)
          (create
             (subi
                (length
                   (head
                      res16))
                1)
             (lam #var"55".
                0.))
          res16
      in
      let dval =
        int2float
          (length
             res16)
      in
      map
        (lam x11.
           divf
             x11
             dval)
        sum
in
let expOnLogWeights =
  lam res15.
    mapReverse
      (lam t450.
         match
           t450
         with
           [ x10 ] ++ xs ++ ""
         in
         cons
             (exp
                x10)
             xs)
      res15
in
let output =
  lam res14: [[Float]].
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
             res14)
      in
      let expVals =
        expectedValues
          res14
          nc
      in
      let varianceVals =
        variance
          res14
          expVals
      in
      let #var"54" =
        printStatistics
          res14
          names1
          nc
          expVals
          varianceVals
      in
      saveCSV
        res14
        names1
        "data.csv"
        expOnLogWeights
in
let printSamples: all a58. (a58 -> [Char]) -> [Float] -> [a58] -> () =
  lam printFun1.
    lam weights4.
      lam samples10.
        recursive
          let rec11: [Float] -> [a58] -> () =
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
                  let s32 =
                    head
                      samples11
                  in
                  let samples12 =
                    tail
                      samples11
                  in
                  let #var"50" =
                    print
                      (printFun1
                         s32)
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
                  rec11
                    weights6
                    samples12
        in
        match
          compileOptions.printSamples
        with
          true
        then
          rec11
            weights4
            samples10
        else
          {}
in
let printSamplesOption: all a57. (a57 -> [Char]) -> [Float] -> [Option a57] -> () =
  lam printFun.
    lam weights1.
      lam samples7.
        recursive
          let rec10: [Float] -> [Option a57] -> () =
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
                  let s30 =
                    head
                      samples8
                  in
                  let samples9 =
                    tail
                      samples8
                  in
                  let #var"46" =
                    match
                      s30
                    with
                      Some s31
                    then
                      print
                        (printFun
                           s31)
                    else
                      print
                        "."
                  in
                  let #var"47" =
                    print
                      " "
                  in
                  let #var"48" =
                    print
                      (float2string
                         w1)
                  in
                  let #var"49" =
                    print
                      "\n"
                  in
                  rec10
                    weights3
                    samples9
        in
        match
          compileOptions.printSamples
        with
          true
        then
          rec10
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
    let #var"45" =
      modref
        _mcmcSamples
        n2
    in
    modref
      _mcmcAccepts
      0
in
let mcmcAccept =
  lam #var"44".
    modref
      _mcmcAccepts
      (addi
         (deref
            _mcmcAccepts)
         1)
in
let mcmcAcceptRate =
  lam #var"43".
    divf
      (int2float
         (deref
            _mcmcAccepts))
      (int2float
         (deref
            _mcmcSamples))
in
recursive
  let #var"RuntimeDistBase_sample": all a55. Dist a55 -> a55 =
    lam __sem_target20.
      let _20 =
        dprint
          __sem_target20
      in
      error
        "No matching case for function sample"
  let #var"RuntimeDistBase_logObserve": all a56. Dist a56 -> a56 -> Float =
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
  let #var"RuntimeDistElementary_sample": all a53. Dist a53 -> a53 =
    lam __sem_target18.
      match
        __sem_target18
      with
        RuntimeDistElementary_DistGamma t428
      then
        unsafeCoerce
          (gammaSample
             (t428.shape)
             (t428.scale))
      else
        match
          __sem_target18
        with
          RuntimeDistElementary_DistExponential t429
        then
          unsafeCoerce
            (exponentialSample
               (t429.rate))
        else
          match
            __sem_target18
          with
            RuntimeDistElementary_DistPoisson t430
          then
            unsafeCoerce
              (poissonSample
                 (t430.lambda))
          else
            match
              __sem_target18
            with
              RuntimeDistElementary_DistBinomial t431
            then
              unsafeCoerce
                (binomialSample
                   (t431.p)
                   (t431.n))
            else
              match
                __sem_target18
              with
                RuntimeDistElementary_DistBernoulli t432
              then
                unsafeCoerce
                  (bernoulliSample
                     (t432.p))
              else
                match
                  __sem_target18
                with
                  RuntimeDistElementary_DistBeta t433
                then
                  unsafeCoerce
                    (betaSample
                       (t433.a)
                       (t433.b))
                else
                  match
                    __sem_target18
                  with
                    RuntimeDistElementary_DistGaussian t434
                  then
                    unsafeCoerce
                      (gaussianSample
                         (t434.mu)
                         (t434.sigma))
                  else
                    match
                      __sem_target18
                    with
                      RuntimeDistElementary_DistMultinomial t435
                    then
                      unsafeCoerce
                        (multinomialSample
                           (t435.p)
                           (t435.n))
                    else
                      match
                        __sem_target18
                      with
                        RuntimeDistElementary_DistCategorical t436
                      then
                        unsafeCoerce
                          (categoricalSample
                             (t436.p))
                      else
                        match
                          __sem_target18
                        with
                          RuntimeDistElementary_DistDirichlet t437
                        then
                          unsafeCoerce
                            (dirichletSample
                               (t437.a))
                        else
                          match
                            __sem_target18
                          with
                            RuntimeDistElementary_DistUniform t438
                          then
                            unsafeCoerce
                              (uniformContinuousSample
                                 (t438.a)
                                 (t438.b))
                          else
                            let _18 =
                              dprint
                                __sem_target18
                            in
                            error
                              "No matching case for function sample"
  let #var"RuntimeDistElementary_logObserve": all a54. Dist a54 -> a54 -> Float =
    lam __sem_target19.
      match
        __sem_target19
      with
        RuntimeDistElementary_DistGamma t439
      then
        unsafeCoerce
          (gammaLogPdf
             (t439.shape)
             (t439.scale))
      else
        match
          __sem_target19
        with
          RuntimeDistElementary_DistExponential t440
        then
          unsafeCoerce
            (exponentialLogPdf
               (t440.rate))
        else
          match
            __sem_target19
          with
            RuntimeDistElementary_DistPoisson t441
          then
            unsafeCoerce
              (poissonLogPmf
                 (t441.lambda))
          else
            match
              __sem_target19
            with
              RuntimeDistElementary_DistBinomial t442
            then
              unsafeCoerce
                (binomialLogPmf
                   (t442.p)
                   (t442.n))
            else
              match
                __sem_target19
              with
                RuntimeDistElementary_DistBernoulli t443
              then
                unsafeCoerce
                  (bernoulliLogPmf
                     (t443.p))
              else
                match
                  __sem_target19
                with
                  RuntimeDistElementary_DistBeta t444
                then
                  unsafeCoerce
                    (betaLogPdf
                       (t444.a)
                       (t444.b))
                else
                  match
                    __sem_target19
                  with
                    RuntimeDistElementary_DistGaussian t445
                  then
                    unsafeCoerce
                      (gaussianLogPdf
                         (t445.mu)
                         (t445.sigma))
                  else
                    match
                      __sem_target19
                    with
                      RuntimeDistElementary_DistMultinomial t446
                    then
                      unsafeCoerce
                        (lam o1.
                           match
                             eqi
                               (t446.n)
                               (foldl1
                                  addi
                                  o1)
                           with
                             true
                           then
                             multinomialLogPmf
                               (t446.p)
                               o1
                           else
                             negf
                               inf)
                    else
                      match
                        __sem_target19
                      with
                        RuntimeDistElementary_DistCategorical t447
                      then
                        unsafeCoerce
                          (categoricalLogPmf
                             (t447.p))
                      else
                        match
                          __sem_target19
                        with
                          RuntimeDistElementary_DistDirichlet t448
                        then
                          unsafeCoerce
                            (dirichletLogPdf
                               (t448.a))
                        else
                          match
                            __sem_target19
                          with
                            RuntimeDistElementary_DistUniform t449
                          then
                            unsafeCoerce
                              (uniformContinuousLogPdf
                                 (t449.a)
                                 (t449.b))
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
  let #var"RuntimeDistEmpirical_sample": all a51. Dist a51 -> a51 =
    lam __sem_target10.
      match
        __sem_target10
      with
        RuntimeDistEmpirical_DistEmpirical t422
      then
        let x8 =
          uniformContinuousSample
            0.
            (last
               (t422.cumulativeWeights))
        in
        let cmp3 =
          lam y1.
            match
              ltf
                (subf
                   y1
                   x8)
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
            (t422.cumulativeWeights)
        with
          Some idx1
        then
          unsafeCoerce
            (get
               (t422.samples)
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
  let #var"RuntimeDistEmpirical_logObserve": all a52. Dist a52 -> a52 -> Float =
    lam __sem_target11.
      match
        __sem_target11
      with
        RuntimeDistEmpirical_DistEmpirical t423
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
        RuntimeDistEmpirical_DistEmpirical t424
      then
        (t424.samples, t424.logWeights)
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
        RuntimeDistEmpirical_DistEmpirical t425
      then
        match
          t425.extra
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
        RuntimeDistEmpirical_DistEmpirical t426
      then
        match
          t426.extra
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
        RuntimeDistEmpirical_DistEmpirical t427
      then
        t427.degenerate
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
                (lam acc45.
                   lam lw6.
                     match
                       geqf
                         lw6
                         acc45
                     with
                       true
                     then
                       lw6
                     else
                       acc45)
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
                      (lam acc44.
                         lam lw5.
                           addf
                             acc44
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
            let f15 =
              lam acc42.
                lam x9.
                  let acc43 =
                    addf
                      acc42
                      (exp
                         x9)
                  in
                  (acc43, acc43)
            in
            match
              mapAccumL
                f15
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
        RuntimeDistCombined_DistCombinedIndependent t420
      then
        unsafeCoerce
          (map
             #var"RuntimeDistCombined_sample"
             (t420.combined))
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
        RuntimeDistCombined_DistCombinedIndependent t421
      then
        unsafeCoerce
          (map
             #var"RuntimeDistCombined_logObserve"
             (t421.combined))
      else
        let _9 =
          dprint
            __sem_target9
        in
        error
          "No matching case for function logObserve"
in
recursive
  let #var"RuntimeDist_sample": all a49. Dist a49 -> a49 =
    lam __sem_target.
      match
        __sem_target
      with
        RuntimeDistCombined_DistCombinedIndependent t390
      then
        unsafeCoerce
          (map
             #var"RuntimeDist_sample"
             (t390.combined))
      else
        match
          __sem_target
        with
          RuntimeDistEmpirical_DistEmpirical t391
        then
          let x6 =
            uniformContinuousSample
              0.
              (last
                 (t391.cumulativeWeights))
          in
          let cmp2 =
            lam y.
              match
                ltf
                  (subf
                     y
                     x6)
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
              (t391.cumulativeWeights)
          with
            Some idx
          then
            unsafeCoerce
              (get
                 (t391.samples)
                 idx)
          else
            error
              "Sampling from empirical distribution failed"
        else
          match
            __sem_target
          with
            RuntimeDistElementary_DistGamma t392
          then
            unsafeCoerce
              (gammaSample
                 (t392.shape)
                 (t392.scale))
          else
            match
              __sem_target
            with
              RuntimeDistElementary_DistExponential t393
            then
              unsafeCoerce
                (exponentialSample
                   (t393.rate))
            else
              match
                __sem_target
              with
                RuntimeDistElementary_DistPoisson t394
              then
                unsafeCoerce
                  (poissonSample
                     (t394.lambda))
              else
                match
                  __sem_target
                with
                  RuntimeDistElementary_DistBinomial t395
                then
                  unsafeCoerce
                    (binomialSample
                       (t395.p)
                       (t395.n))
                else
                  match
                    __sem_target
                  with
                    RuntimeDistElementary_DistBernoulli t396
                  then
                    unsafeCoerce
                      (bernoulliSample
                         (t396.p))
                  else
                    match
                      __sem_target
                    with
                      RuntimeDistElementary_DistBeta t397
                    then
                      unsafeCoerce
                        (betaSample
                           (t397.a)
                           (t397.b))
                    else
                      match
                        __sem_target
                      with
                        RuntimeDistElementary_DistGaussian t398
                      then
                        unsafeCoerce
                          (gaussianSample
                             (t398.mu)
                             (t398.sigma))
                      else
                        match
                          __sem_target
                        with
                          RuntimeDistElementary_DistMultinomial t399
                        then
                          unsafeCoerce
                            (multinomialSample
                               (t399.p)
                               (t399.n))
                        else
                          match
                            __sem_target
                          with
                            RuntimeDistElementary_DistCategorical t400
                          then
                            unsafeCoerce
                              (categoricalSample
                                 (t400.p))
                          else
                            match
                              __sem_target
                            with
                              RuntimeDistElementary_DistDirichlet t401
                            then
                              unsafeCoerce
                                (dirichletSample
                                   (t401.a))
                            else
                              match
                                __sem_target
                              with
                                RuntimeDistElementary_DistUniform t402
                              then
                                unsafeCoerce
                                  (uniformContinuousSample
                                     (t402.a)
                                     (t402.b))
                              else
                                let #var"_" =
                                  dprint
                                    __sem_target
                                in
                                error
                                  "No matching case for function sample"
  let #var"RuntimeDist_logObserve": all a50. Dist a50 -> a50 -> Float =
    lam __sem_target1.
      match
        __sem_target1
      with
        RuntimeDistCombined_DistCombinedIndependent t403
      then
        unsafeCoerce
          (map
             #var"RuntimeDist_logObserve"
             (t403.combined))
      else
        match
          __sem_target1
        with
          RuntimeDistEmpirical_DistEmpirical t404
        then
          error
            "Log observe not supported for empirical distribution"
        else
          match
            __sem_target1
          with
            RuntimeDistElementary_DistGamma t405
          then
            unsafeCoerce
              (gammaLogPdf
                 (t405.shape)
                 (t405.scale))
          else
            match
              __sem_target1
            with
              RuntimeDistElementary_DistExponential t406
            then
              unsafeCoerce
                (exponentialLogPdf
                   (t406.rate))
            else
              match
                __sem_target1
              with
                RuntimeDistElementary_DistPoisson t407
              then
                unsafeCoerce
                  (poissonLogPmf
                     (t407.lambda))
              else
                match
                  __sem_target1
                with
                  RuntimeDistElementary_DistBinomial t408
                then
                  unsafeCoerce
                    (binomialLogPmf
                       (t408.p)
                       (t408.n))
                else
                  match
                    __sem_target1
                  with
                    RuntimeDistElementary_DistBernoulli t409
                  then
                    unsafeCoerce
                      (bernoulliLogPmf
                         (t409.p))
                  else
                    match
                      __sem_target1
                    with
                      RuntimeDistElementary_DistBeta t410
                    then
                      unsafeCoerce
                        (betaLogPdf
                           (t410.a)
                           (t410.b))
                    else
                      match
                        __sem_target1
                      with
                        RuntimeDistElementary_DistGaussian t411
                      then
                        unsafeCoerce
                          (gaussianLogPdf
                             (t411.mu)
                             (t411.sigma))
                      else
                        match
                          __sem_target1
                        with
                          RuntimeDistElementary_DistMultinomial t412
                        then
                          unsafeCoerce
                            (lam o.
                               match
                                 eqi
                                   (t412.n)
                                   (foldl1
                                      addi
                                      o)
                               with
                                 true
                               then
                                 multinomialLogPmf
                                   (t412.p)
                                   o
                               else
                                 negf
                                   inf)
                        else
                          match
                            __sem_target1
                          with
                            RuntimeDistElementary_DistCategorical t413
                          then
                            unsafeCoerce
                              (categoricalLogPmf
                                 (t413.p))
                          else
                            match
                              __sem_target1
                            with
                              RuntimeDistElementary_DistDirichlet t414
                            then
                              unsafeCoerce
                                (dirichletLogPdf
                                   (t414.a))
                            else
                              match
                                __sem_target1
                              with
                                RuntimeDistElementary_DistUniform t415
                              then
                                unsafeCoerce
                                  (uniformContinuousLogPdf
                                     (t415.a)
                                     (t415.b))
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
        RuntimeDistEmpirical_DistEmpirical t416
      then
        (t416.samples, t416.logWeights)
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
        RuntimeDistEmpirical_DistEmpirical t417
      then
        match
          t417.extra
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
        RuntimeDistEmpirical_DistEmpirical t418
      then
        match
          t418.extra
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
        RuntimeDistEmpirical_DistEmpirical t419
      then
        t419.degenerate
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
                (lam acc41.
                   lam lw3.
                     match
                       geqf
                         lw3
                         acc41
                     with
                       true
                     then
                       lw3
                     else
                       acc41)
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
                      (lam acc40.
                         lam lw2.
                           addf
                             acc40
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
            let f14 =
              lam acc38.
                lam x7.
                  let acc39 =
                    addf
                      acc38
                      (exp
                         x7)
                  in
                  (acc39, acc39)
            in
            match
              mapAccumL
                f14
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
let distEmpiricalSamples: all a48. Dist a48 -> ([a48], [Float]) =
  #var"RuntimeDist_empiricalSamples"
in
let distEmpiricalDegenerate: all a47. Dist a47 -> Bool =
  #var"RuntimeDist_empiricalDegenerate"
in
let distEmpiricalNormConst: all a46. Dist a46 -> Float =
  #var"RuntimeDist_empiricalNormConst"
in
let distEmpiricalAcceptRate: all a45. Dist a45 -> Float =
  #var"RuntimeDist_empiricalAcceptRate"
in
let distCombineIndependent: all a44. [Dist a44] -> Dist a44 =
  lam dists.
    RuntimeDistCombined_DistCombinedIndependent
      { combined =
          dists }
in
let sample: all a43. Dist a43 -> a43 =
  #var"RuntimeDist_sample"
in
let logObserve: all a42. Dist a42 -> a42 -> Float =
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
    lam state3.
      modref
        state3
        (addf
           (deref
              state3)
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
let run: all a39. all b5. Unknown -> (State -> (Option (Dist b5), b5 -> Checkpoint a39)) -> Dist a39 =
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
      let state2 =
        ref
          0.
      in
      type Stop a40 =
        {weight: Float, checkpoint: Checkpoint a40}
      in
      let start: (b5 -> Checkpoint a39) -> Float -> (() -> b5) -> Int -> Stop a39 =
        lam cont2.
          lam weight.
            lam sampleFun.
              lam #var"41".
                let #var"42" =
                  modref
                    state2
                    weight
                in
                let checkpoint1: Checkpoint a39 =
                  cont2
                    (sampleFun
                       {})
                in
                { weight =
                    deref
                      state2,
                  checkpoint =
                    checkpoint1 }
      in
      let propagate =
        lam particle.
          lam contWeight1.
            let #var"40" =
              modref
                state2
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
                    state2,
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
                        End a41
                      in
                      a41))
                   particles1)
            else
              let maxWeight =
                foldl
                  (lam acc37.
                     lam p6.
                       match
                         geqf
                           (p6.weight)
                           acc37
                       with
                         true
                       then
                         p6.weight
                       else
                         acc37)
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
                  (lam acc36.
                     lam w.
                       (addf
                         (acc36.0)
                         w, addf
                         (acc36.1)
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
          state2
      with
        (d1, cont1)
      in
      let particles: [Stop a39] =
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
                foldl21
                  (lam acc35.
                     lam s29.
                       lam lw.
                         cons
                           (start
                              cont1
                              lw
                              (lam #var"36".
                                 s29)
                              0)
                           acc35)
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
                     (lam #var"37".
                        #var"RuntimeDist_sample"
                          d2))
            else
              createList
                particleCount
                (start
                   cont1
                   0.
                   (lam #var"38".
                      #var"RuntimeDist_sample"
                        d2))
          else
            createList
              particleCount
              (start
                 cont1
                 0.
                 (lam #var"39".
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
      lam l4.
        lam r2.
          lam seq.
            match
              seq
            with
              ""
            then
              (l4, r2)
            else
              match
                seq
              with
                [ s28 ] ++ seq1 ++ ""
              in
              match
                  p
                    s28
                with
                  true
                then
                  work
                    p
                    (cons
                       s28
                       l4)
                    r2
                    seq1
                else
                  work
                    p
                    l4
                    (cons
                       s28
                       r2)
                    seq1
in
recursive
  let t389 =
    lam cmp1.
      lam h.
        lam x5.
          lti
            (cmp1
               x5
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
type Opaque
in
external externalReadFloatPipe : Int -> [(Timespec, Float)]
in
external externalWriteDistFloatPipe : Int -> (Timespec, ([Float], [Float])) -> ()
in
external externalReadDistFloatRecordPipe : Int -> Int -> [(Timespec, [(Float, Opaque)])]
in
let nanosPerSec =
  1000000000
in
let nanosToTimespec =
  lam nanosPerSec25.
    lam nanos.
      let s27 =
        divi
          nanos
          nanosPerSec25
      in
      let ns3 =
        modi
          nanos
          nanosPerSec25
      in
      (s27, ns3)
in
let timespecToNanos =
  lam nanosPerSec12.
    lam ts.
      match
        ts
      with
        (s1, ns)
      in
      addi
          (muli
             s1
             nanosPerSec12)
          ns
in
let addTimespec =
  lam nanosPerSec24.
    lam lhs2.
      lam rhs2.
        match
          (lhs2, rhs2)
        with
          ((ls2, lns2), (rs2, rns2))
        in
        let s26 =
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
              nanosPerSec24
          with
            true
          then
            (addi
              s26
              1, subi
              ns2
              nanosPerSec24)
          else
            (s26, ns2)
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
              nanosPerSec13)
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
  lam nanosPerSec23.
    lam monoLogicalTime6.
      lam wallLogicalTime22.
        lam delay1.
          let oldPriority =
            setMaxPriority
              {}
          in
          let intervalTime =
            nanosToTimespec
              nanosPerSec23
              delay1
          in
          let endTime =
            getMonotonicTime
              {}
          in
          let elapsedTime =
            diffTimespec
              nanosPerSec23
              endTime
              (deref
                 monoLogicalTime6)
          in
          let waitTime =
            addTimespec
              nanosPerSec23
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
              let #var"35" =
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
                    nanosPerSec23
                    endTime
                    waitTime
                in
                timespecToNanos
                  nanosPerSec23
                  elapsedTime1
              else
                0
          in
          let #var"32" =
            modref
              monoLogicalTime6
              waitTime
          in
          let #var"33" =
            modref
              wallLogicalTime22
              (addTimespec
                 nanosPerSec23
                 (deref
                    wallLogicalTime22)
                 intervalTime)
          in
          let #var"34" =
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
  lam nanosPerSec22.
    lam wallLogicalTime21.
      lam offset.
        lam value1.
          let lt1 =
            deref
              wallLogicalTime21
          in
          (addTimespec
            nanosPerSec22
            lt1
            (nanosToTimespec
               nanosPerSec22
               offset), value1)
in
let sdelay =
  lam nanosPerSec21.
    lam monoLogicalTime5.
      lam wallLogicalTime20.
        lam flushOutputs1.
          lam updateInputs1.
            lam delay.
              let #var"30" =
                flushOutputs1
                  {}
              in
              let overrun =
                delayBy
                  nanosPerSec21
                  monoLogicalTime5
                  wallLogicalTime20
                  delay
              in
              let #var"31" =
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
  lam fd4.
    externalCloseFileDescriptor
      fd4
in
let rtpplReadFloatPort =
  lam fd3.
    externalReadFloatPipe
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
    lam #var"28".
      let #var"29" =
        closeFileDescriptors2
          {}
      in
      exit
        0
in
let rtpplRuntimeInit =
  lam monoLogicalTime4.
    lam wallLogicalTime19.
      lam updateInputSequences.
        lam closeFileDescriptors1.
          lam cont.
            let #var"23" =
              setSigintHandler
                (t1
                   closeFileDescriptors1)
            in
            let #var"24" =
              modref
                monoLogicalTime4
                (getMonotonicTime
                   {})
            in
            let #var"25" =
              modref
                wallLogicalTime19
                (getWallClockTime
                   {})
            in
            let #var"26" =
              updateInputSequences
                {}
            in
            let #var"27" =
              cont
                {}
            in
            {}
in
let fileDescriptors =
  { distObs =
      openFileDescriptor
        "rearRight-distObs",
    steeringAngleObs =
      openFileDescriptor
        "rearRight-steeringAngleObs",
    speedEst =
      openFileDescriptor
        "rearRight-speedEst",
    distEst =
      openFileDescriptor
        "rearRight-distEst" }
in
let closeFileDescriptors =
  lam fileDescriptors6.
    lam #var"22".
      let close_distObs =
        closeFileDescriptor
          (fileDescriptors6.distObs)
      in
      let close_steeringAngleObs =
        closeFileDescriptor
          (fileDescriptors6.steeringAngleObs)
      in
      let close_speedEst =
        closeFileDescriptor
          (fileDescriptors6.speedEst)
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
        "",
      steeringAngleObs =
        "",
      speedEst =
        "" }
in
let outputSeqs =
  ref
    { distEst =
        "" }
in
let t2 =
  lam tsv8.
    match
      tsv8
    with
      (ts5, v3)
    in
    (ts5, #var"RuntimeDistEmpirical_constructDistEmpiricalHelper"
        v3)
in
let updateInputs =
  lam fileDescriptors5.
    lam inputSeqs4.
      lam #var"21".
        modref
          inputSeqs4
          { distObs =
              rtpplReadFloatPort
                (fileDescriptors5.distObs),
            steeringAngleObs =
              rtpplReadFloatPort
                (fileDescriptors5.steeringAngleObs),
            speedEst =
              map
                t2
                (rtpplReadDistFloatRecordPort
                   (fileDescriptors5.speedEst)
                   2) }
in
let t3 =
  lam tsv7.
    match
      tsv7
    with
      (ts4, v2)
    in
    (ts4, unsafeCoerce
        (distEmpiricalSamples
           v2))
in
let flushOutputs =
  lam fileDescriptors4.
    lam outputSeqs4.
      lam #var"19".
        let w_distEst =
          rtpplWriteDistFloatPort
            (fileDescriptors4.distEst)
            (map
               t3
               ((deref
                   outputSeqs4).distEst))
        in
        let #var"20" =
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
let intToFloat =
  int2float
in
let concat: [Unknown] -> [Unknown] -> [Unknown] =
  lam l3.
    lam r1.
      concat
        l3
        r1
in
let sort1: (Unknown -> Unknown -> Int) -> [Unknown] -> [Unknown] =
  lam cmp.
    lam s25.
      quickSort
        cmp
        s25
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
let usMaxRange =
  4.
in
let maxDistance =
  10.
in
let inf1 =
  divf
    0.
    1.
in
let neginf =
  subf
    0.
    inf1
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
let t4 =
  lam nanosPerSec10.
    lam wallLogicalTime10.
      lam intToFloat8.
        lam wheelCircumference6.
          lam m1.
            lam b1.
              lam sigma1.
                lam #var"9".
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
                    let #var"10": () =
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
              let #var"8" =
                foldl
                  (t4
                     nanosPerSec9
                     wallLogicalTime9
                     intToFloat7
                     wheelCircumference5
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
let t5 =
  lam acc11.
    lam x2.
      let v =
        value
          x2
      in
      let acc12 =
        match
          gtf
            v
            acc11
        with
          true
        then
          let acc13 =
            v
          in
          acc13
        else
          acc11
      in
      acc12
in
let maxValue =
  lam neginf7.
    lam s14.
      let acc9 =
        neginf7
      in
      let acc10 =
        foldl
          t5
          acc9
          s14
      in
      acc10
in
let t6 =
  lam acc14.
    lam x3.
      let v1 =
        value
          x3
      in
      let acc15 =
        match
          ltf
            v1
            acc14
        with
          true
        then
          let acc16 =
            v1
          in
          acc16
        else
          acc14
      in
      acc15
in
let minValue =
  lam inf8.
    lam s13.
      let acc7 =
        inf8
      in
      let acc8 =
        foldl
          t6
          acc7
          s13
      in
      acc8
in
let t7 =
  lam nanosPerSec20.
    lam wallLogicalTime18.
      lam ltInt7.
        lam acc32.
          lam tsv6.
            let acc33 =
              match
                ltInt7
                  (timestamp
                     nanosPerSec20
                     wallLogicalTime18
                     acc32)
                  (timestamp
                     nanosPerSec20
                     wallLogicalTime18
                     tsv6)
              with
                true
              then
                let acc34 =
                  tsv6
                in
                acc34
              else
                acc32
            in
            acc33
in
let maxDistLineTimestamp =
  lam nanosPerSec19.
    lam wallLogicalTime17.
      lam ltInt6.
        lam acc30.
          lam tsvs1.
            let acc31 =
              foldl
                (t7
                   nanosPerSec19
                   wallLogicalTime17
                   ltInt6)
                acc30
                tsvs1
            in
            acc31
in
let cmpFloatTimestamp =
  lam nanosPerSec18.
    lam wallLogicalTime16.
      lam negInt5.
        lam ltInt5.
          lam gtInt5.
            lam l2.
              lam r.
                let acc25 =
                  0
                in
                let acc26 =
                  match
                    gtInt5
                      (timestamp
                         nanosPerSec18
                         wallLogicalTime16
                         l2)
                      (timestamp
                         nanosPerSec18
                         wallLogicalTime16
                         r)
                  with
                    true
                  then
                    let acc27 =
                      1
                    in
                    acc27
                  else
                    let acc28 =
                      match
                        ltInt5
                          (timestamp
                             nanosPerSec18
                             wallLogicalTime16
                             l2)
                          (timestamp
                             nanosPerSec18
                             wallLogicalTime16
                             r)
                      with
                        true
                      then
                        let acc29 =
                          negInt5
                            1
                        in
                        acc29
                      else
                        acc25
                    in
                    acc28
                in
                acc26
in
let medianOfLastThreeFloatTsv =
  lam subInt7.
    lam tsvs.
      let n =
        length
          tsvs
      in
      let a28 =
        value
          (get
             tsvs
             (subInt7
                n
                3))
      in
      let b4 =
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
      let res5 =
        0.
      in
      let res6 =
        match
          ltf
            a28
            b4
        with
          true
        then
          let res8 =
            match
              ltf
                b4
                c
            with
              true
            then
              let res9 =
                b4
              in
              res9
            else
              let res10 =
                c
              in
              res10
          in
          res8
        else
          let res11 =
            match
              ltf
                a28
                c
            with
              true
            then
              let res12 =
                a28
              in
              res12
            else
              let res13 =
                c
              in
              res13
          in
          res11
      in
      res6
in
let t8 =
  lam maxRange1.
    lam acc17.
      lam tsv5.
        let acc18 =
          match
            ltf
              (value
                 tsv5)
              maxRange1
          with
            true
          then
            let acc19 =
              false
            in
            acc19
          else
            acc17
        in
        acc18
in
let allOutOfRange: [TSV Float] -> Float -> Bool =
  lam distanceObs1.
    lam maxRange.
      let acc5 =
        true
      in
      let acc6 =
        foldl
          (t8
             maxRange)
          acc5
          distanceObs1
      in
      acc6
in
let t9 =
  lam nanosPerSec16.
    lam wallLogicalTime14.
      lam intToFloat12.
        lam m3.
          lam b3.
            lam sigma3.
              lam #var"15".
                lam tsv4.
                  let ts3 =
                    timestampToSeconds
                      intToFloat12
                      (timestamp
                         nanosPerSec16
                         wallLogicalTime14
                         tsv4)
                  in
                  let #var"16": () =
                    error
                      "Cannot use observe outside of inferred model"
                  in
                  {}
in
let frontRearDistanceModel =
  lam nanosPerSec15.
    lam wallLogicalTime13.
      lam subInt6.
        lam intToFloat11.
          lam maxDistance6.
            lam inf7.
              lam neginf6.
                lam distanceObs.
                  lam saObs3.
                    lam speedEst.
                      lam maxSensorRange.
                        let res =
                          0.
                        in
                        let res1 =
                          match
                            allOutOfRange
                              distanceObs
                              maxSensorRange
                          with
                            true
                          then
                            let res2 =
                              maxSensorRange
                            in
                            res2
                          else
                            let line =
                              error
                                "Cannot use assume outside of inferred model"
                            in
                            let m2 =
                              error
                                "Cannot use assume outside of inferred model"
                            in
                            let b2 =
                              error
                                "Cannot use assume outside of inferred model"
                            in
                            let sigma2 =
                              error
                                "Cannot use assume outside of inferred model"
                            in
                            let res3 =
                              match
                                eqf
                                  (minValue
                                     inf7
                                     saObs3)
                                  (maxValue
                                     neginf6
                                     saObs3)
                              with
                                true
                              then
                                let #var"14" =
                                  foldl
                                    (t9
                                       nanosPerSec15
                                       wallLogicalTime13
                                       intToFloat11
                                       m2
                                       b2
                                       sigma2)
                                    {}
                                    distanceObs
                                in
                                let res4 =
                                  b2
                                in
                                res4
                              else
                                let res7 =
                                  medianOfLastThreeFloatTsv
                                    subInt6
                                    distanceObs
                                in
                                res7
                            in
                            res3
                        in
                        res1
in
let takeThreeMostRecent =
  lam nanosPerSec17.
    lam wallLogicalTime15.
      lam subInt8.
        lam negInt4.
          lam ltInt4.
            lam gtInt4.
              lam geqInt4.
                lam a35.
                  let n1 =
                    length
                      a35
                  in
                  let a36 =
                    sort1
                      (cmpFloatTimestamp
                         nanosPerSec17
                         wallLogicalTime15
                         negInt4
                         ltInt4
                         gtInt4)
                      a35
                  in
                  let a37 =
                    match
                      geqInt4
                        n1
                        3
                    with
                      true
                    then
                      let a38 =
                        [ get
                            a36
                            (subInt8
                               n1
                               3),
                          get
                            a36
                            (subInt8
                               n1
                               2),
                          get
                            a36
                            (subInt8
                               n1
                               1) ]
                      in
                      a38
                    else
                      a36
                  in
                  a37
in
let t10 =
  lam nanosPerSec8.
    lam wallLogicalTime8.
      lam intToFloat6.
        lam maxSpeed4.
          lam wheelCircumference4.
            lam #var"7".
              speedModel
                nanosPerSec8
                wallLogicalTime8
                intToFloat6
                maxSpeed4
                wheelCircumference4
                ""
in
let t11 =
  lam saObs2: [((Int, Int), Float)].
    lam lastObs5: [TSV Float].
      lam lastSpeed5: (Timespec, Dist (Line)).
        lam nanosPerSec14: Int.
          lam wallLogicalTime12: Ref (Timespec).
            lam subInt5: Int -> Int -> Int.
              lam intToFloat10: Int -> Float.
                lam usMaxRange5: Float.
                  lam maxDistance5: Float.
                    lam inf6: Float.
                      lam neginf5: Float.
                        lam state1: State.
                          stopInit
                            (lam #var"13".
                               let map1 =
                                 lam f13.
                                   let t375 =
                                     lam s23.
                                       recursive
                                         let rec9 =
                                           lam s24.
                                             let t377 =
                                               match
                                                 s24
                                               with
                                                 ""
                                               then
                                                 let t378 =
                                                   ""
                                                 in
                                                 t378
                                               else
                                                 let t379 =
                                                   match
                                                     s24
                                                   with
                                                     [ a33 ]
                                                   then
                                                     let t380 =
                                                       f13
                                                         a33
                                                     in
                                                     let t381 =
                                                       [ t380 ]
                                                     in
                                                     t381
                                                   else
                                                     let t382 =
                                                       match
                                                         s24
                                                       with
                                                         [ a34 ] ++ ss7 ++ ""
                                                       then
                                                         let t383 =
                                                           cons
                                                         in
                                                         let t384 =
                                                           f13
                                                             a34
                                                         in
                                                         let t385 =
                                                           t383
                                                             t384
                                                         in
                                                         let t386 =
                                                           rec9
                                                             ss7
                                                         in
                                                         let t387 =
                                                           t385
                                                             t386
                                                         in
                                                         t387
                                                       else
                                                         let t388 =
                                                           never
                                                         in
                                                         t388
                                                     in
                                                     t382
                                                 in
                                                 t379
                                             in
                                             t377
                                       in
                                       let t376 =
                                         rec9
                                           s23
                                       in
                                       t376
                                   in
                                   t375
                               in
                               let iter1 =
                                 lam f12.
                                   let t372 =
                                     lam s22.
                                       let t373 =
                                         map1
                                           f12
                                       in
                                       let #var"18" =
                                         t373
                                           s22
                                       in
                                       let t374 =
                                         {}
                                       in
                                       t374
                                   in
                                   t372
                               in
                               let mapi1 =
                                 lam f11.
                                   let t348 =
                                     lam s20.
                                       recursive
                                         let rec8 =
                                           lam i3.
                                             let t352 =
                                               lam s21.
                                                 let t353 =
                                                   match
                                                     s21
                                                   with
                                                     ""
                                                   then
                                                     let t354 =
                                                       ""
                                                     in
                                                     t354
                                                   else
                                                     let t355 =
                                                       match
                                                         s21
                                                       with
                                                         [ a31 ]
                                                       then
                                                         let t356 =
                                                           f11
                                                             i3
                                                         in
                                                         let t357 =
                                                           t356
                                                             a31
                                                         in
                                                         let t358 =
                                                           [ t357 ]
                                                         in
                                                         t358
                                                       else
                                                         let t359 =
                                                           match
                                                             s21
                                                           with
                                                             [ a32 ] ++ ss6 ++ ""
                                                           then
                                                             let t360 =
                                                               cons
                                                             in
                                                             let t361 =
                                                               f11
                                                                 i3
                                                             in
                                                             let t362 =
                                                               t361
                                                                 a32
                                                             in
                                                             let t363 =
                                                               t360
                                                                 t362
                                                             in
                                                             let t364 =
                                                               addi
                                                             in
                                                             let t365 =
                                                               t364
                                                                 i3
                                                             in
                                                             let t366 =
                                                               1
                                                             in
                                                             let t367 =
                                                               t365
                                                                 t366
                                                             in
                                                             let t368 =
                                                               rec8
                                                                 t367
                                                             in
                                                             let t369 =
                                                               t368
                                                                 ss6
                                                             in
                                                             let t370 =
                                                               t363
                                                                 t369
                                                             in
                                                             t370
                                                           else
                                                             let t371 =
                                                               never
                                                             in
                                                             t371
                                                         in
                                                         t359
                                                     in
                                                     t355
                                                 in
                                                 t353
                                             in
                                             t352
                                       in
                                       let t349 =
                                         0
                                       in
                                       let t350 =
                                         rec8
                                           t349
                                       in
                                       let t351 =
                                         t350
                                           s20
                                       in
                                       t351
                                   in
                                   t348
                               in
                               let iteri1 =
                                 lam f10.
                                   let t345 =
                                     lam s19.
                                       let t346 =
                                         mapi1
                                           f10
                                       in
                                       let #var"17" =
                                         t346
                                           s19
                                       in
                                       let t347 =
                                         {}
                                       in
                                       t347
                                   in
                                   t345
                               in
                               let foldl2 =
                                 lam f9.
                                   let t333 =
                                     lam acc23.
                                       let t334 =
                                         lam s17.
                                           recursive
                                             let rec7 =
                                               lam acc24.
                                                 let t337 =
                                                   lam s18.
                                                     let t338 =
                                                       match
                                                         s18
                                                       with
                                                         ""
                                                       then
                                                         acc24
                                                       else
                                                         let t339 =
                                                           match
                                                             s18
                                                           with
                                                             [ a30 ] ++ ss5 ++ ""
                                                           then
                                                             let t340 =
                                                               f9
                                                                 acc24
                                                             in
                                                             let t341 =
                                                               t340
                                                                 a30
                                                             in
                                                             let t342 =
                                                               rec7
                                                                 t341
                                                             in
                                                             let t343 =
                                                               t342
                                                                 ss5
                                                             in
                                                             t343
                                                           else
                                                             let t344 =
                                                               never
                                                             in
                                                             t344
                                                         in
                                                         t339
                                                     in
                                                     t338
                                                 in
                                                 t337
                                           in
                                           let t335 =
                                             rec7
                                               acc23
                                           in
                                           let t336 =
                                             t335
                                               s17
                                           in
                                           t336
                                       in
                                       t334
                                   in
                                   t333
                               in
                               let foldr2 =
                                 lam f8.
                                   let t321 =
                                     lam acc21.
                                       let t322 =
                                         lam s15.
                                           recursive
                                             let rec6 =
                                               lam acc22.
                                                 let t325 =
                                                   lam s16.
                                                     let t326 =
                                                       match
                                                         s16
                                                       with
                                                         ""
                                                       then
                                                         acc22
                                                       else
                                                         let t327 =
                                                           match
                                                             s16
                                                           with
                                                             [ a29 ] ++ ss4 ++ ""
                                                           then
                                                             let t328 =
                                                               f8
                                                                 a29
                                                             in
                                                             let t329 =
                                                               rec6
                                                                 acc22
                                                             in
                                                             let t330 =
                                                               t329
                                                                 ss4
                                                             in
                                                             let t331 =
                                                               t328
                                                                 t330
                                                             in
                                                             t331
                                                           else
                                                             let t332 =
                                                               never
                                                             in
                                                             t332
                                                         in
                                                         t327
                                                     in
                                                     t326
                                                 in
                                                 t325
                                           in
                                           let t323 =
                                             rec6
                                               acc21
                                           in
                                           let t324 =
                                             t323
                                               s15
                                           in
                                           t324
                                       in
                                       t322
                                   in
                                   t321
                               in
                               let create1 =
                                 lam l1.
                                   let t297 =
                                     lam f7.
                                       recursive
                                         let rec5 =
                                           lam i2.
                                             let t305 =
                                               lam acc20.
                                                 let t306 =
                                                   geqi
                                                 in
                                                 let t307 =
                                                   t306
                                                     i2
                                                 in
                                                 let t308 =
                                                   0
                                                 in
                                                 let t309 =
                                                   t307
                                                     t308
                                                 in
                                                 let t310 =
                                                   match
                                                     t309
                                                   with
                                                     true
                                                   then
                                                     let t311 =
                                                       subi
                                                     in
                                                     let t312 =
                                                       t311
                                                         i2
                                                     in
                                                     let t313 =
                                                       1
                                                     in
                                                     let t314 =
                                                       t312
                                                         t313
                                                     in
                                                     let t315 =
                                                       rec5
                                                         t314
                                                     in
                                                     let t316 =
                                                       cons
                                                     in
                                                     let t317 =
                                                       f7
                                                         i2
                                                     in
                                                     let t318 =
                                                       t316
                                                         t317
                                                     in
                                                     let t319 =
                                                       t318
                                                         acc20
                                                     in
                                                     let t320 =
                                                       t315
                                                         t319
                                                     in
                                                     t320
                                                   else
                                                     acc20
                                                 in
                                                 t310
                                             in
                                             t305
                                       in
                                       let t298 =
                                         subi
                                       in
                                       let t299 =
                                         t298
                                           l1
                                       in
                                       let t300 =
                                         1
                                       in
                                       let t301 =
                                         t299
                                           t300
                                       in
                                       let t302 =
                                         rec5
                                           t301
                                       in
                                       let t303 =
                                         ""
                                       in
                                       let t304 =
                                         t302
                                           t303
                                       in
                                       t304
                                   in
                                   t297
                               in
                               let t176 =
                                 {}
                               in
                               let value =
                                 lam tsv3.
                                   let t295 =
                                     match
                                       tsv3
                                     with
                                       {#label"1" = #var"X"}
                                     then
                                       #var"X"
                                     else
                                       let t296 =
                                         never
                                       in
                                       t296
                                   in
                                   t295
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
                               let intToFloat3 =
                                 intToFloat10
                               in
                               let usMaxRange3 =
                                 usMaxRange5
                               in
                               let maxDistance3 =
                                 maxDistance5
                               in
                               let inf4 =
                                 inf6
                               in
                               let neginf3 =
                                 neginf5
                               in
                               let saObs =
                                 saObs2
                               in
                               let lastObs1 =
                                 lastObs5
                               in
                               let lastSpeed2 =
                                 lastSpeed5
                               in
                               let #var"2" =
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
                               let intToFloat11 =
                                 intToFloat3
                               in
                               let maxDistance6 =
                                 maxDistance3
                               in
                               let inf7 =
                                 inf4
                               in
                               let neginf6 =
                                 neginf3
                               in
                               let distanceObs =
                                 lastObs1
                               in
                               let saObs3 =
                                 saObs
                               in
                               let speedEst =
                                 lastSpeed2
                               in
                               let maxSensorRange =
                                 usMaxRange3
                               in
                               let res =
                                 0.
                               in
                               let distanceObs1 =
                                 distanceObs
                               in
                               let maxRange =
                                 maxSensorRange
                               in
                               let acc5 =
                                 true
                               in
                               let t177 =
                                 foldl2
                               in
                               let maxRange1 =
                                 maxRange
                               in
                               let t178 =
                                 lam acc17.
                                   let t290 =
                                     lam tsv5.
                                       let t291 =
                                         ltf
                                       in
                                       let t292 =
                                         value
                                           tsv5
                                       in
                                       let t293 =
                                         t291
                                           t292
                                       in
                                       let t294 =
                                         t293
                                           maxRange1
                                       in
                                       let acc18 =
                                         match
                                           t294
                                         with
                                           true
                                         then
                                           let acc19 =
                                             false
                                           in
                                           acc19
                                         else
                                           acc17
                                       in
                                       acc18
                                   in
                                   t290
                               in
                               let t179 =
                                 t177
                                   t178
                               in
                               let t180 =
                                 t179
                                   acc5
                               in
                               let acc6 =
                                 t180
                                   distanceObs1
                               in
                               let res1 =
                                 match
                                   acc6
                                 with
                                   true
                                 then
                                   let res2 =
                                     maxSensorRange
                                   in
                                   res2
                                 else
                                   let t181 =
                                     value
                                       speedEst
                                   in
                                   let line =
                                     sample
                                       t181
                                   in
                                   let t182 =
                                     0.
                                   in
                                   let t183 =
                                     match
                                       line
                                     with
                                       {intercept = x4}
                                     then
                                       x4
                                     else
                                       let t289 =
                                         never
                                       in
                                       t289
                                   in
                                   let t184 =
                                     RuntimeDistElementary_DistGaussian
                                       { mu =
                                           t182,
                                         sigma =
                                           t183 }
                                   in
                                   let m2 =
                                     sample
                                       t184
                                   in
                                   let t185 =
                                     0.
                                   in
                                   let t186 =
                                     RuntimeDistElementary_DistUniform
                                       { a =
                                           t185,
                                         b =
                                           maxDistance6 }
                                   in
                                   let b2 =
                                     sample
                                       t186
                                   in
                                   let t187 =
                                     1.
                                   in
                                   let t188 =
                                     1.
                                   in
                                   let t189 =
                                     RuntimeDistElementary_DistGamma
                                       { scale =
                                           t188,
                                         shape =
                                           t187 }
                                   in
                                   let sigma2 =
                                     sample
                                       t189
                                   in
                                   let t190 =
                                     eqf
                                   in
                                   let inf8 =
                                     inf7
                                   in
                                   let s13 =
                                     saObs3
                                   in
                                   let acc7 =
                                     inf8
                                   in
                                   let t191 =
                                     foldl2
                                   in
                                   let t192 =
                                     lam acc14.
                                       let t285 =
                                         lam x3.
                                           let v1 =
                                             value
                                               x3
                                           in
                                           let t286 =
                                             ltf
                                           in
                                           let t287 =
                                             t286
                                               v1
                                           in
                                           let t288 =
                                             t287
                                               acc14
                                           in
                                           let acc15 =
                                             match
                                               t288
                                             with
                                               true
                                             then
                                               let acc16 =
                                                 v1
                                               in
                                               acc16
                                             else
                                               acc14
                                           in
                                           acc15
                                       in
                                       t285
                                   in
                                   let t193 =
                                     t191
                                       t192
                                   in
                                   let t194 =
                                     t193
                                       acc7
                                   in
                                   let acc8 =
                                     t194
                                       s13
                                   in
                                   let t195 =
                                     t190
                                       acc8
                                   in
                                   let neginf7 =
                                     neginf6
                                   in
                                   let s14 =
                                     saObs3
                                   in
                                   let acc9 =
                                     neginf7
                                   in
                                   let t196 =
                                     foldl2
                                   in
                                   let t197 =
                                     lam acc11.
                                       let t281 =
                                         lam x2.
                                           let v =
                                             value
                                               x2
                                           in
                                           let t282 =
                                             gtf
                                           in
                                           let t283 =
                                             t282
                                               v
                                           in
                                           let t284 =
                                             t283
                                               acc11
                                           in
                                           let acc12 =
                                             match
                                               t284
                                             with
                                               true
                                             then
                                               let acc13 =
                                                 v
                                               in
                                               acc13
                                             else
                                               acc11
                                           in
                                           acc12
                                       in
                                       t281
                                   in
                                   let t198 =
                                     t196
                                       t197
                                   in
                                   let t199 =
                                     t198
                                       acc9
                                   in
                                   let acc10 =
                                     t199
                                       s14
                                   in
                                   let t200 =
                                     t195
                                       acc10
                                   in
                                   let res3 =
                                     match
                                       t200
                                     with
                                       true
                                     then
                                       let t201 =
                                         foldl2
                                       in
                                       let nanosPerSec16 =
                                         nanosPerSec15
                                       in
                                       let wallLogicalTime14 =
                                         wallLogicalTime13
                                       in
                                       let intToFloat12 =
                                         intToFloat11
                                       in
                                       let m3 =
                                         m2
                                       in
                                       let b3 =
                                         b2
                                       in
                                       let sigma3 =
                                         sigma2
                                       in
                                       let t202 =
                                         lam #var"15".
                                           let t206 =
                                             lam tsv4.
                                               let intToFloat9 =
                                                 intToFloat12
                                               in
                                               let nanosPerSec11 =
                                                 nanosPerSec16
                                               in
                                               let wallLogicalTime11 =
                                                 wallLogicalTime14
                                               in
                                               let tsv2 =
                                                 tsv4
                                               in
                                               let t207 =
                                                 deref
                                               in
                                               let lt =
                                                 t207
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
                                                   let t252 =
                                                     never
                                                   in
                                                   t252
                                               in
                                               let rhs =
                                                 lt
                                               in
                                               let t208 =
                                                 (lhs, rhs)
                                               in
                                               let t209 =
                                                 match
                                                   t208
                                                 with
                                                   ((ls, lns), (rs, rns))
                                                 then
                                                   let t233 =
                                                     subi
                                                   in
                                                   let t234 =
                                                     t233
                                                       ls
                                                   in
                                                   let s2 =
                                                     t234
                                                       rs
                                                   in
                                                   let t235 =
                                                     subi
                                                   in
                                                   let t236 =
                                                     t235
                                                       lns
                                                   in
                                                   let ns1 =
                                                     t236
                                                       rns
                                                   in
                                                   let t237 =
                                                     lti
                                                   in
                                                   let t238 =
                                                     t237
                                                       ns1
                                                   in
                                                   let t239 =
                                                     0
                                                   in
                                                   let t240 =
                                                     t238
                                                       t239
                                                   in
                                                   let t241 =
                                                     match
                                                       t240
                                                     with
                                                       true
                                                     then
                                                       let t242 =
                                                         addi
                                                       in
                                                       let t243 =
                                                         t242
                                                           ns1
                                                       in
                                                       let t244 =
                                                         t243
                                                           nanosPerSec13
                                                       in
                                                       let t245 =
                                                         subi
                                                       in
                                                       let t246 =
                                                         t245
                                                           s2
                                                       in
                                                       let t247 =
                                                         1
                                                       in
                                                       let t248 =
                                                         t246
                                                           t247
                                                       in
                                                       let t249 =
                                                         (t248, t244)
                                                       in
                                                       t249
                                                     else
                                                       let t250 =
                                                         (s2, ns1)
                                                       in
                                                       t250
                                                   in
                                                   t241
                                                 else
                                                   let t251 =
                                                     never
                                                   in
                                                   t251
                                               in
                                               let ts =
                                                 t209
                                               in
                                               let t210 =
                                                 match
                                                   ts
                                                 with
                                                   (s1, ns)
                                                 then
                                                   let t226 =
                                                     addi
                                                   in
                                                   let t227 =
                                                     muli
                                                   in
                                                   let t228 =
                                                     t227
                                                       s1
                                                   in
                                                   let t229 =
                                                     t228
                                                       nanosPerSec12
                                                   in
                                                   let t230 =
                                                     t226
                                                       t229
                                                   in
                                                   let t231 =
                                                     t230
                                                       ns
                                                   in
                                                   t231
                                                 else
                                                   let t232 =
                                                     never
                                                   in
                                                   t232
                                               in
                                               let ts1 =
                                                 t210
                                               in
                                               let t211 =
                                                 divf
                                               in
                                               let t212 =
                                                 intToFloat9
                                                   ts1
                                               in
                                               let t213 =
                                                 t211
                                                   t212
                                               in
                                               let t214 =
                                                 1000000000
                                               in
                                               let t215 =
                                                 intToFloat9
                                                   t214
                                               in
                                               let t216 =
                                                 t213
                                                   t215
                                               in
                                               let ts3 =
                                                 t216
                                               in
                                               let t217 =
                                                 value
                                                   tsv4
                                               in
                                               let t218 =
                                                 addf
                                               in
                                               let t219 =
                                                 mulf
                                               in
                                               let t220 =
                                                 t219
                                                   m3
                                               in
                                               let t221 =
                                                 t220
                                                   ts3
                                               in
                                               let t222 =
                                                 t218
                                                   t221
                                               in
                                               let t223 =
                                                 t222
                                                   b3
                                               in
                                               let t224 =
                                                 RuntimeDistElementary_DistGaussian
                                                   { mu =
                                                       t223,
                                                     sigma =
                                                       sigma3 }
                                               in
                                               let #var"16": () =
                                                 updateWeight
                                                   (logObserve
                                                      t224
                                                      t217)
                                                   state1
                                               in
                                               let t225 =
                                                 {}
                                               in
                                               t225
                                           in
                                           t206
                                       in
                                       let t203 =
                                         t201
                                           t202
                                       in
                                       let t204 =
                                         {}
                                       in
                                       let t205 =
                                         t203
                                           t204
                                       in
                                       let #var"14" =
                                         t205
                                           distanceObs
                                       in
                                       let res4 =
                                         b2
                                       in
                                       res4
                                     else
                                       let subInt7 =
                                         subInt6
                                       in
                                       let tsvs =
                                         distanceObs
                                       in
                                       let t253 =
                                         length
                                       in
                                       let n =
                                         t253
                                           tsvs
                                       in
                                       let t254 =
                                         get
                                       in
                                       let t255 =
                                         t254
                                           tsvs
                                       in
                                       let t256 =
                                         subInt7
                                           n
                                       in
                                       let t257 =
                                         3
                                       in
                                       let t258 =
                                         t256
                                           t257
                                       in
                                       let t259 =
                                         t255
                                           t258
                                       in
                                       let a28 =
                                         value
                                           t259
                                       in
                                       let t260 =
                                         get
                                       in
                                       let t261 =
                                         t260
                                           tsvs
                                       in
                                       let t262 =
                                         subInt7
                                           n
                                       in
                                       let t263 =
                                         2
                                       in
                                       let t264 =
                                         t262
                                           t263
                                       in
                                       let t265 =
                                         t261
                                           t264
                                       in
                                       let b4 =
                                         value
                                           t265
                                       in
                                       let t266 =
                                         get
                                       in
                                       let t267 =
                                         t266
                                           tsvs
                                       in
                                       let t268 =
                                         subInt7
                                           n
                                       in
                                       let t269 =
                                         1
                                       in
                                       let t270 =
                                         t268
                                           t269
                                       in
                                       let t271 =
                                         t267
                                           t270
                                       in
                                       let c =
                                         value
                                           t271
                                       in
                                       let res5 =
                                         0.
                                       in
                                       let t272 =
                                         ltf
                                       in
                                       let t273 =
                                         t272
                                           a28
                                       in
                                       let t274 =
                                         t273
                                           b4
                                       in
                                       let res6 =
                                         match
                                           t274
                                         with
                                           true
                                         then
                                           let t275 =
                                             ltf
                                           in
                                           let t276 =
                                             t275
                                               b4
                                           in
                                           let t277 =
                                             t276
                                               c
                                           in
                                           let res8 =
                                             match
                                               t277
                                             with
                                               true
                                             then
                                               let res9 =
                                                 b4
                                               in
                                               res9
                                             else
                                               let res10 =
                                                 c
                                               in
                                               res10
                                           in
                                           res8
                                         else
                                           let t278 =
                                             ltf
                                           in
                                           let t279 =
                                             t278
                                               a28
                                           in
                                           let t280 =
                                             t279
                                               c
                                           in
                                           let res11 =
                                             match
                                               t280
                                             with
                                               true
                                             then
                                               let res12 =
                                                 a28
                                               in
                                               res12
                                             else
                                               let res13 =
                                                 c
                                               in
                                               res13
                                           in
                                           res11
                                       in
                                       let res7 =
                                         res6
                                       in
                                       res7
                                   in
                                   res3
                               in
                               (lam x1.
                                  End
                                    x1)
                                 res1)
in
let t12 =
  lam nanosPerSec7: Int.
    lam wallLogicalTime7: Ref (Timespec).
      lam intToFloat5: Int -> Float.
        lam maxSpeed3: Float.
          lam wheelCircumference3: Float.
            lam state: State.
              stopInit
                (lam #var"6".
                   let map =
                     lam f6.
                       let t162 =
                         lam s11.
                           recursive
                             let rec4 =
                               lam s12.
                                 let t164 =
                                   match
                                     s12
                                   with
                                     ""
                                   then
                                     let t165 =
                                       ""
                                     in
                                     t165
                                   else
                                     let t166 =
                                       match
                                         s12
                                       with
                                         [ a26 ]
                                       then
                                         let t167 =
                                           f6
                                             a26
                                         in
                                         let t168 =
                                           [ t167 ]
                                         in
                                         t168
                                       else
                                         let t169 =
                                           match
                                             s12
                                           with
                                             [ a27 ] ++ ss3 ++ ""
                                           then
                                             let t170 =
                                               cons
                                             in
                                             let t171 =
                                               f6
                                                 a27
                                             in
                                             let t172 =
                                               t170
                                                 t171
                                             in
                                             let t173 =
                                               rec4
                                                 ss3
                                             in
                                             let t174 =
                                               t172
                                                 t173
                                             in
                                             t174
                                           else
                                             let t175 =
                                               never
                                             in
                                             t175
                                         in
                                         t169
                                     in
                                     t166
                                 in
                                 t164
                           in
                           let t163 =
                             rec4
                               s11
                           in
                           t163
                       in
                       t162
                   in
                   let iter =
                     lam f5.
                       let t159 =
                         lam s10.
                           let t160 =
                             map
                               f5
                           in
                           let #var"12" =
                             t160
                               s10
                           in
                           let t161 =
                             {}
                           in
                           t161
                       in
                       t159
                   in
                   let mapi =
                     lam f4.
                       let t135 =
                         lam s8.
                           recursive
                             let rec3 =
                               lam i1.
                                 let t139 =
                                   lam s9.
                                     let t140 =
                                       match
                                         s9
                                       with
                                         ""
                                       then
                                         let t141 =
                                           ""
                                         in
                                         t141
                                       else
                                         let t142 =
                                           match
                                             s9
                                           with
                                             [ a24 ]
                                           then
                                             let t143 =
                                               f4
                                                 i1
                                             in
                                             let t144 =
                                               t143
                                                 a24
                                             in
                                             let t145 =
                                               [ t144 ]
                                             in
                                             t145
                                           else
                                             let t146 =
                                               match
                                                 s9
                                               with
                                                 [ a25 ] ++ ss2 ++ ""
                                               then
                                                 let t147 =
                                                   cons
                                                 in
                                                 let t148 =
                                                   f4
                                                     i1
                                                 in
                                                 let t149 =
                                                   t148
                                                     a25
                                                 in
                                                 let t150 =
                                                   t147
                                                     t149
                                                 in
                                                 let t151 =
                                                   addi
                                                 in
                                                 let t152 =
                                                   t151
                                                     i1
                                                 in
                                                 let t153 =
                                                   1
                                                 in
                                                 let t154 =
                                                   t152
                                                     t153
                                                 in
                                                 let t155 =
                                                   rec3
                                                     t154
                                                 in
                                                 let t156 =
                                                   t155
                                                     ss2
                                                 in
                                                 let t157 =
                                                   t150
                                                     t156
                                                 in
                                                 t157
                                               else
                                                 let t158 =
                                                   never
                                                 in
                                                 t158
                                             in
                                             t146
                                         in
                                         t142
                                     in
                                     t140
                                 in
                                 t139
                           in
                           let t136 =
                             0
                           in
                           let t137 =
                             rec3
                               t136
                           in
                           let t138 =
                             t137
                               s8
                           in
                           t138
                       in
                       t135
                   in
                   let iteri =
                     lam f3.
                       let t132 =
                         lam s7.
                           let t133 =
                             mapi
                               f3
                           in
                           let #var"11" =
                             t133
                               s7
                           in
                           let t134 =
                             {}
                           in
                           t134
                       in
                       t132
                   in
                   let foldl =
                     lam f2.
                       let t120 =
                         lam acc3.
                           let t121 =
                             lam s5.
                               recursive
                                 let rec2 =
                                   lam acc4.
                                     let t124 =
                                       lam s6.
                                         let t125 =
                                           match
                                             s6
                                           with
                                             ""
                                           then
                                             acc4
                                           else
                                             let t126 =
                                               match
                                                 s6
                                               with
                                                 [ a23 ] ++ ss1 ++ ""
                                               then
                                                 let t127 =
                                                   f2
                                                     acc4
                                                 in
                                                 let t128 =
                                                   t127
                                                     a23
                                                 in
                                                 let t129 =
                                                   rec2
                                                     t128
                                                 in
                                                 let t130 =
                                                   t129
                                                     ss1
                                                 in
                                                 t130
                                               else
                                                 let t131 =
                                                   never
                                                 in
                                                 t131
                                             in
                                             t126
                                         in
                                         t125
                                     in
                                     t124
                               in
                               let t122 =
                                 rec2
                                   acc3
                               in
                               let t123 =
                                 t122
                                   s5
                               in
                               t123
                           in
                           t121
                       in
                       t120
                   in
                   let foldr =
                     lam f1.
                       let t108 =
                         lam acc1.
                           let t109 =
                             lam s3.
                               recursive
                                 let rec1 =
                                   lam acc2.
                                     let t112 =
                                       lam s4.
                                         let t113 =
                                           match
                                             s4
                                           with
                                             ""
                                           then
                                             acc2
                                           else
                                             let t114 =
                                               match
                                                 s4
                                               with
                                                 [ a22 ] ++ ss ++ ""
                                               then
                                                 let t115 =
                                                   f1
                                                     a22
                                                 in
                                                 let t116 =
                                                   rec1
                                                     acc2
                                                 in
                                                 let t117 =
                                                   t116
                                                     ss
                                                 in
                                                 let t118 =
                                                   t115
                                                     t117
                                                 in
                                                 t118
                                               else
                                                 let t119 =
                                                   never
                                                 in
                                                 t119
                                             in
                                             t114
                                         in
                                         t113
                                     in
                                     t112
                               in
                               let t110 =
                                 rec1
                                   acc1
                               in
                               let t111 =
                                 t110
                                   s3
                               in
                               t111
                           in
                           t109
                       in
                       t108
                   in
                   let create =
                     lam l.
                       let t84 =
                         lam f.
                           recursive
                             let rec =
                               lam i.
                                 let t92 =
                                   lam acc.
                                     let t93 =
                                       geqi
                                     in
                                     let t94 =
                                       t93
                                         i
                                     in
                                     let t95 =
                                       0
                                     in
                                     let t96 =
                                       t94
                                         t95
                                     in
                                     let t97 =
                                       match
                                         t96
                                       with
                                         true
                                       then
                                         let t98 =
                                           subi
                                         in
                                         let t99 =
                                           t98
                                             i
                                         in
                                         let t100 =
                                           1
                                         in
                                         let t101 =
                                           t99
                                             t100
                                         in
                                         let t102 =
                                           rec
                                             t101
                                         in
                                         let t103 =
                                           cons
                                         in
                                         let t104 =
                                           f
                                             i
                                         in
                                         let t105 =
                                           t103
                                             t104
                                         in
                                         let t106 =
                                           t105
                                             acc
                                         in
                                         let t107 =
                                           t102
                                             t106
                                         in
                                         t107
                                       else
                                         acc
                                     in
                                     t97
                                 in
                                 t92
                           in
                           let t85 =
                             subi
                           in
                           let t86 =
                             t85
                               l
                           in
                           let t87 =
                             1
                           in
                           let t88 =
                             t86
                               t87
                           in
                           let t89 =
                             rec
                               t88
                           in
                           let t90 =
                             ""
                           in
                           let t91 =
                             t89
                               t90
                           in
                           t91
                       in
                       t84
                   in
                   let t15 =
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
                   let #var"7" =
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
                   let t16 =
                     0.
                   in
                   let t17 =
                     0.1
                   in
                   let t18 =
                     RuntimeDistElementary_DistGaussian
                       { mu =
                           t16,
                         sigma =
                           t17 }
                   in
                   let m =
                     sample
                       t18
                   in
                   let t19 =
                     0.
                   in
                   let t20 =
                     RuntimeDistElementary_DistUniform
                       { a =
                           t19,
                         b =
                           maxSpeed5 }
                   in
                   let b =
                     sample
                       t20
                   in
                   let t21 =
                     1.
                   in
                   let t22 =
                     1.
                   in
                   let t23 =
                     RuntimeDistElementary_DistGamma
                       { scale =
                           t22,
                         shape =
                           t21 }
                   in
                   let sigma =
                     sample
                       t23
                   in
                   let t24 =
                     foldl
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
                   let m1 =
                     m
                   in
                   let b1 =
                     b
                   in
                   let sigma1 =
                     sigma
                   in
                   let t25 =
                     lam #var"9".
                       let t30 =
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
                           let t31 =
                             deref
                           in
                           let lt =
                             t31
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
                               let t83 =
                                 never
                               in
                               t83
                           in
                           let rhs =
                             lt
                           in
                           let t32 =
                             (lhs, rhs)
                           in
                           let t33 =
                             match
                               t32
                             with
                               ((ls, lns), (rs, rns))
                             then
                               let t64 =
                                 subi
                               in
                               let t65 =
                                 t64
                                   ls
                               in
                               let s2 =
                                 t65
                                   rs
                               in
                               let t66 =
                                 subi
                               in
                               let t67 =
                                 t66
                                   lns
                               in
                               let ns1 =
                                 t67
                                   rns
                               in
                               let t68 =
                                 lti
                               in
                               let t69 =
                                 t68
                                   ns1
                               in
                               let t70 =
                                 0
                               in
                               let t71 =
                                 t69
                                   t70
                               in
                               let t72 =
                                 match
                                   t71
                                 with
                                   true
                                 then
                                   let t73 =
                                     addi
                                   in
                                   let t74 =
                                     t73
                                       ns1
                                   in
                                   let t75 =
                                     t74
                                       nanosPerSec13
                                   in
                                   let t76 =
                                     subi
                                   in
                                   let t77 =
                                     t76
                                       s2
                                   in
                                   let t78 =
                                     1
                                   in
                                   let t79 =
                                     t77
                                       t78
                                   in
                                   let t80 =
                                     (t79, t75)
                                   in
                                   t80
                                 else
                                   let t81 =
                                     (s2, ns1)
                                   in
                                   t81
                               in
                               t72
                             else
                               let t82 =
                                 never
                               in
                               t82
                           in
                           let ts =
                             t33
                           in
                           let t34 =
                             match
                               ts
                             with
                               (s1, ns)
                             then
                               let t57 =
                                 addi
                               in
                               let t58 =
                                 muli
                               in
                               let t59 =
                                 t58
                                   s1
                               in
                               let t60 =
                                 t59
                                   nanosPerSec12
                               in
                               let t61 =
                                 t57
                                   t60
                               in
                               let t62 =
                                 t61
                                   ns
                               in
                               t62
                             else
                               let t63 =
                                 never
                               in
                               t63
                           in
                           let ts1 =
                             t34
                           in
                           let t35 =
                             divf
                           in
                           let t36 =
                             intToFloat9
                               ts1
                           in
                           let t37 =
                             t35
                               t36
                           in
                           let t38 =
                             1000000000
                           in
                           let t39 =
                             intToFloat9
                               t38
                           in
                           let t40 =
                             t37
                               t39
                           in
                           let ts2 =
                             t40
                           in
                           let tsv3 =
                             tsv1
                           in
                           let t41 =
                             match
                               tsv3
                             with
                               {#label"1" = #var"X"}
                             then
                               #var"X"
                             else
                               let t56 =
                                 never
                               in
                               t56
                           in
                           let rpm =
                             t41
                           in
                           let t42 =
                             divf
                           in
                           let t43 =
                             mulf
                           in
                           let t44 =
                             t43
                               rpm
                           in
                           let t45 =
                             t44
                               wheelCircumference6
                           in
                           let t46 =
                             t42
                               t45
                           in
                           let t47 =
                             60.
                           in
                           let mps =
                             t46
                               t47
                           in
                           let t48 =
                             addf
                           in
                           let t49 =
                             mulf
                           in
                           let t50 =
                             t49
                               m1
                           in
                           let t51 =
                             t50
                               ts2
                           in
                           let t52 =
                             t48
                               t51
                           in
                           let t53 =
                             t52
                               b1
                           in
                           let t54 =
                             RuntimeDistElementary_DistGaussian
                               { mu =
                                   t53,
                                 sigma =
                                   sigma1 }
                           in
                           let #var"10": () =
                             updateWeight
                               (logObserve
                                  t54
                                  mps)
                               state
                           in
                           let t55 =
                             {}
                           in
                           t55
                       in
                       t30
                   in
                   let t26 =
                     t24
                       t25
                   in
                   let t27 =
                     {}
                   in
                   let t28 =
                     t26
                       t27
                   in
                   let #var"8" =
                     t28
                       speedObs
                   in
                   let t29 =
                     { slope =
                         m,
                       intercept =
                         b }
                   in
                   (lam x.
                      End
                        x)
                     t29)
in
recursive
  let t14 =
    lam nanosPerSec4.
      lam wallLogicalTime4.
        lam subInt3.
          lam intToFloat3.
            lam usMaxRange3.
              lam maxDistance3.
                lam inf4.
                  lam neginf3.
                    lam saObs.
                      lam lastObs1.
                        lam lastSpeed2.
                          lam #var"2".
                            frontRearDistanceModel
                              nanosPerSec4
                              wallLogicalTime4
                              subInt3
                              intToFloat3
                              maxDistance3
                              inf4
                              neginf3
                              lastObs1
                              saObs
                              lastSpeed2
                              usMaxRange3
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
                      lam negInt3.
                        lam ltInt3.
                          lam gtInt3.
                            lam geqInt3.
                              lam intToFloat4.
                                lam usMaxRange4.
                                  lam maxDistance4.
                                    lam inf5.
                                      lam neginf4.
                                        lam period1.
                                          lam lastObs2.
                                            lam lastSpeed3.
                                              match
                                                true
                                              with
                                                true
                                              then
                                                let #var"3" =
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
                                                let obs =
                                                  unsafeCoerce
                                                    ((deref
                                                        inputSeqs3).distObs)
                                                in
                                                let saObs1 =
                                                  unsafeCoerce
                                                    ((deref
                                                        inputSeqs3).steeringAngleObs)
                                                in
                                                let speed =
                                                  unsafeCoerce
                                                    ((deref
                                                        inputSeqs3).speedEst)
                                                in
                                                let lastObs3 =
                                                  match
                                                    gtInt3
                                                      (length
                                                         obs)
                                                      0
                                                  with
                                                    true
                                                  then
                                                    let lastObs4 =
                                                      takeThreeMostRecent
                                                        nanosPerSec6
                                                        wallLogicalTime6
                                                        subInt4
                                                        negInt3
                                                        ltInt3
                                                        gtInt3
                                                        geqInt3
                                                        (concat
                                                           lastObs2
                                                           obs)
                                                    in
                                                    lastObs4
                                                  else
                                                    lastObs2
                                                in
                                                let lastSpeed4 =
                                                  maxDistLineTimestamp
                                                    nanosPerSec6
                                                    wallLogicalTime6
                                                    ltInt3
                                                    lastSpeed3
                                                    speed
                                                in
                                                let #var"4" =
                                                  match
                                                    match
                                                      eqf
                                                        (minValue
                                                           inf5
                                                           saObs1)
                                                        (maxValue
                                                           neginf4
                                                           saObs1)
                                                    with
                                                      true
                                                    then
                                                      true
                                                    else
                                                      geqInt3
                                                        (length
                                                           lastObs3)
                                                        3
                                                  with
                                                    true
                                                  then
                                                    let d =
                                                      run
                                                        { particles =
                                                            1000 }
                                                        (t11
                                                           saObs1
                                                           lastObs3
                                                           lastSpeed4
                                                           nanosPerSec6
                                                           wallLogicalTime6
                                                           subInt4
                                                           intToFloat4
                                                           usMaxRange4
                                                           maxDistance4
                                                           inf5
                                                           neginf4)
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
                                                                 nanosPerSec5
                                                                 wallLogicalTime5
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
                                                  nanosPerSec5
                                                  monoLogicalTime3
                                                  wallLogicalTime5
                                                  nanosPerSec6
                                                  wallLogicalTime6
                                                  subInt4
                                                  negInt3
                                                  ltInt3
                                                  gtInt3
                                                  geqInt3
                                                  intToFloat4
                                                  usMaxRange4
                                                  maxDistance4
                                                  inf5
                                                  neginf4
                                                  period1
                                                  lastObs2
                                                  lastSpeed4
                                              else
                                                lastSpeed3
in
let #var"RTPPL_frontRearDistance" =
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
                            lam intToFloat2.
                              lam maxSpeed2.
                                lam wheelCircumference2.
                                  lam usMaxRange2.
                                    lam maxDistance2.
                                      lam inf3.
                                        lam neginf2.
                                          lam period.
                                            let initSpeed =
                                              run
                                                { particles =
                                                    1000 }
                                                (t12
                                                   nanosPerSec3
                                                   wallLogicalTime3
                                                   intToFloat2
                                                   maxSpeed2
                                                   wheelCircumference2)
                                            in
                                            let lastSpeed =
                                              tsv
                                                nanosPerSec3
                                                wallLogicalTime3
                                                0
                                                initSpeed
                                            in
                                            let lastObs =
                                              ""
                                            in
                                            let lastSpeed1 =
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
                                                intToFloat2
                                                usMaxRange2
                                                maxDistance2
                                                inf3
                                                neginf2
                                                period
                                                lastObs
                                                lastSpeed
                                            in
                                            {}
in
let t13 =
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
                        lam intToFloat1.
                          lam maxSpeed1.
                            lam wheelCircumference1.
                              lam usMaxRange1.
                                lam maxDistance1.
                                  lam inf2.
                                    lam neginf1.
                                      lam #var"1".
                                        #var"RTPPL_frontRearDistance"
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
                                          intToFloat1
                                          maxSpeed1
                                          wheelCircumference1
                                          usMaxRange1
                                          maxDistance1
                                          inf2
                                          neginf1
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
  (t13
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
     intToFloat
     maxSpeed
     wheelCircumference
     usMaxRange
     maxDistance
     inf1
     neginf)