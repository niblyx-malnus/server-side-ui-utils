/+  manx-utils
|%
:: extract individual classes from a class tape
::
++  de-class
  |=  classes=tape
  ^-  (set tape)
  :: replace all whitespace of any length with ' '
  ::
  =.  classes
    ^-  tape
    %+  scan  classes
    %-  star
    ;~  pose
      (cold ' ' (plus gah))
      next
    ==
  %-  ~(gas in *(set tape))

  (split " " classes)
:: borrowed from ~lagrev-nocfep's string library
::
++  split
  |=  [sep=tape =tape]
  ^-  (list ^tape)
  =|  res=(list ^tape)
  |-
  ?~  tape  (flop res)
  =/  off  (find sep tape)
  ?~  off  (flop [`^tape`tape `(list ^tape)`res])
  %=  $
    res   [(scag `@ud`(need off) `^tape`tape) res]
    tape  (slag +(`@ud`(need off)) `^tape`tape)
  ==
:: 
++  mx
  |_  a=manx
  :: manage root level attributes
  ::
  ++  at
    |%
    :: Get attribute if it exists as a unit
    ::
    ++  get
      |=  n=mane
      ^-  (unit tape)
      ?~  a.g.a
        ~
      ?:  =(n n.i.a.g.a)
        [~ v.i.a.g.a]
      $(a.g.a t.a.g.a)
    :: Get existing or crash
    ::
    ++  got
      |=  n=mane
      (need (get n))
    :: Get with default
    ::
    ++  gut
      |=  [n=mane v=tape]
      (fall (get n) v)
    :: Check for attribute existence
    ::
    ++  has
      |=  n=mane
      !=(~ (get n))
    :: Delete an attribute
    ::
    ++  del
      |=  n=mane
      ^-  manx
      %=    a
          a.g
        |-
        ?~  a.g.a
          ~
        ?:  =(n n.i.a.g.a)
          t.a.g.a
        [i.a.g.a $(a.g.a t.a.g.a)]
      ==
    :: Purge / delete many
    ::
    ++  pur
      |=  n=(set mane)
      ^-  manx
      %=    a
          a.g
        |-
        ?~  a.g.a
          ~
        ?:  (~(has in n) n.i.a.g.a)
          t.a.g.a
        [i.a.g.a $(a.g.a t.a.g.a)]
      ==
    :: Keep only from set
    ::
    ++  kep
      |=  n=(set mane)
      ^-  manx
      %=    a
          a.g
        |-
        ?~  a.g.a
          ~
        ?.  (~(has in n) n.i.a.g.a)
          t.a.g.a
        [i.a.g.a $(a.g.a t.a.g.a)]
      ==
    :: Add a key-value pair (replaces first instance or appends)
    ::
    ++  put
      |=  [n=mane v=tape]
      ^-  manx
      %=    a
          a.g
        |-
        ?~  a.g.a
          [[n v] ~]
        ?:  =(n.i.a.g.a n)
          [[n v] t.a.g.a]
        [i.a.g.a $(a.g.a t.a.g.a)]
      ==
    :: Add many key-value pairs
    ::
    ++  gas
      |=  b=mart
      ^-  manx
      =/  m  (~(gas by *(map mane tape)) b)
      %=    a
          a.g
        |-
        ?~  a.g.a
          ~(tap by m)
        ?~  get=(~(get by m) n.i.a.g.a)
          [i.a.g.a $(a.g.a t.a.g.a)]
        =.  m  (~(del by m) n.i.a.g.a)
        [[n.i.a.g.a u.get] t.a.g.a]
      ==
    :: Add or delete value at key
    ::
    ++  mar
      |=  [n=mane v=(unit tape)] 
      ^-  manx
      ?~  v
        (del n)
      (put n u.v)
    :: Modify value at key
    ::
    ++  jab
      |=  [n=mane f=$-(tape tape)]
      ^-  manx
      (put n (f (fall (get n) ~)))
    :: Prepend attribute value
    ::
    ++  pen
      |=  [n=mane v=tape]
      ^-  manx
      (jab n |=(t=tape (weld v t)))
    :: Extend attribute value
    ::
    ++  ext
      |=  [n=mane v=tape]
      ^-  manx
      (jab n |=(t=tape (weld t v)))
    :: Length of attribute list
    ::
    ++  wyt  (lent a.g.a)
    :: List of values
    ::
    ++  val  (turn a.g.a tail)
    :: List of keys
    ::
    ++  key  (turn a.g.a head)
    :: Transform each value
    ::
    ++  run
      |=  f=$-(tape tape)
      ^-  manx
      %=    a
          a.g
        %+  turn  a.g.a
        |=  [n=mane v=tape]
        [n (f v)]
      ==
    :: Transform value or remove
    ::
    ++  ron
      |=  f=$-(tape (unit tape))
      ^-  manx
      %=    a
          a.g
        %+  murn  a.g.a
        |=  [n=mane v=tape]
        (bind (f v) (lead n))
      ==
    :: Run considering key
    ::
    ++  urn
      |=  f=$-([mane tape] tape)
      ^-  manx
      %=    a
          a.g
        %+  turn  a.g.a
        |=  [n=mane v=tape]
        [n (f n v)]
      ==
    :: Run considering key or remove
    ::
    ++  orn
      |=  f=$-([mane tape] (unit tape))
      ^-  manx
      %=    a
          a.g
        %+  murn  a.g.a
        |=  [n=mane v=tape]
        (bind (f n v) (lead n))
      ==
    :: TrANsform each key-value pair
    ::
    ++  tan
      |=  f=$-([mane tape] [mane tape])
      ^-  manx
      a(a.g (turn a.g.a f))
    :: Transform or remove
    ::
    ++  ton
      |=  f=$-([mane tape] (unit [mane tape]))
      ^-  manx
      a(a.g (murn a.g.a f))
    :: Replace directly
    ::
    ++  new
      |=  b=mart
      ^-  manx
      a(a.g b)
    :: Transform / ChANge entirely
    ::
    ++  can
      |=  f=$-(mart mart)
      ^-  manx
      a(a.g (f a.g.a))
    ::
    ++  all
      |=  f=$-(tape ?)
      (levy (turn a.g.a tail) f)
    ::
    ++  lal
      |=  f=$-([mane tape] ?)
      (levy a.g.a f)
    ::
    ++  any
      |=  f=$-(tape ?)
      (lien (turn a.g.a tail) f)
    ::
    ++  yan
      |=  f=$-([mane tape] ?)
      (lien a.g.a f)
    --
  :: An address is a list of knots which can be parsed to a @ud.
  :: This allows us to manually enter paths with e.g. /3/6/5/0.
  ::
  :: Get the manx at index (slav %ud n) of a marl if it exists.
  ::
  ++  wag
    |=  [n=@ta c=marl]
    ^-  (unit [i=@ud m=manx])
    =/  i=@ud  (slav %ud n)
    ?~  w=(swag [i 1] c)
      ~
    [~ i i.w]
  :: Get manx at tree address
  :: 
  ++  get
    |=  p=path
    ^-  (unit manx)
    ?~  p
      [~ a]
    ?~  w=(wag i.p c.a)
      ~
    $(p t.p, a m.u.w)
  :: Get contents
  ::
  ++  gec
    |=  p=path
    ^-  (unit marl)
    ?~  m=(get p)
      ~
    [~ c.u.m]
  :: Put new manx at address
  ::
  ++  put
    |=  [p=path m=manx]
    ^-  manx
    ?~  p
      m
    ?~  w=(wag i.p c.a)
      a
    a(c (snap c.a i.u.w $(p t.p, a m.u.w)))
  :: Put new contents in manx at address
  ::
  ++  puc
    |=  [p=path m=marl]
    ^-  manx
    ?~  g=(get p)
      a
    (put p [g.u.g m])
  :: Put new manx just before first child (set-after-begin)
  ::
  ++  sab
    |=  [p=path m=manx]
    ^-  manx
    ?~  g=(get p)
      a
    (put p [g.u.g m c.u.g])
  :: Put new manx just after last child (set-before-end)
  ::
  ++  sbe
    |=  [p=path m=manx]
    ^-  manx
    ?~  g=(get p)
      a
    (put p [g.u.g (weld c.u.g m ~)])
  :: Put new manx just before this one in its parent (set-before-begin)
  ::
  ++  sbb
    |=  [p=path m=manx]
    ^-  manx
    ?~  p
      a
    =/  q  (snip `path`p)
    ?~  g=(get q)
      a
    =/  i=@ud  (slav %ud (rear p))
    (put q [g.u.g (into c.u.g i m)])
  :: Put new manx just after this one in its parent (set-after-end)
  ::
  ++  sae
    |=  [p=path m=manx]
    ^-  manx
    ?~  p
      a
    =/  q  (snip `path`p)
    ?~  g=(get q)
      a
    =/  i=@ud  (slav %ud (rear p))
    (put q [g.u.g (into c.u.g +(i) m)])
  :: children
  ::
  ++  kid
    |=  p=path
    ^-  (list (pair path manx))
    ?~  g=(get p)
      ~
    =|  i=@
    |-
    ?~  c.u.g
      ~
    :_  $(i +(i), c.u.g t.c.u.g)
    [(snoc p (scot %ud i)) i.c.u.g]
  :: first child
  ::
  ++  kad
    |=  p=path
    ^-  (unit (pair path manx))
    ?~  g=(get p)
      ~
    ?~  c.u.g
      ~
    [~ (snoc p ~.0) i.c.u.g]
  :: last child
  ::
  ++  kud
    |=  p=path
    ^-  (unit (pair path manx))
    ?~  g=(get p)
      ~
    ?~  c.u.g
      ~
    :-  ~  :_  (rear c.u.g)
    (snoc p (scot %ud (dec (lent c.u.g))))
  :: Previous sibling
  ::
  ++  pes
    |=  p=path
    ^-  (unit (pair path manx))
    ?~  p
      ~
    =/  i=@ud  (slav %ud (rear p))
    ?:  =(i 0)
      ~
    =/  q  (snoc (snip `path`p) (scot %ud (dec i)))
    ?~  g=(get q)
      ~
    [~ q u.g]
  :: Previous sibling
  ::
  ++  nes
    |=  p=path
    ^-  (unit (pair path manx))
    ?~  p
      ~
    =/  i=@ud  (slav %ud (rear p))
    =/  q  (snoc (snip `path`p) (scot %ud +(i)))
    ?~  g=(get q)
      ~
    [~ q u.g]
  :: Depth-first search for first element with id (assumes id uniqueness)
  ::
  ++  gid
    =|  p=path
    |=  id=tape
    ^-  (unit [path manx])
    =/  v
      ?~  g=(get:at %id)
        ~
      ?.  =(id u.g)
        ~
      [~ p a]
    ?^  v
      v
    =|  i=@ud
    |-
    ?~  c.a
      ~
    =/  w  ^$(a i.c.a, p (snoc p (scot %ud i)))
    ?^  w
      w
    $(c.a t.c.a)
  :: Get elements by tag name
  ::
  ++  gat
    =|  p=path
    |=  n=mane
    ^-  (list [path manx])
    =/  v=(list [path manx])
      ?.(=(n n.g.a) ~ [p a]~)
    =|  i=@ud
    |-
    ?~  c.a
      v
    =.  v
      %+  weld  `(list [path manx])`v
      ^$(a i.c.a, p (snoc p (scot %ud i)))
    $(c.a t.c.a)
  :: Get elements by class name
  ::
  ++  gac
    =|  p=path
    |=  c=tape
    ^-  (list [path manx])
    =/  v=(list [path manx])
      ?~  g=(get:at %class)
        ~
      ?.  (~(has in (de-class u.g)) c)
        ~
      [p a]~
    =|  i=@ud
    |-
    ?~  c.a
      v
    =.  v
      %+  weld  `(list [path manx])`v
      ^$(a i.c.a, p (snoc p (scot %ud i)))
    $(c.a t.c.a)
  :: preorder concatenation of descendant text
  ::
  ++  text-content  (zing ~(pre-get-text manx-utils a))
  ::
  ++  inner-text                     !! :: deals with CSS hidden stuff
  ++  query-selector                 !! :: returns manx     CSS selector
  ++  query-selector-all             !! :: returns marl (?) CSS selector
  :: nearest ancestor matching CSS selector
  ::
  ++  closest                        !! :: returns manx     CSS selector
  :: aliases
  ::
  ++  get-attribute               get:at
  ++  set-attribute               put:at
  ++  modify-attribute            jab:at
  ++  prepend-attribute           pen:at
  ++  extend-attribute            ext:at
  ++  remove-attribute            del:at
  ++  set-id                      (cury set-attribute %id)
  ++  modify-id                   (cury modify-attribute %id)
  ++  prepend-id                  (cury prepend-attribute %id)
  ++  extend-id                   (cury extend-attribute %id)
  ++  set-class                   (cury set-attribute %class)
  ++  modify-class                (cury modify-attribute %class)
  ++  prepend-class               (cury prepend-attribute %class)
  ++  extend-class                (cury extend-attribute %class)
  ++  set-style                   (cury set-attribute %style)
  ++  modify-style                (cury modify-attribute %style)
  ++  prepend-style               (cury prepend-attribute %style)
  ++  extend-style                (cury extend-attribute %style)
  ++  get-outer-html              get
  ++  set-outer-html              put
  ++  get-inner-html              gec
  ++  set-inner-html              puc
  ++  set-after-begin             sab
  ++  set-before-end              sbe
  ++  set-before-begin            sbb
  ++  set-after-end               sae
  ++  children                    kid
  ++  first-child                 kad
  ++  last-child                  kud
  ++  previous-sibling            pes
  ++  next-sibling                nes
  ++  get-element-by-id           gid
  ++  get-elements-by-tag-name    gat
  ++  get-elements-by-class-name  gac
  --
--
