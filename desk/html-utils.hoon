/+  manx-utils, monads
|%
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
    ::
    ++  gid  (get %id)
    ++  gac  (get %class)
    ++  ges  (get %style)
    ++  gen  (get %name)
    ++  gev  (get %value)
    :: Put id
    ::
    ++  pid  |=(i=tape (put:at %id i))
    :: Put class(es)
    ::
    ++  pac
      |=  t=tape
      =/  c  (parse-classes (gut:at %class "")) :: current
      =/  n  (parse-classes t)                  :: new
      (put:at %class (zing (join " " (weld c n))))
    :: Delete class(es)
    ::
    ++  dac
      |=  t=tape
      =/  c  (parse-classes (gut:at %class "")) :: current
      =/  n  (sy (parse-classes t))             :: new
      =.  c
        |-
        ?~  c
          ~
        ?:  (~(has in n) i.c)
          $(c t.c)
        [i.c $(c t.c)]
      (put:at %class (zing (join " " c)))
    :: Put style
    ::
    ++  pus  |=(s=tape (put:at %style s))
    :: Put name
    ::
    ++  pun  |=(n=tape (put:at %name n))
    :: Put value
    ::
    ++  puv  |=(v=tape (put:at %value v))
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
  :: Get existing or crash
  ::
  ++  got
    |=  p=path
    (need (get p))
  :: Get with default
  ::
  ++  gut
    |=  [p=path m=manx]
    (fall (get p) m)
  :: Check for existence
  ::
  ++  has
    |=  p=path
    !=(~ (get p))
  :: Get contents
  ::
  ++  gec
    |=  p=path
    ^-  (unit marl)
    ?~  m=(get p)
      ~
    [~ c.u.m]
  :: Get existing contents or crash
  ::
  ++  goc
    |=  p=path
    (need (gec p))
  :: Get with default
  ::
  ++  guc
    |=  [p=path m=marl]
    (fall (gec p) m)
  :: Check contents existence
  ::
  ++  hac
    |=  p=path
    !=(~ (gec p))
  :: Put new manx at address (must be existing)
  ::
  ++  put
    |=  [p=path m=manx]
    ^-  manx
    ?~  p
      m
    ?~  w=(wag i.p c.a)
      a
    ?>  (lth i.u.w (lent c.a))
    a(c (snap c.a i.u.w $(p t.p, a m.u.w)))
  :: Put new contents in manx at address
  ::
  ++  puc
    |=  [p=path c=marl]
    ^-  manx
    ?~  p
      a(c c)
    ?~  w=(wag i.p c.a)
      a
    ?>  (lth i.u.w (lent c.a))
    a(c (snap c.a i.u.w $(p t.p, a m.u.w)))
  :: Delete a manx at an address (must be existing)
  ::
  ++  del
    |=  p=path
    ^-  manx
    ?~  p  !!
    ?~  w=(wag i.p c.a)
      a
    ?>  (lth i.u.w (lent c.a))
    ?~  t.p
      a(c (oust [i.u.w 1] c.a))
    a(c (snap c.a i.u.w $(p t.p, a m.u.w)))
  :: Delete contents of manx at an address
  ::
  ++  rem
    |=  p=path
    (puc p ~)
  :: Put or delete manx at path
  ::
  ++  mar
    |=  [p=path m=(unit manx)] 
    ^-  manx
    ?~  m
      (del p)
    (put p u.m)
  :: Put new manx just before first child (set-after-begin)
  ::
  ++  sab
    |=  [p=path m=manx]
    ^-  manx
    ?~  g=(get p)
      a
    (puc p [m c.u.g])
  :: Put new manx just after last child (set-before-end)
  ::
  ++  sbe
    |=  [p=path m=manx]
    ^-  manx
    ?~  g=(get p)
      a
    (puc p (weld c.u.g m ~))
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
    (puc q (into c.u.g i m))
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
    (puc q (into c.u.g +(i) m))
  :: children satisfying some condition
  ::
  ++  kiz
    =|  i=@
    |=  f=$-([path manx] ?)
    ^-  (list (pair path manx))
    ?~  c.a
      ~
    =/  q  /(scot %ud i)
    ?.  (f q i.c.a)
      $(i +(i), c.a t.c.a)
    :-  [q i.c.a]
    $(i +(i), c.a t.c.a)
  :: nth child that satisfies some condition
  ::
  ++  kid
    =|  i=@
    =/  j=@  1 :: 1-indexed
    |=  [n=@ud f=$-([path manx] ?)]
    ^-  (unit (pair path manx))
    ?~  c.a
      ~
    =/  q  /(scot %ud i)
    ?.  (f q i.c.a)
      $(i +(i), c.a t.c.a)
    ?:  =(n j)
      [~ q i.c.a]
    $(j +(j), i +(i), c.a t.c.a)
  :: nth last child that satisfies some condition
  ::
  ++  kib
    |=  [n=@ud f=$-([path manx] ?)]
    =.  c.a  (flop c.a)
    (kid n f)
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
  :: Get a list of (pair path manx) which satisfy some condition
  :: All descendants (including self)
  :: level-order
  :: 
  ++  wic
    =|  p=path
    |=  f=$-([path manx] ?)
    |^  ^-  (list (pair path manx))
    %+  weld
      ?.((f p a) ~ [p a]~)
    (cloop-a (en-path p c.a))
    ::
    ++  en-path
      =|  i=@ud
      |=  [p=path c=marl]
      ^-  (list (pair path manx))
      ?~  c
        ~
      :_  $(c t.c)
      [(snoc p (scot %ud i)) i.c]
    ::
    ++  cloop-a
      |=  c=(list (pair path manx))
      ^-  (list (pair path manx))
      =/  l  c
      |-
      ^-  (list (pair path manx))
      ?~  l
        (cloop-b c)
      %+  weld
        ?.((f i.l) ~ [i.l ~])
      $(l t.l)
    ::
    ++  cloop-b
      |=  c=(list (pair path manx))
      ^-  (list (pair path manx))
      ?~  c
        ~
      %+  weld
        (cloop-a (en-path [p c.q]:i.c))
      $(c t.c)
    --
  :: Get first (pair path manx) which satisfies some condition
  :: if it exists
  :: All descendants (including self)
  :: level-order
  ::
  ++  wif
    =|  p=path
    |=  f=$-([path manx] ?)
    |^  ^-  (unit (pair path manx))
    ?:  (f p a)
      [~ p a]
    (cloop-a (en-path p c.a))
    ::
    ++  en-path
      =|  i=@ud
      |=  [p=path c=marl]
      ^-  (list (pair path manx))
      ?~  c
        ~
      :_  $(c t.c)
      [(snoc p (scot %ud i)) i.c]
    ::
    ++  cloop-a
      |=  c=(list (pair path manx))
      ^-  (unit (pair path manx))
      =/  l  c
      |-
      ?~  l
        (cloop-b c)
      ?:  (f i.l)
        [~ i.l]
      $(l t.l)
    ::
    ++  cloop-b
      |=  c=(list (pair path manx))
      ^-  (unit (pair path manx))
      ?~  c
        ~
      ?^  u=(cloop-a (en-path [p c.q]:i.c))
        u
      $(c t.c)
    --
  :: Get a list of (pair path manx) which satisfy some condition
  :: All ancestors (including self)
  ::
  ++  wac
    |=  $:  p=path :: location in tree
            f=$-([path manx] ?)
        ==
    ^-  (list (pair path manx))
    ?~  p
      ?.((f p a) ~ [p a]~)
    =/  m=manx  (got p)
    %+  weld
      ?.((f p m) ~ [p m]~)
    $(p (snip `path`p))
  :: Get first (pair path manx) which satisfies some condition
  :: if it exists
  :: All ancestors (including self)
  ::
  ++  waf
    |=  $:  p=path :: location in tree
            f=$-([path manx] ?)
        ==
    ^-  (unit (pair path manx))
    ?~  p
      ?.((f p a) ~ [~ p a])
    =/  m=manx  (got p)
    ?:  (f p m)
      [~ p m]
    $(p (snip `path`p))
  :: Conditions
  ::
  ++  con
    =<  con
    |%
    +$  con  $-([path manx] ?)
    :: Negate condition
    ::
    ++  not
      |=  f=con
      ^-  con
      |=  [p=path m=manx]
      ^-  ?
      !(f p m)
    :: Is tag
    ::
    ++  tag
      |=  n=mane
      |=  [* m=manx] 
      =(n n.g.m)
    :: Is id
    ::
    ++  sid
      |=  i=tape
      |=  [* m=manx] 
      =/  u=(unit tape)
        (get:~(at mx m) %id)
      &(?=(^ u) =(i u.u))
    :: Has class
    ::
    ++  cas
      |=  c=tape
      |=  [* m=manx] 
      %.  c
      %~  has  in
      %-  sy  %-  parse-classes
      (gut:~(at mx m) %class "")
    :: Has attribute
    ::
    ++  tar
      |=  n=mane
      |=  [* m=manx] 
      (has:~(at mx m) n)
    :: Is attribute
    ::
    ++  tir
      |=  [n=mane v=tape]
      |=  [* m=manx] 
      =([~ v] (get:~(at mx m) n))
    :: Is text node
    ::
    ++  tex
      |=  [* m=manx] 
      &(((tag %$) +<) ((tar %$) +<))
    :: Attribute starts with
    ::
    ++  sat
      |=  [n=mane v=tape]
      |=  [* m=manx] 
      =/  u=(unit tape)
        (get:~(at mx m) n)
      &(?=(^ u) =(v (scag (lent v) u.u)))
    :: Attribute ends with
    ::
    ++  eat
      |=  [n=mane v=tape]
      |=  [* m=manx] 
      =/  u=(unit tape)
        (get:~(at mx m) n)
      &(?=(^ u) =((flop v) (scag (lent v) (flop u.u))))
    :: Attribute contains
    ::
    ++  cat
      |=  [n=mane v=tape]
      |=  [* m=manx] 
      =/  u=(unit tape)
        (get:~(at mx m) n)
      &(?=(^ u) ?=(^ (find v u.u)))
    :: Empty (no children)
    ::
    ++  emp  |=([* m=manx] =(~ c.m))
    --
  :: Some common getters
  ::
  :: Get element by id
  ::
  ++  gid  |=(i=tape (wif (sid:con i)))
  :: Get elements by tag name
  ::
  ++  gag  |=(n=mane (wic (tag:con n)))
  :: Get elements by class name
  ::
  ++  gac  |=(c=tape (wic (cas:con c)))
  :: Get elements with attribute
  ::
  ++  gat  |=(n=mane (wic (tar:con n)))
  :: Get elements by attribute
  ::
  ++  git  |=([n=mane v=tape] (wic (tir:con n v)))
  :: First descendant with given name attribute
  ::
  ++  gan  |=(v=tape (wif (tir:con %name v)))
  :: Value attribute of first descendant with given name attribute
  ::
  ++  val  |=(v=tape ?~(m=(gan v) ~ (get:~(at mx q.u.m) %value)))
  :: preorder concatenation of descendant text
  ::
  ++  all-text-content  (zing ~(pre-get-text manx-utils a))
  ::
  ++  inner-text                     !! :: deals with CSS hidden stuff
  ++  query-selector                 !! :: returns manx     CSS selector
  ++  query-selector-all             !! :: returns marl (?) CSS selector
  :: nearest ancestor matching CSS selector
  ::
  ++  closest                        !! :: returns manx     CSS selector
  :: aliases
  ::
  ++  get-attribute                    get:at
  ++  set-attribute                    put:at
  ++  modify-attribute                 jab:at
  ++  prepend-attribute                pen:at
  ++  extend-attribute                 ext:at
  ++  remove-attribute                 del:at
  ++  get-id                           gid:at
  ++  set-id                           pid:at
  ++  modify-id                        (cury jab:at %id)
  ++  prepend-id                       (cury pen:at %id)
  ++  extend-id                        (cury ext:at %id)
  ++  get-class                        gac:at
  ++  set-class                        pac:at
  ++  modify-class                     (cury jab:at %class)
  ++  prepend-class                    (cury pen:at %class)
  ++  extend-class                     (cury ext:at %class)
  ++  get-style                        ges:at
  ++  set-style                        pus:at
  ++  modify-style                     (cury jab:at %style)
  ++  prepend-style                    (cury pen:at %style)
  ++  extend-style                     (cury ext:at %style)
  ++  get-outer-html                   get
  ++  set-outer-html                   put
  ++  get-inner-html                   gec
  ++  set-inner-html                   puc
  ++  delete-node                      del
  ++  remove-inner-html                rem
  ++  set-after-begin                  sab
  ++  set-before-end                   sbe
  ++  set-before-begin                 sbb
  ++  set-after-end                    sae
  ++  children                         (kiz _%.y)
  ++  nth-child                        (curr kid _%.y)
  ++  nth-last-child                   (curr kib _%.y)
  ++  first-child                      (kid 1 _%.y)
  ++  last-child                       (kib 1 _%.y)
  ++  previous-sibling                 pes
  ++  next-sibling                     nes
  ++  get-element-by-id                gid
  ++  get-elements-by-tag-name         gag
  ++  get-elements-by-class-name       gac
  ++  get-elements-with-attribute      gat
  ++  get-elements-by-attribute        git
  ++  get-first-element-by-name        gan
  ++  get-first-value-by-name          val
  ++  get-descendants-by               wic
  ++  get-first-descendant-by          wif
  ++  get-ancestors-by                 wac
  ++  get-first-ancestor-by            waf
  ++  is-tag                           tag:con
  ++  is-id                            sid:con
  ++  has-class                        cas:con
  ++  has-attribute                    tar:con
  ++  is-attribute                     tir:con
  ++  attribute-starts-with            sat:con
  ++  attribute-ends-with              eat:con
  ++  attribute-contains               cat:con
  ++  empty                            emp:con
  ::
  ++  first-child-of-type     |=(t=mane (kid 1 (tag:con t)))
  ++  last-child-of-type      |=(t=mane (kib 1 (tag:con t)))
  ++  nth-child-of-type       |=([n=@ud t=mane] (kid n (tag:con t)))
  ++  nth-last-child-of-type  |=([n=@ud t=mane] (kib n (tag:con t)))
  --
:: 
++  parse-classes
  =<  parse
  =,  parser:monads
  |%
  ++  parse  |=(=tape (fall (rust tape classes) ~))
  ++  class
    =/  gah=(set @t)  (sy [`@t`10 ' ' ~])
    =|  class=tape
    |-
    ;<  c=(unit char)  bind  near :: peek at next char; don't consume
    ?~  c :: if we've fully parsed, return the class so far
      (easy class)
    ?:  (~(has in gah) u.c) :: if next char is whitespace, finish
      (easy class)
    ;<  *  bind  next :: actually consume next char
    $(class (weld class u.c ~)) :: add character and repeat to check next
  ::
  ++  classes
    =|  classes=(list tape)
    |-
    ;<  *           bind  (star gah) :: parse any whitespace
    ;<  class=tape  bind  class      :: parse a class
    =?  classes  ?=(^ class)         :: add non-empty class
      [class classes]
    ;<  d=?  bind  done :: check if fully parsed and return / continue
    ?.  d
      $
    (easy (flop classes)) 
  --
--
