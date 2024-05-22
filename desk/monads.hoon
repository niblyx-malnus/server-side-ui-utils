|%
++  parser
  |%
  ++  pure  easy
  ++  bind  
    |*  =mold
    |*  [sef=rule gat=$-(mold rule)]
    |=  tub=nail
    =/  vex  (sef tub)
    ?~  q.vex  vex
    ((gat p.u.q.vex) q.u.q.vex)
  :: check if done
  ::
  ++  done
    |=  tub=nail
    ^-  (like ?)
    ?~  q.tub
      [p.tub ~ %.y tub]
    [p.tub ~ %.n tub]
  :: peek ahead
  ::
  ++  near
    |=  tub=nail
    ^-  (like (unit char))
    ?~  q.tub
      [p.tub ~ ~ tub]
    [p.tub ~ `i.q.tub tub]
  ::
  ++  peek
    |*  sef=rule
    |=  tub=nail
    =+  vex=(sef tub)
    ?~  q.vex
      [p=p.vex q=[~ u=[p=~ q=tub]]]
    [p=p.vex q=[~ u=[p=[~ p.u.q.vex] q=tub]]]
  --
--
