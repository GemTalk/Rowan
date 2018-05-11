tests
testMap
  | map ston |
  (map := STON mapClass new)
    at: #'foo' put: 1;
    at: #'bar' put: 2.
  ston := self serialize: map.
  self assert: (ston = '{#foo:1,#bar:2}' or: [ ston = '{#bar:2,#foo:1}' ]).
  self assert: (self serialize: STON mapClass new) = '{}'.
  map removeAll.
  map at: 'foo bar' put: #'ok'.
  self assert: (self serialize: map) = '{''foo bar'':#ok}'.
  map removeAll.
  map at: 123 put: 456.
  self assert: (self serialize: map) = '{123:456}'