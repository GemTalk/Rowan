"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: sorting - private
"
transcribeGraph: subtree into: sorted
  (subtree keys asSortedCollection: [ :a :b | a <= b ])
    do: [ :name | | subsubtree |
      subsubtree := subtree at: name.
      sorted add: (candidateAliasMap at: name).
      self transcribeGraph: subsubtree into: sorted ]
