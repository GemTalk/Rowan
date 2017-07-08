"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: sorting
"
inOrder
  | sorted sortedRoots |
  sorted := OrderedCollection new.
  self mapCandidatesIntoGraphs.
  sortedRoots := SortedCollection sortBlock: [ :a :b | a key <= b key ].
  self determineGraphRoots associationsDo: [ :assoc | sortedRoots add: assoc ].
  sortedRoots do: [ :assoc | self transcribeGraph: assoc value into: sorted ].
  ^ sorted
