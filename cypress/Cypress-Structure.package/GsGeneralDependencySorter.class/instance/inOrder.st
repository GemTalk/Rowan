"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: sorting
"
inOrder

	| sorted |
	sorted := OrderedCollection new.
	self mapCandidatesIntoGraphs.
	self determineGraphRoots
		do: [:each | self transcribeGraph: each into: sorted].
	^sorted.
