"
notice: This work is protected by copyright. All rights reserved.
category: sorting - private
"
transcribeGraph: subtree into: sorted

	subtree keysAndValuesDo: [:name :subsubtree |
		sorted add: (candidateAliasMap at: name).
		self transcribeGraph: subsubtree into: sorted.
	].
