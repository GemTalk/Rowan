"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: sorting - private
"
transcribeGraph: subtree into: sorted

	subtree keysAndValuesDo: [:name :subsubtree |
		sorted add: (candidateAliasMap at: name).
		self transcribeGraph: subsubtree into: sorted.
	].
