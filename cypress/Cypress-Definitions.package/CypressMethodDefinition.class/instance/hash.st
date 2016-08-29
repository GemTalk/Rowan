"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
hash

	| hash |
	hash := classIsMeta asString hash.
	hash := source hash bitOr: hash.
	hash := category hash bitOr: hash.
	hash := className hash bitOr: hash.
	^hash