"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
description

	self isKnown ifTrue: [^self savedLocation].
	self isUnknown ifTrue: [^' <unknown>'].
	^' <', advice, '>'