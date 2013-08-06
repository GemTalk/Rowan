"
notice: This work is protected by copyright. All rights reserved.
category: accessing
"
description

	self isKnown ifTrue: [^self savedLocation].
	self isUnknown ifTrue: [^' <unknown>'].
	^' <', advice, '>'