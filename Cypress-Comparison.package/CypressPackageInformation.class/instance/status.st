"
notice: This work is protected by copyright. All rights reserved.
category: accessing
"
status

	| changes |
	(changes := self changesStatus) isEmpty ifTrue: [^self imageDefinitionsStatus].
	^self imageDefinitionsStatus, changes