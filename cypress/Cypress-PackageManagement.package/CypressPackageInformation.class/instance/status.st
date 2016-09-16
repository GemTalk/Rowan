"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
status

	| changes |
	(changes := self changesStatus) isEmpty ifTrue: [^self imageDefinitionsStatus].
	^self imageDefinitionsStatus, changes