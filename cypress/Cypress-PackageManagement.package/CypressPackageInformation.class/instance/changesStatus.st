"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
changesStatus

	^self hasChanges
		ifTrue: [' (' , self changesCount printString , ')']
		ifFalse: ['']