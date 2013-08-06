"
notice: This work is protected by copyright. All rights reserved.
category: accessing
"
changesStatus

	^self hasChanges
		ifTrue: [' (' , self changesCount printString , ')']
		ifFalse: ['']