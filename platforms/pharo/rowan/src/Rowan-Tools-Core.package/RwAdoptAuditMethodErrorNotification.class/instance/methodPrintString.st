accessing
methodPrintString

	^ self 
		className, 
		(self isMetaclass ifTrue: [ ' class >> ' ] ifFalse: [ ' >> ' ]),
		self selector