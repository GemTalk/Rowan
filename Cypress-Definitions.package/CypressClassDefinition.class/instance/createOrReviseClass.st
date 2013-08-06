loading
createOrReviseClass

	^self subclassType = ''
		ifTrue: [self createOrReviseRegularClass]
		ifFalse: 
			[self subclassType = 'byteSubclass'
				ifTrue: [self createOrReviseByteClass]
				ifFalse: 
					[self subclassType = 'indexableSubclass'
						ifTrue: [self createOrReviseIndexableClass]
						ifFalse: 
							[self error: 'unknown subclass type: ' , self subclassType printString]]]