"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-GemStoneCompatibilityLayer
"
printOn: aStream

	| classOrNil selectorOrNil |
	aStream
		nextPutAll: self class name;
		space.
	self isMethodForBlock ifTrue: [aStream nextPutAll: '[] in '].
	classOrNil := self inClass.
	selectorOrNil := self selector.
	aStream
		nextPutAll: (classOrNil == nil ifTrue: ['<nil>'] ifFalse: [classOrNil name]);
		nextPutAll: '>>';
		nextPutAll: (selectorOrNil == nil
					ifTrue: ['<anonymous>']
					ifFalse: [selectorOrNil])