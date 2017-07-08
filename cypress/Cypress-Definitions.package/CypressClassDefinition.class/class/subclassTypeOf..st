"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
subclassTypeOf: aClass
	"Answer a description of the argument to identify whether it is a regular class,
	 a byte subclass, or an indexable subclass."

	^(aClass isBytes and: [aClass superClass isBytes not])
		ifTrue: ['byteSubclass']
		ifFalse: 
			[(aClass isIndexable and: [aClass superClass isIndexable not])
				ifTrue: ['indexableSubclass']
				ifFalse: ['']]