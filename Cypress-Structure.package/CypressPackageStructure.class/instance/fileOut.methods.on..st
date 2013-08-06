"
notice: This work is protected by copyright. All rights reserved.
category: filing out - private
"
fileOut: aString methods: someMethodStructures on: aStream

	someMethodStructures isEmpty ifTrue: [^self].
	self
		fileOut: aString methodsPreambleFor: someMethodStructures any classStructure on: aStream;
		fileOutMethods: someMethodStructures on: aStream