"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOut: aString methods: someMethodStructures on: aStream

	someMethodStructures isEmpty ifTrue: [^self].
	self
		fileOut: aString methodsPreambleFor: someMethodStructures any classStructure on: aStream;
		fileOutMethods: someMethodStructures on: aStream
