"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: filing out - private
"
fileOutMethods: someMethodStructures on: aStream

	(someMethodStructures
		asSortedCollection: [:a :b | a selector <= b selector])
			do: [:methodStructure | self fileOutMethod: methodStructure on: aStream]