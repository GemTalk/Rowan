"
notice: This work is protected by copyright. All rights reserved.
category: filing out - private
"
fileOutMethods: someMethodStructures on: aStream

	(someMethodStructures
		asSortedCollection: [:a :b | a selector <= b selector])
			do: [:methodStructure | self fileOutMethod: methodStructure on: aStream]