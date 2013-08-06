"
notice: This work is protected by copyright. All rights reserved.
category: filing out - private
"
fileOut: aString implementationsFrom: someClassStructures on: aStream

	someClassStructures
		do: [:each | self fileOutType: aString implementationOf: each on: aStream]