"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOut: aString implementationsFrom: someClassStructures on: aStream

	someClassStructures
		do: [:each | self fileOutType: aString implementationOf: each on: aStream]