"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
resolve: aName ifAbsent: absentBlock
	"This is the GemStone version, which will also work with other platforms which have SUnit in the base."

	| resolution |
	resolution := SUnitNameResolver classNamed: aName.
	^resolution isNil ifTrue: [absentBlock value] ifFalse: [resolution]