"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
existingForMethod: aMethod ifAbsent: absentBlock
	"Answer the registered LoadedMethod for the given method."

	^MethodRegistry at: aMethod ifAbsent: absentBlock