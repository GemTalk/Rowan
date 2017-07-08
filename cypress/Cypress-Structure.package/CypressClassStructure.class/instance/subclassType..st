"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
subclassType: aString

	aString isEmpty
		ifTrue: [self properties removeKey: '_gs_subclassType' ifAbsent: []]
		ifFalse: [self properties at: '_gs_subclassType' put: aString]
