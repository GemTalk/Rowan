"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
category

	^self properties
		at: 'category'
		ifAbsent: [self packageStructure packageName]
