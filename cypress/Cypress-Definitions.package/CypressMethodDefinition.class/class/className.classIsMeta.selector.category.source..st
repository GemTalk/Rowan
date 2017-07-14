"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource

	^self new
		className: aName asString
		classIsMeta: isMetaclass
		selector: aSelector asString
		category: aCategory asString
		source: (self normalizeLineEndingsOf: aSource)
