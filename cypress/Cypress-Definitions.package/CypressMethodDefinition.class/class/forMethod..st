"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
forMethod: aGsNMethod

	| behavior selector |
	behavior := aGsNMethod inClass.
	selector := aGsNMethod selector.
	^self new
		className: behavior theNonMetaClass name asString
		classIsMeta: behavior isMeta
		selector: selector asString
		category: (behavior categoryOfSelector: selector) asString
		source: (self normalizeLineEndingsOf: aGsNMethod sourceString asString)