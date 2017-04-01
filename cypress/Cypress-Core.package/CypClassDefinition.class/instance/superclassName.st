"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
superclassName

	^self propertyAt: 'superclass' ifAbsent: [self propertyAt: 'super']