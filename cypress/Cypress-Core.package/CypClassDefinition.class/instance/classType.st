"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
classType
	"Legal types are normal, variable, and bytes."

	^self propertyAt: 'type' ifAbsent: ['normal']