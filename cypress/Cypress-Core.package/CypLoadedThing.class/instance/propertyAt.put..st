"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
propertyAt: propertyName put: aValue
	"Value should be a string"

	self markPackageDirty.
	^properties at: propertyName put: aValue