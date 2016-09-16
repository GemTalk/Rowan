"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: updating properties - private
"
strictCodeFormatProperty: aString

	self validate: aString isOneOf: #('true' 'false').
	properties 
		at: '_gs_strict'
		put: aString
