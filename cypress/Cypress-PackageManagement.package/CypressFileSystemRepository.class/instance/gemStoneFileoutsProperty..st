"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: updating properties - private
"
gemStoneFileoutsProperty: aString

	self validate: aString isOneOf: #('true' 'false').
	properties 
		at: '_gs_fileout'
		put: aString
