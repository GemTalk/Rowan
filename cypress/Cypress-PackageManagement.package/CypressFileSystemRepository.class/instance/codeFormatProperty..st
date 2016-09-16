"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: updating properties
"
codeFormatProperty: aString

	self validate: aString isOneOf: #('Cypress' 'FileTree' 'Flexible').
	properties 
		at: '_gs_format'
		put: aString
