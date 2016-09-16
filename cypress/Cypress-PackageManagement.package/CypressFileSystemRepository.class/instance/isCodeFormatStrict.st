"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: testing properties
"
isCodeFormatStrict

	^(properties 
		at: '_gs_strict'
		ifAbsent: ['']) equalsNoCase: 'true'
