"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: testing properties
"
areGemStoneFileoutsEnabled

	^(properties 
		at: '_gs_fileout'
		ifAbsent: ['']) equalsNoCase: 'true'
