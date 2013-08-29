testing properties
areGemStoneFileoutsEnabled

	^(properties 
		at: '_gs_fileout'
		ifAbsent: ['']) equalsNoCase: 'true'
