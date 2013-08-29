updating properties - private
gemStoneFileoutsProperty: aString

	self validate: aString isOneOf: #('true' 'false').
	properties 
		at: '_gs_fileout'
		put: aString
