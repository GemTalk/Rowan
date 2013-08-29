initializing - private
initializeCreationOn: aUrl alias: aString withGemStoneFileouts: fileoutBoolean

	self
		initializeUrl: aUrl andAlias: aString;
		gemStoneFileoutsEnabled: fileoutBoolean;
		alias: aString;
		writePropertiesFile