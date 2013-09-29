"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
initializeCreationOn: aUrl alias: aString withGemStoneFileouts: fileoutBoolean

	self
		initializeUrl: aUrl andAlias: aString;
		gemStoneFileoutsEnabled: fileoutBoolean;
		alias: aString;
		writePropertiesFile