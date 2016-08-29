"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
initializeCreationOn: aUrl alias: aString

	self
		initializeUrl: aUrl andAlias: aString;
		alias: aString;
		writePropertiesFile