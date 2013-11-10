"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
createOn: aUrl alias: aString withGemStoneFileouts: fileoutBoolean

	^(aUrl repositoryClass new)
		initializeCreationOn: aUrl
			alias: aString
			withGemStoneFileouts: fileoutBoolean;
		yourself
