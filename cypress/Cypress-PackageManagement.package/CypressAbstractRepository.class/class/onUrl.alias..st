"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
onUrl: aUrl alias: aString

	^(aUrl repositoryClass new)
		initializeUrl: aUrl andAlias: aString;
		yourself