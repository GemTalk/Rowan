"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing
"
writePropertiesFile

	self writePropertiesFile: (self isCodeFormatFileTree
				ifTrue: ['properties.json']
				ifFalse: ['properties.ston'])