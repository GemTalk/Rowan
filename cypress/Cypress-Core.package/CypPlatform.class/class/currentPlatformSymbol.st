"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
currentPlatformSymbol
	"Answer a Symbol indicating which flavor of Smalltalk we're running in."

	(Object class canUnderstand: #_topazFileoutDefn) ifTrue: [^#GS].
	self error: 'We appear to be running on an unsupported platform'