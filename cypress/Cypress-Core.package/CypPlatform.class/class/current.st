"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
current
	"Figure out what platform we're running on, then create and answer an instance of my appropriate subclass."

	| platformSymbol |
	platformSymbol := self currentPlatformSymbol.
	^platformSymbol == #GS ifTrue: [self forGemStone]