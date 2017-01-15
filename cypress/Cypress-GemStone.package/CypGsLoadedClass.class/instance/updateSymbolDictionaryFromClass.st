"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
updateSymbolDictionaryFromClass

	| dicts newName oldName |
	dicts := self class symbolDictsForClass: handle.
	dicts size = 0
		ifTrue: [self error: 'Class not found by name in symbol list.'].
	dicts size > 1
		ifTrue: 
			[CypNotification
				signal: 'Class resolved in more than one SymbolDictionary. Using first found.'].
	newName := dicts first name.
	oldName := self propertyAt: 'SymbolDictionary'.
	oldName = newName ifFalse: [self symbolDictionaryName: newName]