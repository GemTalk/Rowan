"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
handleClassUpdateToVersion: newVersion
	"newVersion is a new or renamed version of the class I represent, but is in the same classHistory. Update myself from the given class."

	| dicts dict |
	dicts := self class symbolDictsForClass: newVersion.
	dicts size = 0
		ifTrue: 
			[self error: 'Internal error. Class not found by name in symbol list.'].
	dicts size > 1
		ifTrue: 
			[CypNotification
				signal: 'Class resolved in more than one SymbolDictionary. Using first found.'].
	dict := dicts first.
	self markPackageDirty.
	self symbolDictionaryName: dict name.
	handle := newVersion.

	"Class name may have changed."
	self name: newVersion name.
	
	"And there might be different superclass, instance variables, etc."
	self updatePropertiesFromClass.

	"This needs to be a more generic method for updating from class for any purpose, not just new version."
	self halt.