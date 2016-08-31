"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
loadClassDefinition: aDefaultSymbolDictionaryName
	"Create a new version of the defined class. If the class already exists,
	 copy the behaviors and state from the old version."

	| newClass oldClass |
        self defaultSymbolDictionaryName: aDefaultSymbolDictionaryName.
	oldClass := self actualClassOrNil.
	newClass := self createOrReviseClass.
	(oldClass isNil or: [newClass == oldClass]) ifTrue: [^self].
	self classNeedingMigration: newClass.
	self
		recompileWithSubclassesFrom: oldClass
		to: newClass
		symbolList: System myUserProfile symbolList.
