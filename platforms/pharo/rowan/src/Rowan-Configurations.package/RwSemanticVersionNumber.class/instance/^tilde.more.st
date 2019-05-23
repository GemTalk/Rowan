comparing
~> aMetacelloVersionNumber

	"if this selector is to survive it will need work ... see RwGemStoneVersionNumber ... I think that collapseZeroes will be needed (in some form) to 
		make this boy give expected results"

	aMetacelloVersionNumber size == 1 ifTrue: [ ^false ].
	^self >= aMetacelloVersionNumber and: [ self < aMetacelloVersionNumber approximateBase ]
