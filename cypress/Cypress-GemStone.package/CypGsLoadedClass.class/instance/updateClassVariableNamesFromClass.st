"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
updateClassVariableNamesFromClass
	"Order of variable names does not matter to Cypress (at least not for GemStone) so we keep the names sorted."

	| propertyName oldNames newNames |
	propertyName := 'classvars'.
	oldNames := self propertyAt: propertyName.
	newNames := (handle classVarNames collect: [:each | each asString])
				asSortedCollection.
	oldNames = newNames
		ifFalse: 
			[self markPackageDirty.
			self propertyAt: propertyName put: newNames]