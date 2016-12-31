"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
updateInstvarNamesFromClass
	"Order of instvars does not matter to Cypress (at least not in GemStone) so we keep the instvar names sorted."

	| propertyName oldNames newNames |
	propertyName := 'instvars'.
	oldNames := self propertiesAt: propertyName.
	newNames := (handle instVarNames collect: [:each | each asString])
				asSortedCollection.
	oldNames = newNames
		ifFalse: 
			[self markPackageDirty.
			self propertiesAt: propertyName put: newNames]