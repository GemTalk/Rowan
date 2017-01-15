"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
updateClassInstvarNamesFromClass
	"Order of instvars does not matter to Cypress (at least not in GemStone) so we keep the instvar names sorted."

	| propertyName oldNames newNames |
	propertyName := 'classinstvars'.
	oldNames := self propertyAt: propertyName.
	newNames := (handle class instVarNames collect: [:each | each asString])
				asSortedCollection.
	oldNames = newNames ifFalse: [self propertyAt: propertyName put: newNames]