"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
privateCreateClassFor: aPatchSet

	| classProperties packageName infoSource category createdClass |
	createdClass := self basicCreateClassFor: aPatchSet.
	classProperties := classDefinition properties.
	packageName := packageDefinition key.
	infoSource := Cypress configuration packageInfoSource.
	category := infoSource == #Category
				ifTrue: [packageName]
				ifFalse: [classProperties at: 'category' ifAbsent: ['']].
	createdClass category: category.
	^createdClass