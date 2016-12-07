"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: public
"
packageNamed: aName ifAbsent: absentBlock
	"If I contain a package with the given name, answer a PackageReference to it.
	If not, answer the value of absentBlock."

	^(self packageNames includes: aName)
		ifTrue: [CypPackageReference inRepository: self name: aName]
		ifFalse: [absentBlock value]