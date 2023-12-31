Class {
	#name : 'RwRepositoryResolvedProjectTopazPackageReaderVisitorV2',
	#superclass : 'RwRepositoryComponentProjectReaderVisitor',
	#instVars : [
		'filenameExtension'
	],
	#category : 'Rowan-GemStone-Core-36x'
}

{ #category : 'accessing' }
RwRepositoryResolvedProjectTopazPackageReaderVisitorV2 >> filenameExtension [

	^ filenameExtension ifNil: ['gs' ]
]

{ #category : 'accessing' }
RwRepositoryResolvedProjectTopazPackageReaderVisitorV2 >> filenameExtension: aString [

	filenameExtension := aString
]

{ #category : 'package reading' }
RwRepositoryResolvedProjectTopazPackageReaderVisitorV2 >> readPackages: packagesRoot [
	| trace packageNamesRead |
	packageNamesRead := Set new.
	trace := Rowan projectTools trace.
	packagesRoot files
		do: [ :packageFile | 
			packageFile extension = self filenameExtension
				ifFalse: [ 
					trace
						trace:
							'--- skip reading ' , packageFile printString
								, ' not a topaz package file (missing or malformed package.gs file' ]
				ifTrue: [ 
					| packageName |
					packageName := packageFile base.
					(self packageNamesBlock value: packageName)
						ifTrue: [ 
							trace
								trace:
									'--- reading package ' , packageName asString , ' dir ' , packageFile asString.
							GsFileinPackager
								toPackagesForDefinedProject: self currentProjectDefinition
								componentName:
									(self currentProjectDefinition componentForPackageNamed: packageName) name
								fromServerPath: packageFile pathString.
							packageNamesRead add: packageName ]
						ifFalse: [ trace trace: '      skipped readClasses, ' , packageName , ' rejected' ] ] ].
	^ packageNamesRead
]
