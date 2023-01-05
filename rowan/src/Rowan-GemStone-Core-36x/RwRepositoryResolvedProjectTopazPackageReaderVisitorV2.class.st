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
	| trace |
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
					trace
						trace:
							'--- reading package ' , packageName asString , ' dir ' , packageFile asString.
					(self packageNamesBlock value: packageName)
						ifTrue: [ 
							self gsFileinPackagerClass
								toPackagesForDefinedProject: self currentProjectDefinition
								componentName:
									(self currentProjectDefinition componentForPackageNamed: packageName) name
								fromServerPath: packageFile pathString ]
						ifFalse: [ trace trace: '      skipped readClasses, packageName rejected' ] ] ]
]
