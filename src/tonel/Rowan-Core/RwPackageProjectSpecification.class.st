"
RwPackageLoadSpecification new
    specName: 'rowanLoadSpec';
    packageNames:
        #('Cypress-Core' 'Rowan-Core' 'Rowan-GemStone' 'Rowan-Tests' 'Rowan-Tode');
    packageNamesToLoad:
        #('Cypress-Core' 'Rowan-Core' 'Rowan-GemStone' 'Rowan-Tests');
    repositoryUrl:
        'tonel:/export/foos1/users/dhenrich/dev/_home/shared/repos/rowan/CypressReferenceImplementation/tonel/';
    exportToUrl:
        'file:/export/foos1/users/dhenrich/dev/_home/shared/repos/rowan/CypressReferenceImplementation/'
"
Class {
	#name : 'RwPackageProjectSpecification',
	#superclass : 'RwProjectSpecification',
	#instVars : [
		'packageNames',
		'packageNamesInLoadOrder',
		'packageNamesToLoad'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwPackageProjectSpecification >> imageSpec [

	^ imageSpec ifNil: [ imageSpec := RwPackageProjectImageSpecification new ]
]

{ #category : 'actions' }
RwPackageProjectSpecification >> packageNamesForTool: aRwTool [
  ^ aRwTool packageNamesForPackageSpec: self
]

{ #category : 'accessing' }
RwPackageProjectSpecification >> packageNamesInLoadOrder [

   ^packageNamesInLoadOrder

]

{ #category : 'accessing' }
RwPackageProjectSpecification >> packageNamesInLoadOrder: anObject [

   packageNamesInLoadOrder := anObject

]

{ #category : 'accessing' }
RwPackageProjectSpecification >> packageNamesToLoad: anObject [

   packageNamesToLoad := anObject

]

{ #category : 'accessing' }
RwPackageProjectSpecification >> packageOrGroupNamesToLoad: packageOrGroupNames [

   packageNames := packageOrGroupNames

]

{ #category : 'copying' }
RwPackageProjectSpecification >> postCopy [
  "don't forget to copy any compound state objects"

  super postCopy.
  packageNames := packageNames copy.
  packageNamesToLoad := packageNamesToLoad copy
]
