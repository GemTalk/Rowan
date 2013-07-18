doit
(Object subclass: 'MCPackage'
	instVarNames: #( name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()) category: 'Monticello-Base'

%

! ------------------- Class comment for MCPackage
doit
MCPackage comment: 
''
%

! Remove existing behavior from MCPackage
doit
MCPackage removeAllMethods.
MCPackage class removeAllMethods.
%
! ------------------- Class methods for MCPackage
category: 'as yet unclassified'
set compile_env: 0
classmethod: MCPackage
named: aString
	^ self new name: aString
%
! ------------------- Instance methods for MCPackage
category: 'as yet unclassified'
set compile_env: 0
method: MCPackage
= other
	^ other species = self species and: [other name = name]
%
category: 'as yet unclassified'
set compile_env: 0
method: MCPackage
hash
	^ name hash
%
category: 'testing'
set compile_env: 0
method: MCPackage
hasWorkingCopy
	^ MCWorkingCopy registry includesKey: self
%
category: 'as yet unclassified'
set compile_env: 0
method: MCPackage
name
	^ name
%
category: 'as yet unclassified'
set compile_env: 0
method: MCPackage
name: aString
	name _ aString
%
category: 'as yet unclassified'
set compile_env: 0
method: MCPackage
packageInfo
	^ PackageInfo named: name
%
category: 'as yet unclassified'
set compile_env: 0
method: MCPackage
printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: name;
		nextPut: $)
%
category: 'as yet unclassified'
set compile_env: 0
method: MCPackage
snapshot
	| packageInfo definitions categories |
	packageInfo := self packageInfo.
	definitions := OrderedCollection new.
	categories := packageInfo systemCategories.
	categories isEmpty ifFalse: [ definitions add: (MCOrganizationDefinition categories: categories) ].
	packageInfo methods do: [:ea | definitions add: ea asMethodDefinition] displayingProgress: 'Snapshotting methods...'.
	(packageInfo respondsTo: #overriddenMethods) ifTrue:
		[packageInfo overriddenMethods
			do: [:ea | definitions add:
					(packageInfo changeRecordForOverriddenMethod: ea) asMethodDefinition]
			displayingProgress: 'Searching for overrides...'].
	packageInfo classes do: [:ea | definitions addAll: ea classDefinitions] displayingProgress: 'Snapshotting classes...'.
	^ MCSnapshot fromDefinitions: definitions
%
category: 'as yet unclassified'
set compile_env: 0
method: MCPackage
storeOn: aStream
	aStream
		nextPutAll: 'MCPackage';
		space; nextPutAll: 'named: '; store: name.
%
category: 'as yet unclassified'
set compile_env: 0
method: MCPackage
unload
	^ self workingCopy unload
%
category: 'as yet unclassified'
set compile_env: 0
method: MCPackage
workingCopy
	^ MCWorkingCopy forPackage: self.
%
doit
MCPackage category: 'Monticello-Base'
%
