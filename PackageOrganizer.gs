doit
Object subclass: 'PackageOrganizer'
	instVarNames: #( packages)
	classVars: #()
	classInstVars: #( default)
	poolDictionaries: #()
	inDictionary: ''
	category: 'PackageInfo-Base'
	options: #()

%

! ------------------- Class comment for PackageOrganizer
doit
PackageOrganizer comment: 
''
%

! Remove existing behavior from PackageOrganizer
doit
PackageOrganizer removeAllMethods.
PackageOrganizer class removeAllMethods.
%
! ------------------- Class methods for PackageOrganizer
category: 'as yet unclassified'
set compile_env: 0
classmethod: PackageOrganizer
default
	^ default ifNil: [default _ self new]
%
category: 'as yet unclassified'
set compile_env: 0
classmethod: PackageOrganizer
new
	^ self basicNew initialize
%
! ------------------- Instance methods for PackageOrganizer
category: 'initializing'
set compile_env: 0
method: PackageOrganizer
initialize
	packages _ Dictionary new
%
category: 'searching'
set compile_env: 0
method: PackageOrganizer
noPackageFound
	self error: 'No package found'
%
category: 'searching'
set compile_env: 0
method: PackageOrganizer
packageNamed: aString ifAbsent: errorBlock
	^ packages at: aString ifAbsent: errorBlock
%
category: 'accessing'
set compile_env: 0
method: PackageOrganizer
packageNames
	^ packages keys
%
category: 'searching'
set compile_env: 0
method: PackageOrganizer
packageOfClass: aClass
	^ self packageOfClass: aClass ifNone: [self noPackageFound]
%
category: 'searching'
set compile_env: 0
method: PackageOrganizer
packageOfClass: aClass ifNone: errorBlock
	^ self packages detect: [:ea | ea includesClass: aClass] ifNone: errorBlock
%
category: 'searching'
set compile_env: 0
method: PackageOrganizer
packageOfMethod: aMethodReference
	^ self packageOfMethod: aMethodReference ifNone: [self noPackageFound]
%
category: 'searching'
set compile_env: 0
method: PackageOrganizer
packageOfMethod: aMethodReference ifNone: errorBlock
	^ self packages detect: [:ea | ea includesMethodReference: aMethodReference] ifNone: errorBlock
%
category: 'searching'
set compile_env: 0
method: PackageOrganizer
packageOfMethodCategory: categoryName ofClass: aClass
	^self packageOfMethodCategory: categoryName ofClass: aClass ifNone: [ self noPackageFound ]
%
category: 'searching'
set compile_env: 0
method: PackageOrganizer
packageOfMethodCategory: categoryName ofClass: aClass ifNone: errorBlock
	^ self packages detect: [:ea | ea includesMethodCategory: categoryName ofClassNamed: aClass] ifNone: errorBlock
	
	
%
category: 'searching'
set compile_env: 0
method: PackageOrganizer
packageOfSystemCategory: aSystemCategory
	^ self packageOfSystemCategory: aSystemCategory ifNone: [ self noPackageFound ]
%
category: 'searching'
set compile_env: 0
method: PackageOrganizer
packageOfSystemCategory: aSystemCategory ifNone: errorBlock
	^ self packages detect: [:ea | ea includesSystemCategory: aSystemCategory] ifNone: errorBlock
%
category: 'accessing'
set compile_env: 0
method: PackageOrganizer
packages
	^ packages values
%
category: 'registering'
set compile_env: 0
method: PackageOrganizer
registerPackage: aPackageInfo
	packages at: aPackageInfo packageName put: aPackageInfo.
	self changed: #packages; changed: #packageNames.
%
category: 'registering'
set compile_env: 0
method: PackageOrganizer
registerPackageNamed: aString
	^ self registerPackage: (PackageInfo named: aString)
%
category: 'registering'
set compile_env: 0
method: PackageOrganizer
unregisterPackage: aPackageInfo
	packages removeKey: aPackageInfo packageName ifAbsent: [].	
	self changed: #packages; changed: #packageNames.
%
category: 'registering'
set compile_env: 0
method: PackageOrganizer
unregisterPackageNamed: aString
	self unregisterPackage: (self packageNamed: aString ifAbsent: [^ self])
%
doit
PackageOrganizer category: 'PackageInfo-Base'
%
