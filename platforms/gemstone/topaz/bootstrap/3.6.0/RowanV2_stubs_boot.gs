! Class extensions for 'Behavior'

!		Instance methods for 'Behavior'

category: '*rowan-gemstone-kernel-stubs-36x'
method: Behavior
_rwCompileMethodForConditionalPackaging: sourceString symbolList: symList category: categ environmentId: environmentId ifUnpackagedDo: unpackagedBlock
	"Stub method for use when Rowan is NOT installed. Rowan overrides this method to include additional functionality required by Rowan"

	^ unpackagedBlock value
%

category: 'Updating the Method Dictionary'
method: Behavior
_rwRemoveAllMethods: baseMeths enironmentId: envId
	"Stub method for use when Rowan is NOT installed. Rowan overrides this method to include additional functionality required by Rowan"

	envId == 0
		ifFalse: [ 
			"only set time stamps for methods in envId 0"
			^ self ].
	baseMeths
		keysAndValuesDo: [ :sel :meth | 
			self setStamp: nil forMethod: sel ].
%

! Class extensions for 'Class'

!		Instance methods for 'Class'

category: 'Private'
method: Class
_rwCreateSubclass: aString instVarNames: anArrayOfInstvarNames classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary newVersionOf: oldClass description: aDescription options: optionsArray ifUnpackagedDo: unpackagedBlock
	"Stub method for use when Rowan is NOT installed. Rowan overrides this method to include additional functionality required by Rowan"

^ unpackagedBlock value
%
