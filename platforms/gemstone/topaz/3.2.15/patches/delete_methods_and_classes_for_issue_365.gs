! commented out symbols are needed by either the Rowan audit tool or the audit blocks
  run
	{
		{ Behavior . 
			{
					#persistentSuperclassForEnv: . #persistentSuperclassForEnv:put: . #moveMethod:toCategory:environmentId: . 
"
					#rowanPackageName . #rowanProjectName . 
"
					#rwCompileMethod:category: . #rwCompileMethod:category:packageName: .
					#rwMoveMethod:toCategory: . #rwRemoveSelector: . #_rowanCopyMethodsAndVariablesFrom:dictionaries: .
					#_rwInstVar:constrainTo: . #_rwNewConstraint:atOffset: . #_rwNewInheritedConstraint:atOffset: .
			} }.
		{ Class . 
			{
					#byteSubclass:classVars:classInstVars:poolDictionaries:inDictionary:newVersionOf:description:options: .
					#_equivalentSubclass:superCls:name:newOpts:newFormat:newInstVars:newClassInstVars:newPools:newClassVars:inDict:constraints:isKernel: .
					#_subclass:instVarNames:format:constraints:classVars:classInstVars:poolDictionaries:inDictionary:inClassHistory:description:options: .
					#stonName . #_rwSortedConstraints . #_rwOptionsForDefinition . #_rwOptionsArray . #_rwDefinitionOfConstraints . #rwComment: .  } }.
"
		{ ExecBlock . 
			{
					#cull: . #cull:cull: . #cull:cull:cull: . #cull:cull:cull:cull:
			} }.
"
		{ GsFile class . 
			{
					#_stat:isLstat:	.		} }.
		{ GsFile . 
			{
					#<<	.		} }.
		{ GsPackagePolicy class. 
			{
					#currentOrNil .	
		} }.
		{ ByteArray . 
			{
					#byteArrayMap	.	#stonContainSubObjects . #readHexFrom: . #stonOn: . 	} }.
		{ GsNMethod . 
			{
					#rowanPackageName . #rowanProjectName	.		} }.
	} do: [:ar |
		| beh |
		beh := ar at: 1.
		(ar at: 2)
			do: [:selector | 
				beh removeSelector: selector ] ].
	System commit.	
%

	run
	#(JadeServer64bit24
		JadeServer64bit3x
		JadeServer
		JadeServer64bit32
		JadeServer64bit )
			do: [:className | UserGlobals removeKey: className ].
	System commit.
%

	run
	RowanClassServiceTest removeSelector: #createClassNamed:.
	RowanProjectServiceTest removeSelector: #existingProjectNamed:.
	System commit.
%
