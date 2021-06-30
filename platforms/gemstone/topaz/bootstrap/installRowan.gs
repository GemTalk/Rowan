# set rowanCompile to true 
#
  run
  UserGlobals at: #rowanCompile put: true.
%

  run
	| session symbolList |
	session := GsCurrentSession currentSession.
	symbolList := session symbolList.
	#( #RowanKernel #RowanLoader #RowanTools)
		do: [:symbolName | 
			| newDict size |
			newDict := SymbolDictionary new
				name: symbolName;
				objectSecurityPolicy: symbolList objectSecurityPolicy;
				yourself.
			size := System myUserProfile symbolList size.
			System myUserProfile insertDictionary: newDict at: size + 1 ].
%

