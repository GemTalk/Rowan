# set rowanCompile to true 
#
  run
  UserGlobals at: #rowanCompile put: true.
%

  run
	| session symbolList symbolDictNames |
	session := GsCurrentSession currentSession.
	symbolList := session symbolList.
	symbolDictNames := symbolList names.
	#( #RowanKernel #RowanLoader #RowanTools)
		do: [:symbolName | 
			(symbolDictNames includes: symbolName)
				ifFalse: [ 
					| newDict size |
					newDict := SymbolDictionary new
						name: symbolName;
						objectSecurityPolicy: symbolList objectSecurityPolicy;
						yourself.
					size := System myUserProfile symbolList size.
					System myUserProfile insertDictionary: newDict at: size + 1 ] ].
%

