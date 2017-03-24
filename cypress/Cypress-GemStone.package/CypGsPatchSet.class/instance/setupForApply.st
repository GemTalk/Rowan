"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - applying
"
setupForApply
	"Copy the entire namespace from the user's transient SymbolList into a temporary SymbolDictionary in a temporary 
	SymbolList, the tempSymbols. The tempSymbols, once suitably modified, will be used as the environment in which 
	to compile methods during this apply operation."

	| symbolList |
	tempSymbols := SymbolDictionary new.
	symbolList := GsCurrentSession currentSession symbolList.
	"If there are any duplicated symbols, we only want the one that will be found first one, 
	so we copy in reverse order of search order, thereby overwriting later definitions with earlier ones."
	symbolList reverseDo: 
			[:dict |
			dict keysAndValuesDo: [:aKey :aValue | tempSymbols at: aKey put: aValue]]