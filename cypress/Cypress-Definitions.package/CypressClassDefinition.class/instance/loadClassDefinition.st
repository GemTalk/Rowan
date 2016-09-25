"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
loadClassDefinition
  "Create a new version of the defined class. If the class already exists,
	 copy the behaviors and state from the old version."

  ^ self loadClassDefinition: self defaultSymbolDictionaryName