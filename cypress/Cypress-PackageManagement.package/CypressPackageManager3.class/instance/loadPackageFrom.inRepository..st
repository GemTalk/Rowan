"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
loadPackageFrom: aPackage inRepository: aRepository

  ^ self 
    loadPackageFrom: aPackage 
    defaultSymbolDictionaryName: self defaultSymbolDictionaryName 
    inRepository: aRepository
