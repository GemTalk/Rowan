"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Loading
"
loadPackageFrom: aKnownPackageInformation inRepository: aRepository
  ^ self
    loadPackageFrom: aKnownPackageInformation
    defaultSymbolDictionaryName: nil
    inRepository: aRepository
