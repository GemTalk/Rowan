"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: applying
"
applyModification: aCypressPatchOperation
  self additions add: aCypressPatchOperation.
  self obsoletions
    at: aCypressPatchOperation modification description
    put: aCypressPatchOperation obsoletion
