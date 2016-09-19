"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: unloading
"
unloadSnapshot: aSnapshot
  |  patch |
  patch := CypressSnapshot empty patchRelativeToBase: aSnapshot.
  patch applyTo: self
