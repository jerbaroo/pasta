export const attach = (attachId) => (innerHtml) => () => {
  const elem = document.getElementById(attachId)
  elem.innerHTML = innerHtml
}

var global = {}

export const getGlobal = globalId => () => {
  return global[globalId]
}

export const setGlobal = globalId => c => () => {
  global[globalId] = c
}
