export const attach = (attachId) => (innerHtml) => () => {
  const elem = document.getElementById(attachId)
  elem.innerHTML = innerHtml
}

var pasta = {}

export const getGlobal = globalId => () => {
  return pasta[globalId]
}

export const setGlobal = globalId => c => () => {
  pasta[globalId] = c
}

export const registerFunc = globalName => f => () => {
  console.log(`Registering function ${globalName}`)
  window[globalName] = f
}
