export const attach = (attachId) => (innerHtml) => () => {
  const elem = document.getElementById(attachId)
  elem.innerHTML = innerHtml
}

var cache = null

export const getCache = () => {
  return cache
}

export const setCache = c => () => {
  cache = c
}
