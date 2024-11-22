export const attach = (attachId) => (innerHtml) => () => {
  const elem = document.getElementById(attachId)
  elem.innerHTML = innerHtml
}

var cache = {}

export const getCache = cacheId => () => {
  return cache[cacheId]
}

export const setCache = cacheId => c => () => {
  cache[cacheId] = c
}
