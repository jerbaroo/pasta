export const attach = attachId => innerHtml => () => {
  const elem = document.getElementById(attachId)
  if (elem.classList.contains("pasta-ssr")) {
    elem.classList.remove("pasta-ssr")
    console.log("Ignoring first render due to SSR")
  } else {
    elem.innerHTML = innerHtml
    console.log("Updated inner HTML")
  }
}

export const attachSSR = raw => html => {
  console.log(`Attaching HTML body ${raw}`)
  return html.replace("PASTABODY", raw)
}

var __pasta = {}

export const getGlobal = globalId => () => {
  return __pasta[globalId]
}

export const setGlobal = globalId => c => () => {
  __pasta[globalId] = c
}

export const postSSRCleanup = html =>
  html.replace("PASTAJS", "").replace("PASTABODY", "")

export const registerFunc = globalName => f => () => {
  console.log(`Registering function ${globalName}`)
  window[globalName] = f
}

export const registerFuncSSR = globalName => html => {
  console.log(`Registering function ${globalName}`)
  const toReplace = "PASTAJS"
  return html.replace(toReplace,`
      ${globalName} = () => {
        console.log("I have not been defined :(")
      }
    ${toReplace}`
  )
}
