var __pasta = {}

export const getGlobal = appName => () => {
  return __pasta[appName]
}

export const innerHTML = attachId => innerHtml => () => {
  const elem = document.getElementById(attachId)
  elem.innerHTML = innerHtml
  console.log("Updated inner HTML")
}

export const registerFunc = globalFuncName => f => () => {
  window[globalFuncName] = f
}

export const setGlobal = appName => c => () => {
  __pasta[appName] = c
}

// SSR /////////////////////////////////////////////////////////////////////////

const insertJS = (html, s) => {
  const toReplace = "PASTAJS"
  return html.replace(toReplace,`
      ${s}
    ${toReplace}
  `)
}

export const getSSRState = appName => nothing => just => () => {
  const ssrState = window[ssrVarName(appName)]
  if (ssrState === undefined) {
    console.log(`PASTA (${appName}): No SSR state found`)
    return nothing
  } {
    console.log(`PASTA (${appName}): SSR state found, deleting and returning`)
    delete window[ssrVarName(appName)]
    return just(ssrState)
  }
}

export const innerHTMLSSR = raw => html => {
  console.log(`PASTA SSR: Attaching HTML body via innerHTML`)
  return html.replace("PASTABODY", raw)
}

export const postSSRCleanup = html => {
  console.log(`PASTA SSR: Cleaning up`)
  return html.replace("PASTAJS", "").replace("PASTABODY", "")
}

export const registerFuncSSR = globalFuncName => html => {
  console.log(`PASTA SSR: registering function ${globalFuncName}`)
  return insertJS(html,
    `${globalFuncName} = () => {
      console.log("I have not been defined :(")
    }`
  )
}

export const setSSRState = appName => state => html => {
  console.log(`PASTA SSR: Setting SSR state = ${state}`)
  return insertJS(html,`window.${ssrVarName(appName)} = ${state}`)
}

const ssrVarName = appName => `__pasta_${appName}`
