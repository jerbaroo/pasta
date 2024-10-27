export const attach = (attachId) => (innerHtml) => () => {
  const elem = document.getElementById(attachId)
  elem.innerHTML = innerHtml
}
