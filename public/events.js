document.addEventListener('DOMContentLoaded', () => {
    const content = document.getElementById('content')
    const source = new EventSource(document.location.pathname)
    source.addEventListener('message', event => {
        content.innerHTML = event.data
    })
})
