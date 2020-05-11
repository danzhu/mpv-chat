document.addEventListener('DOMContentLoaded', async () => {
    const content = document.getElementById('content')
    const source = new EventSource('/events')
    source.addEventListener('message', event => {
        content.innerHTML = event.data
    })
})
