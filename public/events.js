document.addEventListener('DOMContentLoaded', () => {
    const content = document.getElementById('content')
    const source = new EventSource(document.location.pathname)
    source.addEventListener('message', (event) => {
        const data = JSON.parse(event.data)
        document.title = data.title
        content.innerHTML = data.content

        for (const b of content.querySelectorAll('[data-post]')) {
            b.addEventListener('click', () => {
                fetch(b.getAttribute('data-post'), {
                    method: 'POST',
                    body: b.getAttribute('data-body'),
                })
            })
        }
    })
})
