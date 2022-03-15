exports.fetchTextImpl = (location) => () => {
    return fetch(location).then(
        (response) => {
            return response.text();
        }
    );
}
