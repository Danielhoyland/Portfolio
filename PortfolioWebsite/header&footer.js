// Dynamically load the header and footer
fetch('header.html')
    .then(response => response.text())
    .then(data => {
        // Insert the fetched HTML into the header
        document.getElementById('header').innerHTML = data;

        // Now modify the href values of the links with the class "portfolio-link"
        document.querySelectorAll('.portfolio-link').forEach(link => {
            link.href = link.href.replace('PortfolioWebsite/', '');
        });
    });




fetch('footer.html')
    .then(response => response.text())
    .then(data => {
        document.getElementById('footer').innerHTML = data;

        // Dynamically set the correct path based on the current URL location
        const githubIcon = document.getElementById('github-icon');

        let iconPath = 'images/Octicons-mark-github.svg';


        githubIcon.src = iconPath;

        const link = document.querySelector('.index-link');
        if (!link.href.startsWith('../')) {
            link.href = '../' + link.getAttribute('href');
        }
    });
