// Dynamically load the header and footer
fetch('header.html')
    .then(response => response.text())
    .then(data => document.getElementById('header').innerHTML = data);


fetch('footer.html')
    .then(response => response.text())
    .then(data => {
        document.getElementById('footer').innerHTML = data;

        // Dynamically set the correct path based on the current URL location
        const githubIcon = document.getElementById('github-icon');
        
        let iconPath = 'PortfolioWebsite/images/Octicons-mark-github.svg';
        

        githubIcon.src = iconPath;
    });
