// Dynamically load the header and footer
fetch("header.html")
  .then((response) => response.text())
  .then((data) => {
    // Insert the fetched HTML into the header
    document.getElementById("header").innerHTML = data;

    document.querySelectorAll(".portfolio-link").forEach((link) => {
      link.href = link.href.replace("PortfolioWebsite/", "");
    });
    // Get all the nav links
    const navLinks = document.querySelectorAll(".portfolio-link");

    // Loop through all links and check if the href matches the current page's URL
    navLinks.forEach((link) => {
      if (link.href === window.location.href) {
        link.classList.add("active"); // Add 'active' class to the current page link
      }
    });
  });

fetch("footer.html")
  .then((response) => response.text())
  .then((data) => {
    document.getElementById("footer").innerHTML = data;

    // Dynamically set the correct path based on the current URL location
    const githubIcon = document.getElementById("github-icon");

    let iconPath = "images/Octicons-mark-github.svg";

    githubIcon.src = iconPath;

    const link = document.querySelector(".index-link");
    if (!link.href.startsWith("../")) {
      link.href = "../" + link.getAttribute("href");
    }
  });
