// Function to create a dot at the mouse position
function createDot(x, y) {
    const dot = document.createElement('div');
    dot.classList.add('dot');

    const dotSize = 10; // Define the size of the dot (px)
    
    // Get the full scrollable area dimensions
    const pageWidth = document.documentElement.scrollWidth;
    const pageHeight = document.documentElement.scrollHeight;

    // Adjust x and y to ensure the dot is created at least dotSize away from edges
    if (x > pageWidth - dotSize) x = pageWidth - dotSize;
    if (x < dotSize) x = dotSize;
    if (y > pageHeight - dotSize) y = pageHeight - dotSize;
    if (y < dotSize) y = dotSize;

    // Set initial position of the dot based on the adjusted mouse location
    dot.style.left = `${x}px`;
    dot.style.top = `${y}px`;

    // Append the dot to the body
    document.body.appendChild(dot);

    // Randomize the direction for the dot to slide away from the cursor
    const angle = Math.random() * 2 * Math.PI; // Random angle in radians (0 to 2π)
    const velocity = 50 + Math.random() * 100; // Random velocity (50 to 150 pixels)

    // Calculate new position based on the angle and velocity
    let targetX = x + Math.cos(angle) * velocity;
    let targetY = y + Math.sin(angle) * velocity;

    // Ensure the dot stays within the scrollable page boundaries after moving
    if (targetX > pageWidth - dotSize) targetX = pageWidth - dotSize;
    if (targetX < dotSize) targetX = dotSize;
    if (targetY > pageHeight - dotSize) targetY = pageHeight - dotSize;
    if (targetY < dotSize) targetY = dotSize;

    // Move the dot to the calculated target position if it's within bounds
    setTimeout(() => {
        dot.style.transform = `translate(${targetX - x}px, ${targetY - y}px)`;
    }, 10); // Small delay to ensure dot starts at the original position

    // Remove the dot from the DOM after the animation ends
    setTimeout(() => {
        dot.remove();
    }, 1000); // Matches the duration of the fadeOut animation
}

// Listen for mousemove event on the body
document.body.addEventListener('mousemove', (event) => {
    // Get the mouse position
    const x = event.clientX;
    const y = event.clientY + window.scrollY; // Adjust y by scroll position

    // Create a small spray of dots around the cursor
    for (let i = 0; i < 3; i++) {
        // Create a random offset for the dots to spread around the cursor
        const offsetX = Math.random() * 20 - 10;
        const offsetY = Math.random() * 20 - 10;

        // Create the dot at the randomized position around the cursor
        createDot(x + offsetX, y + offsetY);
    }
});
