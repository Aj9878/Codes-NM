<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Ajit's Profile</title>
    <link href="https://fonts.googleapis.com/css2?family=Roboto:wght@400;500&display=swap" rel="stylesheet">
    <style>
        /* General body styling */
        body {
            font-family: 'Roboto', sans-serif;
            margin: 0;
            padding: 0;
            line-height: 1.6;
            background-color: #f4f4f4;
            color: #333;
            overflow-x: hidden;
        }

        /* Navigation bar styling */
        header {
            background: linear-gradient(90deg, #007bff, #0056b3);
            color: #fff;
            padding: 15px 0;
            position: fixed;
            width: 100%;
            top: 0;
            z-index: 1000;
            box-shadow: 0 2px 15px rgba(0, 0, 0, 0.3);
            transition: background-color 0.3s, padding 0.3s;
        }

        nav ul {
            list-style: none;
            display: flex;
            justify-content: center;
            margin: 0;
            padding: 0;
        }

        nav ul li {
            margin: 0 20px;
        }

        nav ul li a {
            color: #fff;
            text-decoration: none;
            padding: 10px 20px;
            transition: background-color 0.3s, color 0.3s, transform 0.3s;
            border-radius: 5px;
            font-weight: 500;
        }

        nav ul li a:hover {
            background-color: #17a2b8;
            transform: scale(1.05);
            box-shadow: 0 4px 10px rgba(0, 0, 0, 0.2);
        }

        nav ul li a.active {
            background-color: #343a40;
            color: #fff;
        }

        /* Header shrink effect on scroll */
        header.shrink {
            background-color: #0056b3;
            padding: 10px 0;
        }

        /* Section styling */
        section {
            padding: 120px 20px; /* Adjusting for fixed header */
            min-height: 100vh;
            background-color: #fff;
            border-bottom: 1px solid #ddd;
            text-align: center;
            opacity: 0;
            transform: translateY(20px);
            transition: opacity 0.6s ease-out, transform 0.6s ease-out;
        }

        section:nth-of-type(odd) {
            background-color: #f8f9fa;
        }

        section h1, section h2 {
            margin-top: 0;
            font-size: 2.5em;
            color: #343a40;
            text-transform: uppercase;
            letter-spacing: 2px;
            animation: fadeInDown 1s ease-out;
        }

        section p, section ul {
            max-width: 800px;
            margin: 20px auto;
            text-align: left;
            font-size: 1.1em;
            line-height: 1.8;
            color: #666;
            animation: fadeInUp 1s ease-out;
        }

        /* Profile container styling */
        .profile-container {
            display: flex;
            align-items: center;
            flex-direction: column;
        }

        .profile-container img {
            border-radius: 50%;
            width: 150px;
            height: 150px;
            object-fit: cover;
            box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2);
            margin-bottom: 20px;
            transition: transform 0.3s;
        }

        .profile-container img:hover {
            transform: scale(1.1);
            box-shadow: 0 12px 24px rgba(0, 0, 0, 0.3);
        }

        /* Work gallery styling */
        .work-gallery {
            display: flex;
            justify-content: space-around;
            flex-wrap: wrap;
            gap: 20px;
            padding-top: 20px;
        }

        .work-item {
            flex: 1 1 calc(33% - 40px);
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
            border-radius: 10px;
            overflow: hidden;
            background-color: #fff;
            transition: transform 0.3s, box-shadow 0.3s;
            position: relative;
            display: flex;
            flex-direction: column;
            justify-content: space-between;
        }

        .work-item:hover {
            transform: translateY(-5px);
            box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2);
        }

        .work-item img {
            width: 100%;
            height: auto;
            display: block;
            transition: opacity 0.3s ease-in-out;
        }

        .work-item img:hover {
            opacity: 0.8;
        }

        .work-item p {
            padding: 15px;
            margin: 0;
            background-color: #007bff;
            color: #fff;
            text-align: center;
            font-weight: bold;
            font-size: 1.2em;
        }

        .button-group {
            display: flex; /* Use Flexbox for horizontal alignment */
            gap: 10px; /* Space between buttons */
            margin-top: 10px;
            padding: 10px;
        }

        .button {
            display: inline-block;
            padding: 10px 20px;
            font-size: 16px;
            text-align: center;
            text-decoration: none;
            color: #fff;
            background-color: #007BFF; /* Adjust color as needed */
            border-radius: 5px;
            transition: background-color 0.3s ease;
        }

        .button:hover {
            background-color: #0056b3; /* Adjust hover color as needed */
        }

        /* Scroll-to-top button styling */
        #scrollToTopBtn {
            position: fixed;
            bottom: 30px;
            right: 30px;
            background-color: #007bff;
            color: white;
            border: none;
            padding: 15px 20px;
            border-radius: 50%;
            cursor: pointer;
            display: none;
            box-shadow: 0 4px 10px rgba(0, 0, 0, 0.3);
            font-size: 24px;
            transition: background-color 0.3s, box-shadow 0.3s, transform 0.3s;
        }

        #scrollToTopBtn:hover {
            background-color: #0056b3;
            box-shadow: 0 6px 15px rgba(0, 0, 0, 0.4);
            transform: scale(1.1);
        }

        /* Footer styling */
        footer {
            text-align: center;
            padding: 20px 0;
            background-color: #343a40;
            color: #fff;
            border-top: 4px solid #007bff;
        }

        footer p {
            margin: 0;
            font-size: 1em;
            color: #ccc;
        }

        /* Additional Styling for a Consistent Look */
        a {
            text-decoration: none;
            color: inherit;
        }

        a:hover {
            color: #007bff;
            transition: color 0.3s;
        }

        /* Animation on Scroll */
        .reveal {
            opacity: 1;
            transform: translateY(0);
        }

        @keyframes fadeInDown {
            from {
                opacity: 0;
                transform: translateY(-20px);
            }
            to {
                opacity: 1;
                transform: translateY(0);
            }
        }

        @keyframes fadeInUp {
            from {
                opacity: 0;
                transform: translateY(20px);
            }
            to {
                opacity: 1;
                transform: translateY(0);
            }
        }

        /* Responsive Design */
        @media (max-width: 768px) {
            nav ul li {
                margin: 0 10px;
            }

            .work-item {
                flex: 1 1 100%;
            }

            section h1, section h2 {
                font-size: 2em;
            }
        }

        @media (max-width: 576px) {
            section {
                padding: 100px 10px;
            }

            .work-item {
                flex: 1 1 100%;
            }

            .profile-container img {
                width: 120px;
                height: 120px;
            }
        }
    </style>
</head>
<body>

    <!-- Header with Navigation -->
    <header>
        <nav>
            <ul>
                <li><a href="#home" class="active">Home</a></li>
                <li><a href="#resume">Resume</a></li>
                <li><a href="#work">My Work</a></li>
                <li><a href="#interests">Interests</a></li>
                <li><a href="#contact">Contact</a></li>
            </ul>
        </nav>
    </header>

    <!-- Merged Home & About Me Section -->
    <section id="home">
        <div class="profile-container reveal">
            <img src="images/DSC02522.jpg" alt="Profile Picture of Ajit">
            <h1>Welcome to My Profile</h1>
            <p>Hello! My name is Ajit. I recently completed my Master's degree in Physics, with a strong passion for numerical methods and algorithm development. My master's dissertation focused on solving the one-dimensional linear advection equation using finite difference and finite volume methods. I have also worked through and solved algorithms presented in "Numerical Analysis" by Richard L. Burden and J. Douglas Faires.</p>
            <p>I am eager to apply my systematic thinking, algorithm development, and programming skills in future research endeavors in different aspects of physics.</p>
        </div>
    </section>

    <!-- Resume Section -->
    <section id="resume">
        <h2 class="reveal">Resume</h2>
        <div class="resume-section reveal">
            <h3>Education</h3>
            <ul>
                <li><strong>Sikkim University</strong> - Master of Science in Physics (2024)</li>
                <li><strong>Motilal Nehru College, Delhi University</strong> - Bachelor of Science in Physical Sciences (2021)</li>
            </ul>
        </div>

        <div class="resume-section reveal">
            <h3>Experience</h3>
            <ul>
                <li><strong>Research Intern</strong> - University Name (Year - Year)
                    <p>Brief description of your role and achievements.</p>
                </li>
                <li><strong>Teaching Assistant</strong> - University Name (Year - Year)
                    <p>Brief description of your role and achievements.</p>
                </li>
            </ul>
        </div>

        <div class="resume-section reveal">
            <h3>Skills</h3>
            <ul>
                <li>Numerical Methods</li>
                <li>Algorithm Development</li>
                <li>Programming (e.g., Python, MATLAB)</li>
                <li>Systematic Thinking</li>
                <li>Quick in Learning New Technology</li>
            </ul>
        </div>
    </section>

<!-- My Work Section -->
<section id="work">
    <h2 class="reveal">My Work</h2>
    <div class="work-gallery">
        <div class="work-item reveal">
            <img src="images/work1.jpg" alt="Work 1">
            <p>Master's Dissertation: Solving the One-Dimensional Linear Advection Equation</p>
            <div class="button-group">
                <a href="https://drive.google.com/file/d/1L00zMNapZzlwp4r0KbP0T8AATxBHawy4/view" target="_blank" class="button">View Report</a>
                <a href="https://docs.google.com/presentation/d/121fSn9rbGo_WkUFbtdCBilm50WlDjyNi/edit#slide=id.p16" target="_blank" class="button">View Slides</a>
                <a href="https://cus.ac.in/index.php/en/schools-e/physical-sciences/physics-dept/dr-rupak-mukherjee" target="_blank" class="button">Guide</a>
            </div>
        </div>
        <div class="work-item reveal">
            <img src="images/work2.jpg" alt="Work 2">
            <p>Implementation of Algorithms (Based on "Numerical Analysis" by Richard L. Burden and J. Douglas Faires)</p>
            <div class="button-group">
                <a href="https://github.com/Aj9878/Codes-NM/tree/main/Burden%20and%20Faires" target="_blank" class="button">View on GitHub</a>
            </div>
        </div>
        <div class="work-item reveal">
            <img src="images/work3.jpg" alt="Work 3">
            <p>Course Projects on Numerical Methods</p>
        </div>
    </div>
</section>

    <!-- Interests Section -->
    <section id="interests">
        <h2 class="reveal">Interests</h2>
        <ul class="reveal">
            <li>Numerical Methods and Algorithm Development</li>
            <li>Programming and Computational Physics</li>
            <li>Quantum Mechanics and Cosmology</li>
            <li>Continued Learning and Research in Physics</li>
        </ul>
    </section>

    <!-- Contact Section -->
    <section id="contact">
        <h2 class="reveal">Contact</h2>
        <p class="reveal">Feel free to reach out to me via the following methods:</p>
        <ul class="reveal">
            <li>Email: <a href="mailto:ajitnepal15@gmail.com">ajitnepal15@gmail.com</a></li>
            <li>LinkedIn: <a href="#" target="_blank">LinkedIn Profile</a></li>
            <li>GitHub: <a href="#" target="_blank">GitHub Profile</a></li>
        </ul>
    </section>

    <!-- Footer -->
    <footer>
        <p>&copy; 2024 Ajit. All rights reserved.</p>
    </footer>

    <!-- Scroll-to-Top Button -->
    <button id="scrollToTopBtn" title="Go to top">↑</button>

    <script>
        // Smooth scrolling for navigation links
        document.querySelectorAll('nav ul li a').forEach(anchor => {
            anchor.addEventListener('click', function (e) {
                e.preventDefault();
                const targetId = this.getAttribute('href').substring(1);
                const targetSection = document.getElementById(targetId);
                window.scrollTo({
                    top: targetSection.offsetTop - 50, // Adjusting for header height
                    behavior: 'smooth'
                });

                // Update active link
                document.querySelectorAll('nav ul li a').forEach(link => {
                    link.classList.remove('active');
                });
                this.classList.add('active');
            });
        });

        // Highlight the active navigation link based on scroll position
        window.addEventListener('scroll', function () {
            let scrollPosition = window.scrollY || document.documentElement.scrollTop;
            document.querySelectorAll('section').forEach(section => {
                if (scrollPosition >= section.offsetTop - 60 && scrollPosition < section.offsetTop + section.offsetHeight) {
                    document.querySelectorAll('nav ul li a').forEach(anchor => {
                        anchor.classList.remove('active');
                    });
                    const activeLink = document.querySelector(`nav ul li a[href="#${section.id}"]`);
                    activeLink.classList.add('active');
                }

                // Reveal sections with animation
                if (section.getBoundingClientRect().top < window.innerHeight - 100) {
                    section.classList.add('reveal');
                }
            });

            // Shrink header on scroll
            const header = document.querySelector('header');
            if (scrollPosition > 100) {
                header.classList.add('shrink');
            } else {
                header.classList.remove('shrink');
            }
        });

        // Scroll-to-top button functionality
        const scrollToTopBtn = document.getElementById('scrollToTopBtn');

        window.addEventListener('scroll', function () {
            if (window.scrollY > 300) {
                scrollToTopBtn.style.display = 'block';
            } else {
                scrollToTopBtn.style.display = 'none';
            }
        });

        scrollToTopBtn.addEventListener('click', function () {
            window.scrollTo({
                top: 0,
                behavior: 'smooth'
            });
        });

        // Reveal sections on load
        document.addEventListener('DOMContentLoaded', function () {
            document.querySelectorAll('section').forEach(section => {
                if (section.getBoundingClientRect().top < window.innerHeight - 100) {
                    section.classList.add('reveal');
                }
            });
        });
    </script>
</body>
</html>
