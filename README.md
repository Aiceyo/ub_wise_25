# 📊 Data Storytelling: Cross-Cultural Attitudes Towards Epilepsy

**Live Demo:** https://aiceyo.github.io/ub_wise_25/sapefinal/index.html

## 📌 Project Overview
This project is an **interactive scrollytelling prototype** designed to make complex psychological and sociological research accessible to a broader audience. It visualizes the findings of the cross-cultural study *"A cross-cultural comparative study of attitudes towards people with epilepsy in Japan and Germany"* (Kerkhoff et al., 2025). 

Developed as part of the "Data Storytelling" seminar at Bielefeld University, this prototype translates static academic data into an engaging, user-driven digital narrative.

## 🧠 My Role & AI-Assisted Workflow
Instead of focusing solely on manual syntax, I approached this project from the perspective of a **UX Designer & Product Owner**. My primary focus was on information architecture, didactic translation, and user flow.

To bring this vision to life efficiently, I utilized **AI-assisted rapid prototyping (LLMs)** to generate the frontend codebase. My workflow involved:
* **Content Strategy:** Curating and simplifying raw academic data for web consumption.
* **UX/UI Direction:** Defining the interaction models (scrollytelling pacing, progressive disclosure) and layout grids.
* **AI Prompting & Iteration:** Directing an AI assistant to write the underlying HTML/CSS/Vanilla JS, reviewing the output, and continuously refining the code to meet exact UX specifications.

This approach allowed me to bypass boilerplate coding and dedicate my resources entirely to optimizing the user experience and narrative structure.

## 🎯 UX & Design Goals
* **Scrollytelling & Cognitive Pacing:** Custom JavaScript intercepts standard scrolling to build the narrative element-by-element, preventing cognitive overload and guiding the user's attention deliberately.
* **Progressive Disclosure:** Complex psychological terms and contextual study information are hidden behind interactive tooltips and info-boxes. Users can explore details on demand without cluttering the primary interface.
* **Responsive Architecture:** The interface seamlessly transitions from a complex two-column CSS grid on desktop to a readable, highly accessible single-column flow on mobile devices.

## 🛠️ Tech Stack & Methods
* **Core Technologies:** HTML5, CSS3, Vanilla JavaScript (No heavy frameworks used, ensuring high performance).
* **Development Method:** AI-Assisted Rapid Prototyping (Prompt Engineering, Code Review, Iterative Refinement).
* **Features:** Custom scroll-event listeners, dynamic DOM manipulation for chapter navigation, semantic HTML, CSS Grid & Flexbox.

## 🚀 How to Run Locally
Since this is a vanilla web project, no build tools or package managers are required.
1. Clone the repository: `git clone https://github.com/Aiceyo/ub_wise_25.git`
2. Navigate to the `sapefinal/` folder and launch `index.html` in any modern web browser.
