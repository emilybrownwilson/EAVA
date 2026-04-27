# Welcome to EAVA contributing guide!

Thank you for your time and expertise. 

# Setup

Install dependencies and check the package:

install.packages("devtools")
devtools::install()
devtools::check()

# Workflow

Fork the repository
Create a new branch 
Make your changes
Submit a pull request

# Code Style

Follow consistent R style (e.g., snake_case)
Ensure code passes devtools::check() without warnings

# Documentation
Use roxygen2 for all functions and datasets
Update documentation with:
devtools::document()

# Testing
Run tests with:
devtools::test()
Add tests for new functionality when appropriate

# Issues
Use GitHub issues for bugs or feature requests
Reference issues in pull requests when applicable

