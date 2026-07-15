# TDT4259 Applied Data Science Model

Machine learning model project for group assignment in course
[TDT4259 Applied Data Science](https://www.ntnu.edu/studies/courses/TDT4259#tab=omEmnet) at
[NTNU](https://www.ntnu.edu/). The project is done by group 17.

### How to run

To run the machine learning model application, you need to have Anaconda installed. If you do not
have Anaconda installed, you can download the installation
[here](https://www.anaconda.com/download).

Once you have Anaconda installed, you need to set up the Anaconda environment for the application.
Run the following command to install the Anaconda enviroment:

```
conda env create -f environment.yml -p <your-environment-directory>/modelenv
```

**Note:** On Windows, the default Anaconda environment directory is located at
`C:/Users/<your-user>/anaconda3/envs`. If you want to use the default environment directory, use
this path in `<your-environment-directory>`.

Once the environment is set up, you need to select it as the kernel source for the `model.ipynb`
file, and either run the file in your Jupyter Notebook interface or by running the following
command:

```
jupyter execute model.ipynb
```

### Troubleshooting

If you for some reason get errors when running the machine learning model application, it is
usually caused by conflicting dependencies. You can solve this by deleting your Anaconda
environment for the application and setting it up again. Run the following command to delete the
Anaconda environment:

```
conda remove -n modelenv --all
```

HOW TO RUN THE API

Go into Application folder and run: python app.py

Pip install all missing imports if you dont have

After it runs open the HTML file in like chrome

If it doesntwork change the port to on of the ones popping up when you launch the API in the html file

