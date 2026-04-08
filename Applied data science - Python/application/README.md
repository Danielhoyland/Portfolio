
# Student Graduation Prediction

## How to Start

1. **Install dependencies**  
   Make sure you have Python installed (preferably 3.9+). Then, install the required packages:

   ```bash
   pip install -r requirements.txt


2. **Run the backend server**
   In the project directory, start the Flask server:

   ```bash
   python app.py
   ```

   You should see an output like:

   ```
   * Running on http://127.0.0.1:5000
   ```

3. **Open the frontend**
   Open the `interface.html` file in a web browser. Fill out the form and click **Predict**.

---

## Troubleshooting

* **500 Internal Server Error / 404 Not Found**

  * Make sure the Flask server is running.
  * Check that the IP and port in the HTML fetch call match the server (e.g., `http://127.0.0.1:5000/predict`).
  * Ensure all required model files and `data.csv` are in the correct directory.

* **CORS errors in the browser**

  * Make sure `flask-cors` is installed.
  * Ensure `CORS(app)` is included in `app.py`.

* **Preprocessor/Model errors**

  * The model relies on `data.csv` for fitting the preprocessor. Ensure it exists and has the correct columns.


