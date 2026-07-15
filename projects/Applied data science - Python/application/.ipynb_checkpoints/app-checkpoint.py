from flask import Flask, request, jsonify
from predict_backend import predict_student
from flask_cors import CORS  # <-- add this line


app = Flask(__name__)
CORS(app)

@app.route("/predict", methods=["POST"])
def predict():
    data = request.get_json()
    label, prob = predict_student(data)
    return jsonify({
        "result": label,
        "certainty": round(prob * 100, 2) 
    })

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000)
