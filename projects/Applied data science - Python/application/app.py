from flask import Flask, request, jsonify
from predict_backend import predict_student
from flask_cors import CORS

app = Flask(__name__)
CORS(app)

@app.route("/predict", methods=["POST"])
def predict():
    data = request.get_json()
    label, prob, top_factors = predict_student(data)
    return jsonify({
        "result": label,
        "certainty": f"{prob}%",
        "top_dropout_factors": top_factors
    })

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000)
