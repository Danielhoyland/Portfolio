from flask import Flask, request, jsonify
from flask_mysqldb import MySQL

app = Flask(__name__)

app.config['MYSQL_HOST']="localhost"
app.config['MYSQL_USER']="root"
app.config['MYSQL_PASSWORD']=""
app.config['MYSQL_DB']="assignment4"

mysql = MySQL(app)

@app.route('/')
def index():  # put application's code here
    return 'Hello World!'

@app.route('/task1',methods=['GET'])
def getcustomerinfo():
    if request.method == 'GET':

        cur = mysql.connection.cursor()

        customer_info = cur.execute("SELECT `Name`, `Customer_id`, `Gender`, DATE_FORMAT(`Birth_date`, '%Y-%m-%d'), `City`, `Address`, `Postal_code`, `Home_Phone`, `Mobile_phone`, `Email` FROM `customer` WHERE `city`='trondheim'")
        if customer_info>0:
            customer_info = cur.fetchall()

            cur.close()
            return jsonify(customer_info),200
        else: return jsonify('Nothing Found'),404

@app.route('/task2',methods=['GET'])
def getdomaininfo():
    if request.method == 'GET':

        cur = mysql.connection.cursor()

        customer_info = cur.execute("SELECT `Name`, `Customer_id`, `Gender`, DATE_FORMAT(`Birth_date`, '%Y-%m-%d'), `City`, `Address`, `Postal_code`, `Home_Phone`, `Mobile_phone`, `Email` FROM customer WHERE email LIKE '%.com'")
        if customer_info>0:
            customer_info = cur.fetchall()

            cur.close()
            return jsonify(customer_info),200
        else: return jsonify('Nothing Found'),404
        
@app.route('/task3',methods=['GET'])
def getloaninfobetween():
    if request.method == 'GET':

        cur = mysql.connection.cursor()

        customer_info = cur.execute("SELECT * FROM loan WHERE Starting_date BETWEEN '2019-06-01' AND '2020-06-01'")
        if customer_info>0:
            customer_info = cur.fetchall()

            cur.close()
            return jsonify(customer_info),200
        else: return jsonify('Nothing Found'),404

@app.route('/task4',methods=['GET'])
def getyungestloaner():
    if request.method == 'GET':

        cur = mysql.connection.cursor()

        customer_info = cur.execute("SELECT c.Name, c.Customer_id, c.Gender, DATE_FORMAT(c.Birth_date, '%Y-%m-%d'), c.City, c.Address, c.Postal_code, c.Home_Phone, c.Mobile_phone, c.Email FROM loan l JOIN customer c ON l.customer_id = c.customer_id WHERE c.birth_date = (SELECT MAX(birth_date) FROM customer WHERE customer_id IN (SELECT customer_id FROM loan))")
        if customer_info>0:
            customer_info = cur.fetchall()

            cur.close()
            return jsonify(customer_info),200
        else: return jsonify('Nothing Found'),404
        
@app.route('/task5',methods=['GET'])
def getnotloaners():
    if request.method == 'GET':

        cur = mysql.connection.cursor()

        customer_info = cur.execute("SELECT `Name`, `Customer_id`, `Gender`, DATE_FORMAT(`Birth_date`, '%Y-%m-%d'), `City`, `Address`, `Postal_code`, `Home_Phone`, `Mobile_phone`, `Email` FROM customer WHERE customer.Customer_id NOT IN (SELECT DISTINCT loan.Customer_id FROM loan)")
        if customer_info>0:
            customer_info = cur.fetchall()

            cur.close()
            return jsonify(customer_info),200
        else: return jsonify('Nothing Found'),404

if __name__ == '__main__':
    app.run()
    
