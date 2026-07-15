import React from "react";
//import QrReader from "react-qr-scanner";

class QRScan extends React.Component {
  state = {
    delay: 100,
    result: "No result"
  };

  handleScan = (data: any) => {
    if (data!=null ) {
      this.setState({
        result: data
      });
    } 
  };

  handleError = (err: any) => {
    console.error(err);
  };
  /*
  render() {
    return (
        <div>
          <QrReader
            delay={this.state.delay}
            onError={this.handleError}
            onScan={this.handleScan}
          />
            <p>Scanned QR Code: {this.state.result}</p>
        </div>
    );
  }
  */
}

export default QRScan;
