import './App.css'
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import Login from './pages/login';
import Adminpage from './pages/admin';

function App() {

  return (
      <Router>
        <Routes>
            <Route path="/" element={<Login />} />
            <Route path="/admin" element={<Adminpage />} /> 
          </Routes>
      </Router>
  )
}

export default App
