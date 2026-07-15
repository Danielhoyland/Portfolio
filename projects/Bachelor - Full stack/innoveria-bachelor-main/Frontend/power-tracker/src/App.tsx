import './App.css'
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import Login from './pages/login';
import Overview from './pages/overview';
import ManageGateways from './components/manageGateway'

import { ThemeProvider } from "@/components/theme-provider"
import ManageSensors from './pages/ManageSensors';

import ManageSystem from './pages/adminSysConfig';
import UserConfig from './pages/adminUserConfig';

import ManageEnoek from './pages/enoek';
import FAQ from './pages/FAQ';

function App() {

  return (
    <ThemeProvider defaultTheme="dark" storageKey="vite-ui-theme">
      <Router>
        <Routes>
            <Route path="/" element={<Login />} /> 
            <Route path="/overview" element={<Overview />} /> 
            <Route path="/sensors" element={<ManageSensors />} />
            <Route path="/users" element={<UserConfig />} />
            <Route path="/sys&gate" element={<ManageSystem />} />
            <Route path="/enoek" element={<ManageEnoek />} />
            <Route path="/faq" element={<FAQ />} />
          </Routes>
      </Router>
    </ThemeProvider>
  )
}

export default App
